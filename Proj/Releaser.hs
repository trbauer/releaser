module Proj.Releaser where

import Proj.Version

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Char
import Data.IORef
import Data.List
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.Process
import Text.Printf


data ProjectConfig =
  ProjectConfig {
    pcRepos :: ![Repo]
  , pcCabalFile :: FilePath -- project.cabal
  , pcVersionFile :: FilePath -- Version.hs (we look for (vERSION, vERSION_DATE))
  , pcExePaths :: [FilePath] -- e.g. dist/foo.exe (the target built)
  , pcExeTargetDirs :: [FilePath] -- z:\users\trbauer ...
  , pcProjectArchive :: [FilePath] -- ...
  , pcStrip :: !Bool
  } deriving (Show,Read)

data Repo = Mercurial | Git deriving (Show,Read,Eq)
repoExe :: Repo -> String
repoExe Mercurial = "hg"
repoExe Git       = "git"
repoDir r = "." ++ repoExe r


vERSION_FILE = "Version.hs"
vERSION_FIELD = "(vERSION,vERSION_DATE)"
dEFAULT_ARCHIVE_DIR = "archive"

fmtProjectConfig :: ProjectConfig -> String
fmtProjectConfig pc =
  "ProjectConfig {\n" ++
  fmtField0 "pcRepos" pcRepos ++
  fmtField "pcCabalFile" pcCabalFile ++
  fmtField "pcVersionFile" pcVersionFile ++
--  fmtField "pcVersionField" pcVersionField ++
  fmtField "pcExesPath" pcExePaths ++
  fmtField "pcExeTargetDirs" pcExeTargetDirs ++
  fmtField "pcProjectArchive" pcProjectArchive ++
  fmtField "pcStrip" pcStrip ++
  "}\n"
  where fmtField0 nm f = "    " ++ nm ++ " = " ++ show (f pc) ++ "\n"
        fmtField  nm f = "  , " ++ nm ++ " = " ++ show (f pc) ++ "\n"

data ROpts = ROpts {
    roConfig   :: !String
  , roClobber :: !Bool    -- -c
  , roInit     :: !Bool   -- 'init'
  , roDryRun :: !Bool   -- -t (or --dry-run)
  , roVerbose :: !Bool    -- -v
  } deriving Show
dftROpts = ROpts {
    roConfig   = ""
  , roClobber  = False
  , roInit     = False
  , roDryRun   = False
  , roVerbose  = False
  }

parseROpts :: ROpts -> [String] -> IO ROpts
parseROpts ro [] = do
  cfg <-
    if roInit ro || not (null (roConfig ro)) then return (roConfig ro)
      else do
        fs <- getDirectoryContents "."
        case filter (".cfg"`isSuffixOf`) fs of
          [c] -> return c
          _ -> fatal "config file must be specified (except in 'init' mode)"
  return ro {roConfig = cfg}
parseROpts ro (a:as)
  | a == "init" = parseROpts (ro {roInit = True}) as
  | a == "init" && roInit ro = badArg a "init already specified"
  | a`elem`["-c","--clobber"] = parseROpts (ro {roClobber = True}) as
  | a`elem`["-t","--dry-run"] = parseROpts (ro {roDryRun = True}) as
  | a`elem`["-v","--verbose"] = parseROpts (ro {roVerbose = True}) as
  | a`elem`["-V","--version"] = putStrLn (vERSION ++ "(" ++ vERSION_DATE ++ ")") >> exitSuccess
  | take 1 a == "-" = badArg a "unrecognized option"
  | not (null (roConfig ro)) = badArg a ("config file already specified as " ++ roConfig ro)
  | otherwise = parseROpts (ro {roConfig = a}) as

usage =
  "usage: runghc releaser OPTS [init] [CFG_FILE])\n" ++
  "where\n" ++
  "   -c|--clobber   clobbers archive (overwrite same version)\n" ++
  "   -t|--dry-run   dry run only; just (t)est things\n" ++
  "   -v|--verbose   verbose output\n" ++
  "\n" ++
  "  'init'           will initialize CFG file\n" ++
  "  CFG_FILE is      the (.cfg) file\n" ++
  ""

badArg :: String -> String -> IO a
badArg a msg = do
  fatal $ a ++ ": " ++ msg ++ "\n" ++ usage

warningLn :: String -> IO ()
warningLn = hPutStrLn stderr

fatal :: String -> IO a
fatal msg = do
  hPutStrLn stderr msg
  exitFailure

messageLn = putStrLn


run :: [String] -> IO ()
run as = do
  ro <- parseROpts dftROpts as
  z <- doesFileExist (roConfig ro)
  if roInit ro then do
    when z $ fatal ("init: " ++ roConfig ro ++ " already exists, delete it first")
    initRelease ro
    else do
      fstr <- readFile (roConfig ro)
      pc <-
        case reads fstr of
          [(pc,sfx)] -> return pc
          [] -> fatal $ "parse error reading config file:\n" ++ fstr
          _ -> fatal $ "parse error reading config file (ambiguous):\n" ++ fstr
      z <- runRelease ro pc
      if z then exitSuccess else exitFailure

initRelease :: ROpts -> IO ()
initRelease ro = do
  let chooseConfig field val why =
          messageLn (printf "%-16s ==> %-32s" field (show val) ++ why_str) >> return val
        where why_str = if null why then "" else " (" ++ why ++ ")"
      verboseLn = if roVerbose ro then putStrLn else const (return ())
  today <- getDateString

  -- get list of .cabal files in current directory
  fs <- getDirectoryContents "."
  let cabal_files = filter (".cabal"`isSuffixOf`) fs
      has_cabal = length cabal_files == 1
      no_clobber = not (roClobber ro)

  -- choose the .cfg name if it isn't given
  -- ensure that we aren't clobbering it (unless -c)
  cfg_file <-
    if not (null (roConfig ro)) then
      chooseConfig ".cfg file" (roConfig ro) "specified on command line"
      else if not (null cabal_files) then
        chooseConfig ".cfg file" (dropExtension (head cabal_files) ++ ".cfg") "inferred from .cabal file"
        else
          fatal ".cfg file must be specified (cannot be inferred from .cabal"
  z <- doesFileExist cfg_file
  when (no_clobber && z) $
    fatal $ "config file " ++ show cfg_file ++ " already exists"

  -- find the .cabal file
  cabal_file <-
    case cabal_files of
      []  -> chooseConfig "pcCabalFile" "" "no .cabal found"
      [f] -> chooseConfig "pcCabalFile" f  "using existing .cabal file"
      _   -> chooseConfig "pcCabalFile" "" "ambiguous / multiple .cabal found"

  -- find the repos (e.g. .hg or .git)
  has_hg <- doesDirectoryExist (repoDir Mercurial)
  let repo_hg = if has_hg then [Mercurial] else []
  has_git <- doesDirectoryExist (repoDir Git)
  let repo_git = if has_git then [Git] else []
  repos <-
    if (not (null repo_hg) && not (null repo_git))
      then chooseConfig "pcRepos" (repo_hg ++ repo_git) "found both .hg and .git"
      else if not (null repo_hg) then chooseConfig "pcRepos" (repo_hg ++ repo_git) "found .hg"
      else if not (null repo_git) then chooseConfig "pcRepos" (repo_hg ++ repo_git) "found .git"
      else chooseConfig "pcRepos" [] "no repos found"

  -- find Version.hs
  version_file <-
    if vERSION_FILE `elem` fs then do
      f <- chooseConfig "pcVersionFile" vERSION_FILE ("found " ++ vERSION_FILE)
      val <- findProjectVersion vERSION_FILE
      case val of
        Left err -> warningLn $ "could not find " ++ vERSION_FIELD ++ " in Version.hs"
        Right v  -> verboseLn $ "  initial version value is: " ++ show v
      return f
      else do
        vfile <- chooseConfig "pcVersionFile" vERSION_FILE "generating"
        cabal_ver <- if has_cabal then return [0,1,0,0] else findCabalVersion cabal_file
        let vstr = "module Version where\n\n" ++
                   "-- auto-generated\n" ++
                   vERSION_FIELD ++ " = (\"" ++ verToStr cabal_ver ++ "\",\"" ++ today ++ "\") -- initial revision\n"
        writeFile vERSION_FILE vstr
        length vstr `seq` return ()
        return vfile

  -- find the initial .exe name
  let mkExeTarget e = "dist" </> "build" </> e </> (e ++ ".exe")
  exe_name <-
    if has_cabal then do
      exes <- findCabalExecutables cabal_file
      case exes of
        (e:_) -> chooseConfig "pcExePath" (mkExeTarget e) "took first 'executable' line in .cabal file"
        _ -> chooseConfig "pcExePath" (mkExeTarget (dropExtension cabal_file)) "based name off .cabal file's name"
      else do -- search in ./dist/build/.. for exes
        chooseConfig "pcExePath" (mkExeTarget "the") "cannot find executable name (from .cabal executable or name)"

  -- look for default archive directory, created if needs be
  z <- doesDirectoryExist dEFAULT_ARCHIVE_DIR
  archv <-
    if z then chooseConfig "pcProjectArchive" [dEFAULT_ARCHIVE_DIR] "found existing"
     else do
      a <- chooseConfig "pcProjectArchive" [dEFAULT_ARCHIVE_DIR] "creating"
      when (not (roDryRun ro)) $
        createDirectory dEFAULT_ARCHIVE_DIR
      return a

  -- write the result out
  let output = fmtProjectConfig $ ProjectConfig repos cabal_file version_file [exe_name] [] archv True
  if roDryRun ro then putStr $ "TEST ONLY: we'd write to " ++ cfg_file ++ " the following:\n" ++ output
    else writeFile cfg_file output



runRelease :: ROpts -> ProjectConfig -> IO Bool
runRelease ro pc = do
  ior_success <- newIORef True
  today <- getDateString
  let has_cabal = not (null (pcCabalFile pc))
      not_dry_run = not (roDryRun ro)
      whenNotDryRun = when not_dry_run
      verboseLn = if roVerbose ro then putStrLn else const (return ())
      problem msg
        | roClobber ro || roDryRun ro = warningLn ("WARNING: " ++ msg)
        | otherwise = fatal ("ERROR: " ++ msg ++ " (-c to clobber)")
      problemIf c msg = when c $ problem msg

  -- fetch the various version, check them for consistency
  cabal_ver <- if has_cabal then findCabalVersion (pcCabalFile pc) else return []
  ehs_ver <- findProjectVersion (pcVersionFile pc)
  (hs_ver,hs_ver_date) <-
    case ehs_ver of
      Left err -> fatal $ "could not find project version: " ++ err
      Right r -> return r
  problemIf (not (null (cabal_ver)) && cabal_ver /= hs_ver) $
    "mismatch between cabal version and project version; " ++
    "did you bump both? (cabal "++ show cabal_ver ++ " vs version file " ++ show hs_ver ++ " )"
  problemIf (hs_ver_date /= today) "release date in version file is not today"

  -- ensure repos are up to date
  forM_ (pcRepos pc) $ \r -> do
    let rcs_exe = repoExe r
    (ec,out,err) <- exec rcs_exe ["diff"]
    problemIf (not (null out)) $ "outstanding changes in " ++ rcs_exe ++ " repository"
    problemIf (ec /= 0) $ "(" ++ rcs_exe ++ " diff) exited " ++ show ec

  -- ensure the target directories exist
  forM_ (pcExePaths pc) $ \exe -> do
    let exe = dropExtension (takeFileName exe) -- "foo/bar/baz.exe" -> "baz"
    forM_ (pcExeTargetDirs pc) $ \dir -> do
      verboseLn $ "ensuring exe target dir exists: " ++ show dir
      z <- doesDirectoryExist dir
      problemIf (not z) $ "exe target installation dir " ++ dir ++ " does not exist"

    -- ensure the version is bumped (by using an archive)
    forM_ (pcProjectArchive pc) $ \dir -> do
      verboseLn $ "checking archive target dir for version >= " ++ hs_ver_date
      z <- doesDirectoryExist dir
      if not z
        then problem $ "archive dir " ++ dir ++ " doesn't exist"
        else do
          fs <- getDirectoryContents dir
          case maxVersionedExe exe fs of
            [] -> return () -- not present
            v
              | v >= hs_ver -> problem "archive has version >= this version already"
              | otherwise -> return ()

  problemIf (today /= hs_ver_date) $ "mismatch of version date " ++ hs_ver_date ++ " vs today"

  -- build the file (ensure it exists afterwards)
  forM_ (pcExePaths pc) $ \exe -> do
    z <- doesFileExist exe
    when (z && not_dry_run) $
      removeFile exe
  when has_cabal $ do
    putStrLn "[cabal build]"
    when (not (roDryRun ro)) $ do
      out <- exec0 "cabal" ["build"]
      verboseLn (labelLines ("[cabal build] ") out)

  forM_ (pcExePaths pc) $ \exe -> do
    z <- doesFileExist exe
    when (not z && not_dry_run) $
      problem $ "no executable found at " ++ exe ++ " (after build)"
    when (pcStrip pc && z) $ do
      putStrLn $ "[strip " ++ exe ++ "]"
      when (not (roDryRun ro)) $ do
        exec0 "strip"  [exe]
        return ()

  let copy src dst = do
        verboseLn $ "COPYING " ++ src ++ " to " ++ dst
        let handler :: SomeException -> IO ()
            handler e = do
              writeIORef ior_success False
              hPutStrLn stderr ("COPYING " ++ src ++ " to " ++ dst ++ ":\n" ++ show e)
        whenNotDryRun $
          copyFile src dst `catch` handler

  -- install the executable
  forM_ (pcExeTargetDirs pc) $ \targ_dir -> do
    z <- doesDirectoryExist targ_dir
    if not z then problem $ "target directory " ++ targ_dir ++ " doesn't exist"
      else do
        forM_ (pcExePaths pc) $ \exe -> do
          let target= targ_dir </> takeFileName exe
          copy exe target

  -- archive things (repo and versioned executables)
  let exe = dropExtension (takeFileName (head (pcExePaths pc)))
      ext = takeExtension (head (pcExePaths pc))
      versioned_exe = exe ++ "-" ++ verToStr hs_ver ++ ext
      archive_gz = dropExtension versioned_exe ++ ".tar.gz"
      save_repo = not (null (pcProjectArchive pc))
  when save_repo $ do
    forM_ (pcRepos pc) $ \r -> do
      let arch_msg = "[" ++ repoExe r ++ " archive " ++ archive_gz ++ "]"
      putStrLn arch_msg
      whenNotDryRun $ do
        out <-
          case r of
            Mercurial -> do
              exec0 (repoExe r) ["archive",archive_gz]
            Git -> do
              exec0 (repoExe r) ["archive","--format=tar.gz","-o",archive_gz,"HEAD"]
        putStrLn $ labelLines (arch_msg ++ " ") out
  forM_ (pcProjectArchive pc) $ \targ_dir -> do
    z <- doesDirectoryExist targ_dir
    if not z
      then problem $ "archive directory " ++ targ_dir ++ " doesn't exist"
      else do
        if save_repo
          then copy archive_gz (targ_dir </> archive_gz)
          else verboseLn $ "cannot save repo, repo is not mercurial"
        forM_ (pcExePaths pc) $ \exe -> do
          let versioned_exe = dropExtension (takeFileName exe) ++ "-" ++ verToStr hs_ver ++ takeExtension exe
          copy exe (targ_dir </> versioned_exe)

  when save_repo $ do
    verboseLn $ "removing " ++ archive_gz ++ " (done copying)"
    whenNotDryRun $ removeFile archive_gz

  readIORef ior_success

-- finds the version in the soruce file
findProjectVersion :: FilePath -> IO (Either String (Version,String))
findProjectVersion = (findVersionField <$>) . readFileLines
-- Searches for something of the form
-- (vERSION, vERSION_DATE) = ("1.4.8.0", "02/09/2015") -- post-cabalization release
findVersionField :: [String] -> Either String (Version,String)
findVersionField lns =
    case concatMap pVerLine lns of
     [(vstr,vdate)] ->
      case strToVer vstr of
        Nothing -> Left $ "malformed version string: " ++ show vstr ++ " in source version file"
        Just v -> Right (v,vdate)
     [] -> Left $ "line with " ++ show vERSION_FIELD ++ " not found"
     _ -> Left $ "multiple " ++ vERSION_FIELD ++ " lines found"
  where pVerLine ln
          | null ln = []
          | isSpace (head ln) = []
          | null sfx = []
          | packed_pfx /= vERSION_FIELD = []
          | otherwise =
            case reads (tail sfx) of
              [(val,_)] -> [val]
              _ -> []
          where (pfx,sfx) = span (/='=') ln
                packed_pfx = concatMap (\c -> if isSpace c then [] else [c]) pfx

-- version:             1.4.7.0
findCabalVersion :: FilePath -> IO Version
findCabalVersion fp = readFileLines fp >>= findCabalVersionInLines

findCabalVersionInLines :: [String] -> IO Version
findCabalVersionInLines lns = do
  let ver_key = "version:"
  case filter (ver_key`isPrefixOf`) lns of
    [vln] ->
      case strToVer vstr of
        Nothing -> fatal $ "malformed version line from cabal file: " ++ show vstr
        Just x -> return x
      where vstr = trimWs (drop (length ver_key) vln)
    [] -> fatal "failed to find version in cabal file"
    _ -> fatal "multiple versions found in cabal file"

-- lists 'executable' rules in a .cabal
findCabalExecutables :: FilePath -> IO [String]
findCabalExecutables fp = concatMap findExecLine <$> readFileLines fp
  where findExecLine :: String -> [String]
        findExecLine ln =
            case splitAt (length exec_str) ln of
              (_,"") -> []
              (pfx,sfx)
                | pfx == exec_str && isSpace (head sfx) -> [trimWs sfx]
                | otherwise -> []
          where exec_str = "executable"


type Version = [Int]

strToVer :: String -> Maybe Version
strToVer str =
  case reads ("[" ++ istr ++ "]") of
    [(vs,"")] -> Just vs
    _ -> Nothing
  where istr = map (\c -> if c == '.' then ',' else c) str

verToStr :: Version -> String
verToStr = intercalate "." . map show


readFileLines :: FilePath -> IO [String]
readFileLines = (lines <$>) . readFile

-- foo-1.0.0.1.exe -> 1.0.0.1
maxVersionedExe :: String -> [String] -> Version
maxVersionedExe  exe fs =
  case reverse (sort (concatMap parseVer fs)) of
    [] -> []
    (ver:_) -> ver
 where exe_pfx = exe ++ "-"
       parseVer :: String -> [Version]
       parseVer fnm
        | takeExtension fnm /= "exe" = []
        | otherwise =
          case splitAt (length exe_pfx) (dropExtension fnm) of
           (_,"") -> []
           (ex,vs)
            | ex == exe ->
              case strToVer vs of
                Nothing -> []
                Just v -> [v]
            | otherwise -> []

trimWs :: String -> String
trimWs = reverse . dropWhile isSpace . reverse . dropWhile isSpace

exec :: FilePath -> [String] -> IO (Int,String,String)
exec exe args = do
  (ec,out,err) <- readProcessWithExitCode exe args ""
  case ec of
    ExitFailure x -> return (x,out,err)
    ExitSuccess -> return (0,out,err)

exec0 :: FilePath -> [String] -> IO String
exec0 exe args = do
  (ec,out,err) <- exec exe args
  when (ec /= 0) $ do
    hPutStrLn stderr $ exe ++ " exited " ++ show ec
    when (not (null out)) $
      hPutStrLn stderr $ labelLines ("[" ++ dropExtension (takeFileName exe) ++ ".stdout] ") out
    when (not (null err)) $
      hPutStrLn stderr $ labelLines ("[" ++ dropExtension (takeFileName exe) ++ ".stderr] ") err
    exitFailure
  return out

labelLines :: String -> String -> String
labelLines pfx = unlines . map (pfx++) . lines

getDateString :: IO String
getDateString = showDate <$> ymd
  where showDate :: (Integer,Int,Int) -> String
        showDate (y,m,d) = printf "%02d/%02d" m d ++ "/" ++ show y
year :: IO String
year = (\(y,_,_) -> show y) <$> ymd

ymd :: IO (Integer,Int,Int)
ymd = (toGregorian . localDay . zonedTimeToLocalTime) <$> getZonedTime
