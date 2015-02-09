module Main where

import Proj.Releaser

import System.Environment(getArgs)

main :: IO ()
main = getArgs >>= run
