module Main where

import JackCompiler

import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    jackCompiler (args !! 1)
