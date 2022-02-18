module Main where

import JackAnalyzer

import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    classes <- runAnalyzer (args !! 1)
    return ()
