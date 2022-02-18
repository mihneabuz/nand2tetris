module JackCompiler where

import Token
import JackStructure
import JackTokenizer
import JackAnalyzer
import JackGenerator

import Data.List (isSuffixOf)
import Data.Text (Text, unpack)
import qualified Data.Text.IO as TextIO 
import System.Directory (doesFileExist, doesDirectoryExist, listDirectory)

runTokenizer :: FilePath -> IO [Token]
runTokenizer path = do
    isFile <- doesFileExist path
    if isFile then do
        contents <- TextIO.readFile path
        return $ filter (/= Whitespace) . tokens $ contents
    else error $ "File does not exist: " ++ path

runAnalyzer :: FilePath -> IO [Class]
runAnalyzer path = do
    isFile <- doesFileExist path
    isDir  <- doesDirectoryExist path
    if isFile then do
        fileTokens <- runTokenizer path
        let fileClass = analyzeClass fileTokens
        return [fileClass]
    else if isDir then do
        files <- listDirectory path
        let jackFiles = map (\file -> path ++ "/" ++ file) . filter (\file -> ".jack" `isSuffixOf` file) $ files
        classes <- mapM runAnalyzer jackFiles
        return $ concat classes
    else error $ "File does not exist: " ++ path

runCompiler :: FilePath -> IO [(FilePath, Text)]
runCompiler path = do
    classes <- runAnalyzer path

    isDir <- doesDirectoryExist path
    let outputDir = if isDir then path ++ "/" else reverse . dropWhile (/= '/') . reverse $ path

    let generated = map generateClass classes
    let names = map (\(Class name _ _) -> outputDir ++ unpack name ++ ".vm") classes

    -- mapM_ print classes
    -- mapM_ (putStrLn . unpack) generated
    -- print names

    return $ zip names generated

jackCompiler :: FilePath -> IO ()
jackCompiler path = do
    compiled <- runCompiler path
    mapM_ (uncurry TextIO.writeFile) compiled
