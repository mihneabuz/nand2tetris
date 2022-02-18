{-# LANGUAGE OverloadedStrings #-}

module JackTokenizer where

import Parser
import Token

import Control.Applicative
import Data.Char (isDigit)
import Data.Text (Text)
import qualified Data.Text as T

type JackParser = Parser Token

(+++) = T.append
(<:) = T.cons

delimiters :: [Char]
delimiters = jackWhitespace ++ jackSymbols

takeWord :: Text -> Text
takeWord = T.takeWhile (not . flip elem delimiters)

dropWord :: Text -> Text
dropWord = T.dropWhile (not . flip elem delimiters)

isInteger :: Text -> Bool
isInteger text = if T.head text == '-' then T.all isDigit (T.tail text) else T.all isDigit text

jackWhitespace :: [Char]
jackWhitespace = ['\n', '\t', '\r', ' ']

jackWhitespaceParser :: JackParser
jackWhitespaceParser = Parser $ \text -> 
    if T.null text 
       then Nothing
       else if T.head text `elem` jackWhitespace
               then Just (Whitespace, T.dropWhile (`elem` jackWhitespace) text)
               else Nothing

jackKeywords :: [Text]
jackKeywords = map T.pack [ "class"
                          , "constructor"
                          , "function"
                          , "method"
                          , "field"
                          , "static"
                          , "var"
                          , "true"
                          , "false"
                          , "null"
                          , "this"
                          , "let"
                          , "do"
                          , "if"
                          , "else"
                          , "while"
                          , "return"
                          ]


jackKeywordParser :: JackParser
jackKeywordParser = Parser $ \text ->
    if T.null text
       then Nothing
       else if takeWord text `elem` jackKeywords
               then Just (Keyword (takeWord text), dropWord text)
               else Nothing

jackSymbols ::  [Char]
jackSymbols = [ '(', ')'
              , '[', ']'
              , '{', '}'
              , '.', ',', ';'
              , '+', '-'
              , '*', '/'
              , '&', '|'
              , '<', '>'
              , '='
              , '~'
              ]

jackSymbolParser :: JackParser
jackSymbolParser = Parser $ \text -> 
    if T.null text 
       then Nothing
       else if T.head text `elem` jackSymbols
               then Just (Symbol (T.head text), T.tail text)
               else Nothing

jackConstIntegerParser :: JackParser
jackConstIntegerParser = Parser $ \text ->
    if T.null text
       then Nothing
       else if isInteger $ takeWord text 
               then Just (IntegerConst (read . T.unpack $ takeWord text), dropWord text)
               else Nothing

jackConstStringParser :: JackParser
jackConstStringParser = Parser $ \text ->
    if T.null text
       then Nothing
       else if T.head text == '"'
               then Just (StringConst (T.takeWhile (/= '"') . T.tail $ text), T.tail . T.dropWhile (/= '"') . T.tail $ text)
               else Nothing

jackIdentifierParser :: JackParser
jackIdentifierParser = Parser $ \text ->
    if T.null text 
       then Nothing
       else Just (Identifier (takeWord text), dropWord text)

jackParser :: Parser [Token]
jackParser = many  $  jackWhitespaceParser
                  <|> jackSymbolParser
                  <|> jackConstStringParser
                  <|> jackConstIntegerParser
                  <|> jackKeywordParser
                  <|> jackIdentifierParser

strip :: Text -> Text
strip text 
  | T.null text = T.empty
  | T.head text `elem` jackWhitespace = T.cons ' ' . strip . T.dropWhile (`elem` jackWhitespace) $ text
  | T.head text == '"' = let (before, after) = T.break (== '"') $ T.tail text
                         in ('"' <: before +++ "\"") +++ (strip . T.tail $ after)
  | otherwise = case T.take 2 text of
                  "//" -> strip . T.dropWhile (/= '\n') $ text
                  "/*" -> strip . T.drop 2 . snd . T.breakOn "*/" $ text
                  _ -> T.head text `T.cons` strip (T.tail text)

tokens :: Text -> [Token]
tokens text = case runParser jackParser $ strip text of
                Just (tokens, rest) -> tokens
                Nothing -> []
