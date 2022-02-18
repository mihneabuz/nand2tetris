{-# LANGUAGE OverloadedStrings #-}

import JackStructure
import JackAnalyzer
import JackGenerator
import JackCompiler
import Token

import Data.Text (Text, unpack)
import qualified Data.Text.IO as TextIO 

main :: IO ()
main = do
    jackCompiler "../Seven"
    jackCompiler "../Average"
    jackCompiler "../ConvertToBin"
    jackCompiler "../ComplexArrays"
    jackCompiler "../Square"
    jackCompiler "../Pong"

test = [ Keyword "class"
       , Identifier "Main"
       , Symbol '{'
       , Keyword "field"
       , Identifier "int"
       , Identifier "a"
       , Symbol ';'
       , Keyword "static"
       , Identifier "char"
       , Identifier "b"
       , Symbol ','
       , Identifier "c"
       , Symbol ';'
       , Keyword "function"
       , Identifier "void"
       , Identifier "main"
       , Symbol '('
       , Symbol ')'
       , Symbol '{'
       , Keyword "let"
       , Identifier "x"
       , Symbol '['
       , IntegerConst 1
       , Symbol '+'
       , Identifier "y"
       , Symbol ']'
       , Symbol '='
       , IntegerConst 10
       , Symbol ';'
       , Keyword "do"
       , Identifier "run"
       , Symbol '('
       , Identifier "x"
       , Symbol ','
       , Identifier "x"
       , Symbol '+'
       , IntegerConst 10
       , Symbol ')'
       , Symbol ';'
       , Symbol '}'
       , Keyword "method"
       , Identifier "int"
       , Identifier "add"
       , Symbol '('
       , Identifier "int"
       , Identifier "a"
       , Symbol ','
       , Identifier "int"
       , Identifier "b"
       , Symbol ')'
       , Symbol '{'
       , Keyword "var"
       , Identifier "int"
       , Identifier "temp"
       , Symbol ','
       , Identifier "iter"
       , Symbol ';'
       , Keyword "var"
       , Identifier "char"
       , Identifier "chr"
       , Symbol ';'
       , Keyword "let"
       , Identifier "temp"
       , Symbol '='
       , IntegerConst 1
       , Symbol '+'
       , IntegerConst 2
       , Symbol ';'
       , Keyword "if"
       , Symbol '('
       , IntegerConst 1
       , Symbol ')'
       , Symbol '{'
       , Keyword "let"
       , Identifier "temp"
       , Symbol '='
       , Identifier "temp"
       , Symbol '+'
       , IntegerConst 1
       , Symbol ';'
       , Symbol '}'
       , Keyword "else"
       , Symbol '{'
       , Keyword "let"
       , Identifier "temp"
       , Symbol '='
       , Identifier "temp"
       , Symbol '+'
       , IntegerConst 2
       , Symbol ';'
       , Symbol '}'
       , Keyword "return"
       , IntegerConst 0
       , Symbol ';'
       , Symbol '}'
       , Symbol '}'
       ]


test2 = [ Keyword "class"
       , Identifier "Main"
       , Symbol '{'
       , Keyword "field"
       , Identifier "int"
       , Identifier "a"
       , Symbol ';'
       , Keyword "static"
       , Identifier "char"
       , Identifier "b"
       , Symbol ','
       , Identifier "c"
       , Symbol ';'
       , Keyword "function"
       , Identifier "void"
       , Identifier "main"
       , Symbol '('
       , Symbol ')'
       , Symbol '{'
       , Symbol '}'
       , Keyword "method"
       , Identifier "int"
       , Identifier "add"
       , Symbol '('
       , Identifier "int"
       , Identifier "a"
       , Symbol ','
       , Identifier "int"
       , Identifier "b"
       , Symbol ')'
       , Symbol '{'
       , Keyword "var"
       , Identifier "int"
       , Identifier "temp"
       , Symbol ','
       , Identifier "iter"
       , Symbol ';'
       , Keyword "var"
       , Identifier "char"
       , Identifier "chr"
       , Symbol ';'
       , Keyword "let"
       , Identifier "temp"
       , Symbol '='
       , IntegerConst 1
       , Symbol '+'
       , IntegerConst 2
       , Symbol ';'
       , Keyword "return"
       , IntegerConst 0
       , Symbol ';'
       , Symbol '}'
       , Symbol '}'
       ]
