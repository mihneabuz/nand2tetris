{-# LANGUAGE OverloadedStrings #-}

import JackAnalyzer
import Token

main :: IO ()
main = do
    let cls = analyzeClass test
    print cls

    cls1 <- runAnalyzer "../ArrayTest"
    cls2 <- runAnalyzer "../ExpressionLessSquare"
    cls3 <- runAnalyzer "../Square"

    print cls1 
    print cls2
    print cls3

    return ()

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
