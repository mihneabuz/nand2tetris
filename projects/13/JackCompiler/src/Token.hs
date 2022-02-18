module Token where

import Data.Text (Text, unpack)

data Token = Keyword Text
           | Symbol Char
           | IntegerConst Int
           | StringConst Text
           | Identifier Text
           | Whitespace
            deriving (Eq)

instance Show Token where
    show (Keyword x)      = "<keyword> "      ++ unpack x ++ " </keyword>"
    show (Symbol x)       = "<symbol> "       ++ [x]      ++ " </symbol>"
    show (IntegerConst x) = "<integerConst> " ++ show x   ++ " </integerConst>"
    show (StringConst x)  = "<stringConst> "  ++ unpack x ++ " </stringConst>"
    show (Identifier x)   = "<identifier> "   ++ unpack x ++ " </identifier>"
    show Whitespace       = "<whitespace>"
