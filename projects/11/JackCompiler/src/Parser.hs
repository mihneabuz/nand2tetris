module Parser where

import Control.Applicative
import Data.Text (Text)

newtype Parser a = Parser { runParser :: Text -> Maybe (a, Text) }

instance Functor Parser where
    fmap f (Parser x) = Parser $ \str -> do
        (res, str') <- x str
        return (f res, str')

instance Applicative Parser where
    pure a = Parser $ \str -> Just (a, str)
    (Parser f) <*> (Parser x) = Parser $ \str -> do
        (f', str1) <- f str  
        (x', str2) <- x str1
        return (f' x', str2)

instance Monad Parser where
    (Parser x) >>= f = Parser $ \str -> do
        (res, str') <- x str
        runParser (f res) str'

instance MonadFail Parser where
    fail _ = Parser $ const Nothing

instance Alternative Parser where
    empty = Parser $ const Nothing
    (Parser x) <|> (Parser y) = Parser $ \str -> 
        case x str of
          Just res -> Just res
          Nothing  -> y str
