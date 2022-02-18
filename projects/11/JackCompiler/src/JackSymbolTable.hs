{-# LANGUAGE OverloadedStrings #-}

module JackSymbolTable where

import JackStructure
import Data.Text (Text, unpack)

data Sym = Sym Identifier VarType Text Int
         deriving (Show, Eq)
type SymbolTable = ([Sym], [Sym])

safeHead :: [a] -> Maybe a
safeHead []     = Nothing
safeHead (x:xs) = Just x

lookupSymbol :: SymbolTable -> Identifier -> Sym
lookupSymbol (classTable, localTable) id = 
    let _lookup id = safeHead . filter (\(Sym sym _ _ _) -> id == sym)
    in case _lookup id localTable of
         Just symbol -> symbol
         Nothing     -> case _lookup id classTable of
            Just symbol -> symbol
            Nothing     -> error $ "Variable " ++ unpack id ++ " undefined"

checkSymbol :: SymbolTable -> Identifier -> Bool
checkSymbol (classTable, localTable) id = 
    let _lookup id = safeHead . filter (\(Sym sym _ _ _) -> id == sym)
    in case _lookup id localTable of
         Just symbol -> True
         Nothing     -> case _lookup id classTable of
            Just symbol -> True
            Nothing     -> False

constructClassTable :: [ClassVarDec] -> [Sym]
constructClassTable varDecs = staticSyms ++ fieldSyms
    where 
        static (Static _ _) = True
        static _ = False

        staticDecs = filter static varDecs 
        fieldDecs  = filter (not . static) varDecs 

        staticSyms = zipWith (\x (Static varType id) -> Sym id varType "static" x) [0..] staticDecs
        fieldSyms  = zipWith (\x (Field  varType id) -> Sym id varType "this"  x) [0..] fieldDecs 

constructLocalTable :: VarType -> Subroutine -> [Sym]
constructLocalTable classType routine = case routine of
    (Function    _ _ (ParameterList args) (Body locals _)) -> _symTable args locals
    (Constructor _ _ (ParameterList args) (Body locals _)) -> Sym "this" classType "pointer" 0 : _symTable args locals
    (Method      _ _ (ParameterList args) (Body locals _)) -> _symTable (VarDec classType "this" : args) locals
    where
        _symTable arguments localVars = argSyms ++ localSyms
            where argSyms   = zipWith (\x (VarDec varType id) -> Sym id varType "argument" x) [0..] arguments
                  localSyms = zipWith (\x (VarDec varType id) -> Sym id varType "local"    x) [0..] localVars

countFields :: [Sym] -> Int
countFields = length . filter (\(Sym _ _ segment _) -> segment == "this")
