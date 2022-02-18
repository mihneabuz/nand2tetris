{-# LANGUAGE OverloadedStrings #-}

module JackGenerator where

import JackStructure
import JackSymbolTable

import Data.Char (ord)
import Data.Text (Text)
import Data.List (intersperse, foldl')
import qualified Data.Text as T

(+++) = T.append

generateClass :: Class -> Text
generateClass (Class name varDecs subroutines) = do
    let classTable = constructClassTable varDecs
    let classType  = Custom name
    let instructions = generateSubroutines classType classTable subroutines

    T.intercalate "\n" . reverse $ instructions 

   
generateSubroutines :: VarType -> [Sym] -> [Subroutine] -> [Text]
generateSubroutines classType classSymbols = fst . foldl' folder ([], 1)
    where folder (acc, label) routine  = (generated ++ acc, newLabel) 
            where (generated, newLabel) = genSubroutine classType classSymbols label routine

genSubroutine :: VarType -> [Sym] -> Int -> Subroutine -> ([Text], Int)
genSubroutine classType classTable label subroutine = do
    let localTable = constructLocalTable classType subroutine
    let className = (\(Custom x) -> x) classType 
    let syms = (classTable, localTable)
    case subroutine of
      (Method      returnType name params body) -> genMethod (className +++ "." +++ name) syms label body 
      (Function    returnType name params body) -> genFunc   (className +++ "." +++ name) syms label body 
      (Constructor returnType name params body) -> genConstr (className +++ "." +++ name) syms label (countFields classTable) body 

genFunc :: Text -> SymbolTable -> Int -> Body -> ([Text], Int)
genFunc name syms label (Body locals statements) = do
    let header = "function " +++ name +++ " " +++ (T.pack . show . length $ locals)
    generateStatements syms [header] label statements

genConstr :: Text -> SymbolTable -> Int -> Int -> Body -> ([Text], Int)
genConstr name syms label fields (Body locals statements) = do 
    let header = "function " +++ name +++ " " +++ (T.pack . show . length $ locals)
    let boiler = ["pop pointer 0", "call Memory.alloc 1", "push constant " +++ (T.pack . show $ fields), header]
    generateStatements syms boiler label statements

genMethod :: Text -> SymbolTable -> Int -> Body -> ([Text], Int)
genMethod name syms label (Body locals statements) = do
    let header = "function " +++ name +++ " " +++ (T.pack . show . length $ locals)
    let boiler = ["pop pointer 0", "push argument 0", header]
    generateStatements syms boiler label statements




generateStatements :: SymbolTable -> [Text] -> Int -> [Statement] -> ([Text], Int)
generateStatements lookup init label = foldl' folder (init, label)
    where folder (acc, label) statement  = (generated ++ acc, newLabel)
            where (generated, newLabel) = genStatement lookup label statement

genStatement :: SymbolTable -> Int -> Statement -> ([Text], Int)
genStatement syms label (Let id Nothing expr) = (pop : genExpr, label)
    where genExpr = generateExpr syms expr
          (Sym _ _ segment count) = lookupSymbol syms id
          pop = "pop " +++ segment +++ " " +++ (T.pack . show $ count)

genStatement syms label (Let id (Just indexExpr) valueExpr) = do
    let genIndexExpr = generateExpr syms indexExpr
    let genValueExpr = generateExpr syms valueExpr

    let (Sym _ _ segment count) = lookupSymbol syms id
    let push = "push " +++ segment +++ " " +++ (T.pack . show $ count)

    let ins = [ "pop that 0",
                "pop pointer 1",
                "add",
                push
              ]

    (ins ++ genIndexExpr ++ genValueExpr, label)

genStatement lookup label (If expr trueBlock falseBlock) = do
    let genExpr = generateExpr lookup expr
    let (genTrueBlock,  newLabel1) = generateStatements lookup [] (label + 1) trueBlock
    let (genFalseBlock, newLabel2) = generateStatements lookup [] newLabel1 falseBlock

    let l1 = "IF_EXPR_" +++ (T.pack . show $ label)
    let l2 = "IF_END_" +++ (T.pack . show $ label)

    let ins = ("label " +++ l2)
            : genFalseBlock
            ++ ["label " +++ l1, "goto " +++ l2]
            ++ genTrueBlock
            ++ ["if-goto " +++ l1, "not"]
            ++ genExpr

    (ins, newLabel2)

genStatement lookup label (While expr loopBlock) = do
    let genExpr = generateExpr lookup expr
    let (genLoop, newLabel) = generateStatements lookup [] (label + 1) loopBlock

    let l1 = "WHILE_EXPR_" +++ (T.pack . show $ label)
    let l2 = "WHILE_END_" +++ (T.pack . show $ label)

    let ins = ["label " +++ l2, "goto " +++ l1]
            ++ genLoop
            ++ ["if-goto " +++ l2, "not"]
            ++ genExpr
            ++ ["label " +++ l1]

    (ins, newLabel)

genStatement lookup label (Do expr) = ("pop temp 0" : genExpr, label)
    where genExpr = generateExpr lookup expr

genStatement lookup label (Return Nothing) = (["return", "push constant 0"], label)
genStatement lookup label (Return (Just expr)) = ("return" : genExpr, label)
    where genExpr = generateExpr lookup expr




generateExpr :: SymbolTable -> Expr -> [Text]
-- CONSTANTS
generateExpr _ Null = ["push constant 0"]
generateExpr _ (IntConst x)  = [T.pack $ "push constant " ++ show x]
generateExpr _ (CharConst x) = [T.pack $ "push constant " ++ show (ord x)]
generateExpr _ (BoolConst x) = if x then ["not", "push constant 0"]
                                         else ["push constant 0"]
generateExpr _ (StrConst s) = do
    let append = "call String.appendChar 2"
    let chars  = map (\x -> T.pack $ "push constant " ++ show (ord x)) . reverse . T.unpack $ s
    let ins = append : intersperse append chars

    let len = show . T.length $ s
    let alloc = ["call String.new 1", T.pack $ "push constant " ++ len]

    ins ++ alloc


-- VARIABLES
generateExpr syms This = ["push " +++ segment +++ " " +++ (T.pack . show $ count)]
    where (Sym _ _ segment count) = lookupSymbol syms "this"

generateExpr syms (Variable Nothing id) = ["push " +++ segment +++ " " +++ (T.pack . show $ count)]
    where (Sym _ _ segment count) = lookupSymbol syms id

generateExpr syms (Variable (Just indexExpr) id) = do
    let (Sym _ _ segment count) = lookupSymbol syms id
    let push = "push " +++ segment +++ " " +++ (T.pack . show $ count)
    let genExpr = generateExpr syms indexExpr
    ["push that 0", "pop pointer 1", "add", push] ++ genExpr


-- OPERATIONS
generateExpr syms (Unary op expr) = unaryVM op : generateExpr syms expr 
generateExpr syms (Binary op expr1 expr2) = binaryVM op : genExpr2 ++ genExpr1
    where genExpr1 = generateExpr syms expr1
          genExpr2 = generateExpr syms expr2


-- SUBROUTINE CALLS
generateExpr syms (Call Nothing id exprs) = do
    let (Sym _ (Custom className) segment count) = lookupSymbol syms "this"
    let genArgs = concatMap (generateExpr syms) . reverse $ exprs
    let genCall = "call " +++ className +++ "." +++ id +++ " " +++ (T.pack . show . (+1) . length $ exprs)
    let genThis = "push " +++ segment +++ " " +++ (T.pack . show $ count)
    genCall : genArgs ++ [genThis]

generateExpr syms (Call (Just id1) id2 exprs) = do
    let genArgs = concatMap (generateExpr syms) . reverse $ exprs
    if checkSymbol syms id1 then do
        let (Sym _ (Custom className) segment count) = lookupSymbol syms id1
        let genPush = "push " +++ segment +++ " " +++ (T.pack . show $ count)
        let genCall = "call " +++ className +++ "." +++ id2 +++ " " +++ (T.pack . show . (+1) . length $ exprs)
        genCall : genArgs ++ [genPush]
    else do
        let genCall = "call " +++ id1 +++ "." +++ id2 +++ " " +++ (T.pack . show . length $ exprs)
        genCall : genArgs


binaryVM :: Char -> Text
binaryVM op = case op of
   '+' -> "add" 
   '-' -> "sub"
   '&' -> "and"
   '|' -> "or"
   '<' -> "lt"
   '>' -> "gt"
   '=' -> "eq"
   '*' -> "call Math.multiply 2"
   '/' -> "call Math.divide 2"
   _ -> "ERROR"

unaryVM :: Char -> Text
unaryVM op = case op of
   '~' -> "not"
   '-' -> "neg"
   _ -> "ERROR"
