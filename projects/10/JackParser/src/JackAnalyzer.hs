{-# LANGUAGE OverloadedStrings #-}

module JackAnalyzer where

import Token
import JackTokenizer

import Data.List (isSuffixOf)
import Data.List.Split (splitOn)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TextIO 
import System.Directory

type Identifier = String

data VarType = Void
             | Int 
             | Char
             | String
             | Custom String
             deriving (Show, Eq)

data ClassVarDec = Static VarType Identifier
                 | Field  VarType Identifier
                 deriving (Show, Eq)

data VarDec = VarDec VarType Identifier
            deriving (Show, Eq)

data Class = Class String [ClassVarDec] [Subroutine]
           deriving (Eq)

data Subroutine = Constructor VarType Identifier ParameterList Body
                | Function VarType Identifier ParameterList Body
                | Method VarType Identifier ParameterList Body
                deriving (Eq)

newtype ParameterList = ParameterList [VarDec]
                      deriving (Show, Eq)

data Body = Body [VarDec] [Statement]
          deriving (Eq)

data Statement = Let Identifier Index Expr
               | If Expr [Statement] [Statement]
               | While Expr [Statement]
               | Do Expr
               | Return (Maybe Expr)
               deriving (Eq)

data Expr = IntConst Int
          | CharConst Char
          | StrConst String
          | BoolConst Bool
          | Variable Index Identifier 
          | Call (Maybe Identifier) Identifier [Expr]
          | Unary Char Expr
          | Binary Char Expr Expr
          | This
          | Null
          deriving (Eq)

type Index = Maybe Expr

data SymbolType = UnaryOP | BinaryOP | OpenParens | CloseParens

--  DELETE: Remove show definitions
instance Show Class where
    show (Class name varDecs subroutines) = 
        "Class " ++ name ++ "\n" ++
        concatMap (\x -> show x ++ "\n") varDecs ++ "\n" ++
        concatMap (\x -> show x ++ "\n\n") subroutines

instance Show Subroutine where
    show (Constructor varType name (ParameterList params) body) =
        "Constructor " ++ show varType ++ " " ++ name ++ " (" ++ 
        concatMap (\x -> show x ++ " ") params ++ ")\n" ++
        show body
    show (Function varType name (ParameterList params) body) =
        "Function " ++ show varType ++ " " ++ name ++ " (" ++ 
        concatMap (\x -> show x ++ " ") params ++ ")\n" ++
        show body
    show (Method varType name (ParameterList params) body) =
        "Method " ++ show varType ++ " " ++ name ++ " (" ++ 
        concatMap (\x -> show x ++ " ") params ++ ")\n" ++
        show body

instance Show Body where
    show (Body varDecs statements) =
        "{\n" ++ concatMap (\x -> "  " ++ show x ++ "\n") varDecs ++ "\n" ++
        concatMap (\x -> "  " ++ show x ++ "\n") statements ++ "}\n"

instance Show Expr where
    show (IntConst i)  = show i
    show (CharConst c) = show c
    show (StrConst s)  = show s
    show (BoolConst b) = show b
    show (Unary op expr)         = op : "(" ++ show expr ++ ")"
    show (Binary op expr1 expr2) = "(" ++ show expr1 ++ ") " ++ [op] ++ " (" ++ show expr2 ++ ")"
    show (Variable Nothing name)     = name
    show (Variable (Just expr) name) = name ++ "[" ++ show expr ++ "]"
    show (Call Nothing name args)    = "CALL " ++ name ++ "(" ++ show args ++ ")"
    show (Call (Just cls) name args) = "CALL " ++ cls ++ "." ++ name ++ "(" ++ show args ++ ")"
    show This = "this"
    show Null = "null"

instance Show Statement where
    show (Let name Nothing expr) = "Let " ++ name ++ " = " ++ show expr
    show (Let name (Just indexExpr) expr) = "Let " ++ name ++ "[" ++ show indexExpr ++ "] = " ++ show expr
    show (If expr trueBlock falseBlock) = "If (" ++ show expr ++ ") {\n" ++ 
        concatMap (\x -> "   " ++ show x ++ "\n") trueBlock ++
            (if null falseBlock then "" else "   } else {\n" ++
            concatMap (\x -> "   " ++ show x ++ "\n") falseBlock) ++
        "   }"
    show (While expr loopBlock) = "While (" ++ show expr ++ ") {\n" ++
        concatMap (\x -> "   " ++ show x ++ "\n") loopBlock ++ "   }"
    show (Do expr) = "Do " ++ show expr
    show (Return Nothing) = "Return"
    show (Return (Just expr)) = "Return " ++ show expr

takeParameterList :: [Token] -> ([Token], [Token])
takeParameterList tokens = case head tokens of
    Symbol '(' -> fmap tail . break (== Symbol ')') . tail $ tokens
    _ -> error "Expected Parameter List"

takeBlock :: [Token] -> ([Token], [Token])
takeBlock [] = error "Expected Block but got nothing"
takeBlock tokens = case head tokens of
    Symbol '{' -> _takeBlock 1 [] (tail tokens)
    _ -> error "Expected Block"
    where
        _takeBlock parens acc [] = error "Expected end of Block"
        _takeBlock parens acc (token:rest)= case token of
            Symbol '{' -> _takeBlock (parens + 1) (token : acc) rest
            Symbol '}' -> if parens == 1 then (reverse acc, rest)
                                         else _takeBlock (parens - 1) (token : acc) rest
            _ -> _takeBlock parens (token : acc) rest

takeParens :: [Token] -> ([Token], [Token])
takeParens [] = error "Expected Parenthesis block but got nothing"
takeParens tokens = case head tokens of
    Symbol '(' -> _takeParens 1 [] (tail tokens)
    _ -> error "Expected Parenthesis block"
    where
        _takeParens parens acc [] = error "Expected end of Paranthesis"
        _takeParens parens acc (token:rest) = case token of
            Symbol '(' -> _takeParens (parens + 1) (token : acc) rest
            Symbol ')' -> if parens == 1 then (reverse acc, rest)
                                         else _takeParens (parens - 1) (token : acc) rest
            _ -> _takeParens parens (token : acc) rest

takeIndexParens :: [Token] -> ([Token], [Token])
takeIndexParens [] = error "Expected Square Paranthesis block but got nothing"
takeIndexParens tokens = case head tokens of
    Symbol '[' -> _takeIndexParens 1 [] (tail tokens)
    _ -> error "Expected Square Paranthesis block"
    where
        _takeIndexParens parens acc [] = error "Expected end of Index Paranthesis"
        _takeIndexParens parens acc (token:rest) = case token of
            Symbol '[' -> _takeIndexParens (parens + 1) (token : acc) rest
            Symbol ']' -> if parens == 1 then (reverse acc, rest)
                                         else _takeIndexParens (parens - 1) (token : acc) rest
            _ -> _takeIndexParens parens (token : acc) rest

takeArgumentList :: [Token] -> ([[Token]], [Token])
takeArgumentList [] = error "Expected Argument List block but got nothing"
takeArgumentList tokens = case take 2 tokens of
    [Symbol '(', Symbol ')'] -> ([], drop 2 tokens)
    [Symbol '(', _] -> _takeArgumentList 1 [[]] (tail tokens)
    _ -> error "Expected Argument List"
    where
        _takeArgumentList parens acc []    = error "Expected end of Argument List"
        _takeArgumentList parens [] tokens = error "Bad acc takeArgumentList"
        _takeArgumentList parens (last:acc) (token:rest) = case token of
            Symbol '(' -> _takeArgumentList (parens + 1) ((token : last) : acc) rest
            Symbol ')' -> if parens == 1 then (reverse $ map reverse $ last : acc, rest)
                                         else _takeArgumentList (parens - 1) ((token : last) : acc) rest
            Symbol ',' -> _takeArgumentList parens ([] : last : acc) rest
            _ -> _takeArgumentList parens ((token : last) : acc) rest
    

takeStatement :: [Token] -> ([Token], [Token])
takeStatement = fmap tail . break (== Symbol ';')

analyzeClass :: [Token] -> Class
analyzeClass tokens = case take 2 tokens of
    [Keyword "class", Identifier className] -> Class (T.unpack className) classVars classSubroutines
    _ -> error "Expected Class Definition"
    where
        (classVars, rest) = parseClassVars . fst . takeBlock . drop 2 $ tokens
        classSubroutines = parseClassSubroutines rest

readType :: Text -> VarType
readType "void"   = Void
readType "int"    = Int
readType "char"   = Char
readType "string" = String
readType t        = Custom (T.unpack t)

parseClassVars :: [Token] -> ([ClassVarDec], [Token])
parseClassVars = _parseClassVars []
    where 
        _parseClassVars acc tokens = case take 2 tokens of
            [Keyword "static", Identifier varType] ->
                _parseClassVars (acc ++ map (Static (readType varType)) varNames) rest
            [Keyword "field", Identifier varType] ->
                _parseClassVars (acc ++ map (Field  (readType varType)) varNames) rest
            _ -> (acc, tokens)
            where (line, rest) = takeStatement . drop 2 $ tokens
                  varNames = map (\[Identifier x] -> T.unpack x) . splitOn [Symbol ','] $ line

parseClassSubroutines :: [Token] -> [Subroutine]
parseClassSubroutines [] = []
parseClassSubroutines tokens = subroutine : parseClassSubroutines rest
    where (subroutine, rest) = parseSubroutine tokens

parseSubroutine :: [Token] -> (Subroutine, [Token])
parseSubroutine tokens = case head tokens of
    Keyword "constructor" -> (Constructor returnType routineName parameterList body, rest)
    Keyword "function"    -> (Function returnType routineName parameterList body, rest)
    Keyword "method"      -> (Method returnType routineName parameterList body, rest)
    _ -> error "Expected Subroutine Definition"
    where 
        (returnType, routineName)   = _parseSubroutine1 $ drop 1 tokens
        (parameterList, body, rest) = _parseSubroutine2 $ drop 3 tokens

        _parseSubroutine1 tokens = case take 2 tokens of
            [Identifier returnType, Identifier routineName] -> (readType returnType, T.unpack routineName)
            _ -> error "Expected Return Type and Subroutine Name"

        _parseSubroutine2 tokens = (parseParameterList tokens1, parseBody tokens2, rest2)
            where (tokens1, rest1) = takeParens tokens
                  (tokens2, rest2) = takeBlock rest1

parseParameterList :: [Token] -> ParameterList
parseParameterList [] = ParameterList []
parseParameterList tokens = ParameterList . map varDec . splitOn [Symbol ','] $ tokens
    where varDec [Identifier varType, Identifier varName] = VarDec (readType varType) (T.unpack varName)
          varDec _ = error "Bad Var Declaration in Parameter List"

parseBody :: [Token] -> Body
parseBody [] = Body [] []
parseBody tokens = Body localVars statements
    where (localVars, rest) = parseLocalVars tokens
          statements = parseStatements rest

parseLocalVars :: [Token] -> ([VarDec], [Token])
parseLocalVars = _parseLocalVars []
    where
        _parseLocalVars acc tokens = case take 2 tokens of
            [Keyword "var", Identifier varType] -> 
                _parseLocalVars (acc ++ map (VarDec (readType varType)) varNames) rest
            _ -> (acc, tokens)
            where (line, rest) = takeStatement . drop 2 $ tokens
                  varNames = map (\[Identifier x] -> T.unpack x) . splitOn [Symbol ','] $ line

parseStatements :: [Token] -> [Statement]
parseStatements [] = []
parseStatements tokens = statement : parseStatements rest
    where (statement, rest) = parseStatement tokens

parseStatement :: [Token] -> (Statement, [Token])
parseStatement tokens = case head tokens of
    Keyword "let"    -> parseLetStatement    $ tail tokens
    Keyword "if"     -> parseIfStatement     $ tail tokens
    Keyword "while"  -> parseWhileStatement  $ tail tokens
    Keyword "do"     -> parseDoStatement     $ tail tokens
    Keyword "return" -> parseReturnStatement $ tail tokens
    _ -> error "Expected Statement"
    where 
        parseLetStatement tokens = do
            let (statementTokens, rest) = takeStatement tokens
            let (begin, restExpr) = splitAt 2 statementTokens
            case begin of
                [Identifier varName, Symbol '='] -> (Let (T.unpack varName) Nothing (parseExpr restExpr), rest)
                [Identifier varName, Symbol '['] -> do
                    let (indexExpr, restExpr2) = takeIndexParens (Symbol '[' : restExpr)
                    if head restExpr2 == Symbol '='
                       then (Let (T.unpack varName) (Just $ parseExpr indexExpr) (parseExpr $ tail restExpr2), rest)
                       else error "Bad Let Statement, Expected '='"
                _ -> error "Bad Let Statement"

        parseIfStatement tokens = do
            let (exprTokens, rest1) = takeParens tokens
            let (trueBlock,  rest2) = takeBlock rest1
            if null rest2 || (head rest2 /= Keyword "else")
                then (If (parseExpr exprTokens) (parseStatements trueBlock) [], rest2)
                else do
                    let (falseBlock, rest3) = takeBlock $ tail rest2
                    (If (parseExpr exprTokens) (parseStatements trueBlock) (parseStatements falseBlock), rest3)

        parseWhileStatement tokens = do
            let (exprTokens, rest1) = takeParens tokens
            let (loopBlock,  rest2) = takeBlock rest1
            (While (parseExpr exprTokens) (parseStatements loopBlock), rest2)

        parseDoStatement tokens = do
            let (exprTokens, rest) = takeStatement tokens
            (Do (parseExpr exprTokens), rest)

        parseReturnStatement tokens = do
            if head tokens == Symbol ';'
               then (Return Nothing, tail tokens)
               else do
                   let (exprTokens, rest) = takeStatement tokens
                   (Return (Just $ parseExpr exprTokens), rest)

isUnaryOP :: Char -> Bool
isUnaryOP = flip elem ['~', '-']

isBinaryOP :: Char -> Bool
isBinaryOP = flip elem ['+', '-', '*', '/', '&', '|', '<', '>', '=']

symbolType :: Char -> SymbolType
symbolType '(' = OpenParens
symbolType ')' = CloseParens
symbolType x 
  | isUnaryOP x  = UnaryOP
  | isBinaryOP x = BinaryOP
  | otherwise = error $ "Bad Symbol " ++ [x]

parseExpr :: [Token] -> Expr
parseExpr tokens = do
    let (term, rest) = parseTerm tokens
    if null rest 
       then term
       else case head rest of
          Symbol op -> if isBinaryOP op 
                          then Binary op term (parseExpr $ tail rest)
                          else error $ "Expected Binary Operator " ++ [op]
          _ -> error $ "Expected Operator " ++ show (head rest) 

parseTerm :: [Token] -> (Expr, [Token])
parseTerm tokens = case head tokens of
    Keyword "true"  -> (BoolConst True, tail tokens)
    Keyword "false" -> (BoolConst False, tail tokens)
    Keyword "this"  -> (This, tail tokens)
    Keyword "null"  -> (Null, tail tokens)

    IntegerConst x  -> (IntConst x, tail tokens)
    StringConst str -> (StrConst (T.unpack str), tail tokens)

    Symbol op -> case symbolType op of
       UnaryOP -> do
           let (expr, rest) = parseTerm $ tail tokens
           (Unary op expr, rest)
       OpenParens -> do
           let (parensBlock, rest) = takeParens tokens
           let expr = parseExpr parensBlock
           (expr, rest)
       BinaryOP    -> error "Bad Binary Operator"
       CloseParens -> error "Bad Closing Parenthesis"

    Identifier str -> do
        if null $ tail tokens then
            (Variable Nothing (T.unpack str), tail tokens)
        else
            case head $ tail tokens of
              Symbol '[' -> do
                  let (indexExpr, rest) = takeIndexParens $ tail tokens
                  (Variable (Just $ parseExpr indexExpr) (T.unpack str), rest)
              Symbol '(' -> do
                  let (parameterTokens, rest) = takeArgumentList $ tail tokens
                  (Call Nothing (T.unpack str) (map parseExpr parameterTokens), rest)
              Symbol '.' -> do
                  let (subroutineToken, rest1) = splitAt 1 $ drop 2 tokens
                  let (parameterTokens, rest2) = takeArgumentList rest1
                  case subroutineToken of
                    [Identifier subroutineName] -> 
                        (Call (Just $ T.unpack str) (T.unpack subroutineName) (map parseExpr parameterTokens), rest2)
                    _ -> error "Expected subroutine name"
              _ -> (Variable Nothing (T.unpack str), tail tokens)
              
    _ -> error "Expected Expression"

runAnalyzer :: FilePath -> IO [Class]
runAnalyzer path = do
    isFile <- doesFileExist path
    isDir  <- doesDirectoryExist path
    if isFile then do
        contents <- TextIO.readFile path
        let fileTokens = filter (/= Whitespace) . tokens $ contents
        let fileClass = analyzeClass fileTokens
        return [fileClass]
    else if isDir then do
        files <- listDirectory path
        let jackFiles = map (\file -> path ++ "/" ++ file) . filter (\file -> ".jack" `isSuffixOf` file) $ files
        classes <- mapM runAnalyzer jackFiles
        return $ concat classes
    else error $ "File does not exist: " ++ path
