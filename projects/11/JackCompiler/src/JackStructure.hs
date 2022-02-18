module JackStructure where

import Data.Text (Text)


data Class = Class Text [ClassVarDec] [Subroutine]
           deriving (Eq)

data ClassVarDec = Static VarType Identifier
                 | Field  VarType Identifier
                 deriving (Show, Eq)

data Subroutine = Constructor VarType Identifier ParameterList Body
                | Function VarType Identifier ParameterList Body
                | Method VarType Identifier ParameterList Body
                deriving (Eq)

newtype ParameterList = ParameterList [VarDec]
                      deriving (Show, Eq)

data Body = Body [VarDec] [Statement]
          deriving (Eq)

data VarDec = VarDec VarType Identifier
            deriving (Show, Eq)

data Statement = Let Identifier Index Expr
               | If Expr [Statement] [Statement]
               | While Expr [Statement]
               | Do Expr
               | Return (Maybe Expr)
               deriving (Eq)

data Expr = IntConst Int
          | CharConst Char
          | BoolConst Bool
          | StrConst Text 
          | Variable Index Identifier 
          | Call (Maybe Identifier) Identifier [Expr]
          | Unary Char Expr
          | Binary Char Expr Expr
          | This
          | Null
          deriving (Eq)

data VarType = Void
             | Int 
             | Char
             | String
             | Custom Text
             deriving (Show, Eq)

type Identifier = Text
type Index = Maybe Expr


--  DELETE: Remove show definitions
instance Show Class where
    show (Class name varDecs subroutines) = 
        "Class " ++ show name ++ "\n" ++
        concatMap (\x -> show x ++ "\n") varDecs ++ "\n" ++
        concatMap (\x -> show x ++ "\n\n") subroutines

instance Show Subroutine where
    show (Constructor varType name (ParameterList params) body) =
        "Constructor " ++ show varType ++ " " ++ show name ++ " (" ++ 
        concatMap (\x -> show x ++ " ") params ++ ")\n" ++
        show body
    show (Function varType name (ParameterList params) body) =
        "Function " ++ show varType ++ " " ++ show name ++ " (" ++ 
        concatMap (\x -> show x ++ " ") params ++ ")\n" ++
        show body
    show (Method varType name (ParameterList params) body) =
        "Method " ++ show varType ++ " " ++ show name ++ " (" ++ 
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
    show (Variable Nothing name)     = show name
    show (Variable (Just expr) name) = show name ++ "[" ++ show expr ++ "]"
    show (Call Nothing name args)    = show name ++ "(" ++ show args ++ ")"
    show (Call (Just cls) name args) = show cls ++ "." ++ show name ++ "(" ++ show args ++ ")"
    show This = "this"
    show Null = "null"

instance Show Statement where
    show (Let name Nothing expr) = "Let " ++ show name ++ " = " ++ show expr
    show (Let name (Just indexExpr) expr) = "Let " ++ show name ++ "[" ++ show indexExpr ++ "] = " ++ show expr
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

isUnaryOP :: Char -> Bool
isUnaryOP = flip elem ['~', '-']

isBinaryOP :: Char -> Bool
isBinaryOP = flip elem ['+', '-', '*', '/', '&', '|', '<', '>', '=']
