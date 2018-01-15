module Expr where

data Value = I Int | B Bool deriving (Eq, Show)
data BinOp = Plus | Mul | Minus | Less | Greater | Equals deriving (Eq, Show)
data UnOp = Neg | Not deriving (Eq, Show)
data Expr = BinOp BinOp Expr Expr | UnOp UnOp Expr | Const Value | Var String deriving (Eq, Show)
data Statement = Assign String Expr | While Expr Statement | If Expr Statement (Maybe Statement) | Compound [Statement] deriving (Eq, Show)

infixr 0 $=
infixl 4 .<, .>, .==
infixl 6 .+, .-
infixl 7 .*

($=) = Assign
(.+) = BinOp Plus
(.-) = BinOp Minus
(.*) = BinOp Mul
(.<) = BinOp Less
(.>) = BinOp Greater
(.==) = BinOp Equals
int = Const . I
bool = Const . B
neg = UnOp Neg
