module Expr where

import Combinators
import Test.HUnit
import Data.Char

data Value = I Int | B Bool deriving (Eq, Show)
data BinOp = Plus | Mul | Minus | Less | Greater | Equals deriving (Eq, Show)
data UnOp = Neg | Not deriving (Eq, Show)
data Expr = BinOp BinOp Expr Expr | UnOp UnOp Expr | Const Value | Var String deriving (Eq, Show)
data Statement = Assign String Expr | While Expr Statement | If Expr Statement (Maybe Statement) | Compound [Statement] deriving (Eq, Show)

infixr 0 $=
($=) = Assign
(.+) = BinOp Plus
(.-) = BinOp Minus
(.*) = BinOp Mul
(.<) = BinOp Less
(.>) = BinOp Greater
int = Const . I
bool = Const . B
neg = UnOp Neg

--------------------------------------------------
-- Лексический анализ

data Lexeme
    = Ident String
    | NumberConst Int | BoolConst Bool
    | BinOpSign BinOp
    | NotKeyword | WhileKeyword | IfKeyword | ThenKeyword | ElseKeyword | AssignmentSign | SemicolonSign
    | LPSign | RPSign | LBSign | RBSign
    | UnknownLexeme String
    deriving (Eq, Show)

lexer :: String -> [Lexeme]
lexer "" = []
lexer (x:xs) | isSpace x = lexer xs
lexer xs@(x:_) | isAlpha x || x == '_' =
    let (ident, rest) = span (\x -> isAlphaNum x || x == '_') xs
    in case ident of
        "true" -> BoolConst True : lexer rest
        "false" -> BoolConst False : lexer rest
        "while" -> WhileKeyword : lexer rest
        "if" -> IfKeyword : lexer rest
        "then" -> ThenKeyword : lexer rest
        "else" -> ElseKeyword : lexer rest
        "not" -> NotKeyword : lexer rest
        _ -> Ident ident : lexer rest
lexer xs@(x:_) | isDigit x =
    let (number, rest) = span isDigit xs
    in NumberConst (read number) : lexer rest
lexer xs@(x:_) | isOpSymbol x =
    let (op, rest) = span isOpSymbol xs
    in case op of
        "+" -> BinOpSign Plus : lexer rest
        "*" -> BinOpSign Mul : lexer rest
        "-" -> BinOpSign Minus : lexer rest
        "<" -> BinOpSign Less : lexer rest
        ">" -> BinOpSign Greater : lexer rest
        "==" -> BinOpSign Equals : lexer rest
        ":=" -> AssignmentSign : lexer rest
        ";" -> SemicolonSign : lexer rest
        _ -> UnknownLexeme op : lexer rest
lexer ('(':xs) = LPSign : lexer xs
lexer (')':xs) = RPSign : lexer xs
lexer ('{':xs) = LBSign : lexer xs
lexer ('}':xs) = RBSign : lexer xs
lexer (x:xs) =
    let (lex, rest) = span (not . isValid) xs
    in UnknownLexeme (x:lex) : lexer rest

getErrors :: [Lexeme] -> ([Lexeme], [String])
getErrors [] = ([], [])
getErrors (UnknownLexeme e : ls) =
    let (rs, es) = getErrors ls
    in (rs, e:es)
getErrors (l:ls) =
    let (rs, es) = getErrors ls
    in (l:rs, es)

isOpSymbol :: Char -> Bool
isOpSymbol c = elem c "~!@#$%^&*-+=<>?/:;"

isValid :: Char -> Bool
isValid c = isOpSymbol c || isAlphaNum c || isSpace c || elem c "(){}"

--------------------------------------------------
-- Синтаксический анализ

pValue :: Parser Lexeme Value
pValue = pBool <|> pInt

pExpr :: Parser Lexeme Expr
pExpr = undefined

pStatement :: Parser Lexeme Statement
pStatement = undefined

-- tests

testsValue = [ parserTestOK pValue (lexer "123")    === (I 123, [])
             , parserTestOK pValue (lexer "-123")   === (I (-123), [])
             , parserTestOK pValue (lexer "true")   === (B True, [])
             , parserTestOK pValue (lexer "false")  === (B False, [])
             ]

testsExpr = [ parserTestOK pExpr (lexer "123 * x")          === (int 123 .* Var "x", [])
            , parserTestOK pExpr (lexer "123 + x * 5")      === (int 123 .+ (Var "x" .* int 5), [])
            , parserTestOK pExpr (lexer "3 > 123 + x * 5")  === (int 3 .> (int 123 .+ (Var "x" .* int 5)), [])
            ]

testsStatement = [ parserTestOK pStatement (lexer "x := 3;")                                                    === ("x" $= int 3, [])
                 , parserTestOK pStatement (lexer "if x > 0 then y := x else y := -x")                          === (If (Var "x" .> int 0) ("y" $= Var "x") (Just $ "y" $= neg (Var "x")), [])
                 , parserTestOK pStatement (lexer "if x > 0 then y := x")                                       === (If (Var "x" .> int 0) ("y" $= Var "x") Nothing, [])
                 , parserTestOK pStatement (lexer "while (x > 0) x := x - 1;")                                  === (While (Var "x" .> int 0) $ "x" $= Var "x" .- int 1, [])
                 , parserTestOK pStatement (lexer "r := 1; i := 0; while (i < n) { i := i + 1; r := r * i; }")  === (fac, [])
                 ]
  where
    fac = Compound
        [ "r" $= int 1
        , "i" $= int 0
        , While (Var "i" .< Var "n") $ Compound
            [ "i" $= Var "i" .+ int 1
            , "r" $= Var "r" .* Var "i"
            ]
        ]

a === (r,s) = a (s,r)

main = fmap (const ()) $ runTestTT $ test
    $  label "Value"     testsValue
    ++ label "Expr"      testsExpr
    ++ label "Statement" testsStatement
  where
    label :: String -> [Test] -> [Test]
    label l = map (\(i,t) -> TestLabel (l ++ " [" ++ show i ++ "]") t) . zip [1..]
