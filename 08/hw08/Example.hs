module Example where

-- Это пример решения задания, аналогичного заданию из Expr.hs

import Combinators
import Test.HUnit
import Data.Char

data Expr = Plus Expr Expr | Mul Expr Expr | Val Int deriving (Eq,Show)

--------------------------------------------------
-- Лексический анализ

data Lex = PLUS | MUL | LP | RP | Nat Int deriving (Eq,Show)

lexer :: String -> [Lex]
lexer "" = []
lexer (x:xs) | isSpace x = lexer xs
lexer xs@(x:_) | isDigit x =
    let (number, rest) = span isDigit xs
    in Nat (read number) : lexer rest
lexer ('+':xs) = PLUS : lexer xs
lexer ('*':xs) = MUL : lexer xs
lexer ('(':xs) = LP : lexer xs
lexer (')':xs) = RP : lexer xs
lexer (x:xs) = lexer xs

--------------------------------------------------
-- Синтаксический анализ

pAtom :: Parser Lex Expr
pAtom = fmap (\(Nat x) -> Val x) (satisfy isNat) <|>
       satisfy (== LP) *> pExpr <* satisfy (== RP)
  where
    isNat :: Lex -> Bool
    isNat Nat{} = True
    isNat _ = False

pMul :: Parser Lex Expr
pMul = foldl Mul <$> pAtom <*> many (satisfy (== MUL) *> pAtom)

pExpr :: Parser Lex Expr
pExpr = foldl Plus <$> pMul <*> many (satisfy (== PLUS) *> pMul)

main = do
    print $ parse pExpr $ lexer "1 + 2 * 3 + 4"
    print $ parse pExpr $ lexer "1 * 2 + 3 * 4"
    print $ parse pExpr $ lexer "(1 + 2) * (3 + 4)"
