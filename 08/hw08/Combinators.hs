module Combinators
    ( module Parser
    , many, many1
    , symbol, anySymbol, string, oneOf
    , digit, natural, integer
    , spaces
    , try
    , endBy, endBy1
    , sepBy, sepBy1
    , foldr1P, foldl1P
    , between, brackets, parens, braces, angles
    ) where

import Parser
import Data.Char

-- Поведение всех комбинаторов описано в тестах в Main.hs.

symbol :: lex -> Parser lex ()
symbol = undefined

anySymbol :: Parser lex lex
anySymbol = undefined

digit :: Parser Char Int
digit = undefined

string :: [lex] -> Parser lex ()
string = undefined

oneOf :: [lex] -> Parser lex lex
oneOf = undefined

many :: Parser lex a -> Parser lex [a]
many = undefined

many1 :: Parser lex a -> Parser lex [a]
many1 = undefined

natural :: Parser Char Integer
natural = undefined

integer :: Parser Char Integer
integer = undefined

spaces :: Parser Char ()
spaces = undefined

try :: Parser lex a -> Parser lex (Maybe a)
try = undefined

endBy :: Parser lex a -> Parser lex b -> Parser lex [a]
endBy = undefined

endBy1 :: Parser lex a -> Parser lex b -> Parser lex [a]
endBy1 = undefined

sepBy :: Parser lex a -> Parser lex b -> Parser lex [a]
sepBy = undefined

sepBy1 :: Parser lex a -> Parser lex b -> Parser lex [a]
sepBy1 = undefined

between :: Parser lex a -> Parser lex b -> Parser lex c -> Parser lex c
between = undefined

brackets :: Parser lex a -> Parser lex a
brackets = undefined

parens :: Parser lex a -> Parser lex a
parens = undefined

braces :: Parser lex a -> Parser lex a
braces = undefined

angles :: Parser lex a -> Parser lex a
angles = undefined

foldr1P :: (a -> b -> a -> a) -> Parser lex a -> Parser lex b -> Parser lex a
foldr1P = undefined

foldl1P :: (a -> b -> a -> a) -> Parser lex a -> Parser lex b -> Parser lex a
foldl1P = undefined
