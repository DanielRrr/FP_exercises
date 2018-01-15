-- Список экспорта менять нельзя!
module Parser
    ( Parser
    , pure, (<$>), (<$), (<*>), (<*), (*>)
    , empty, (<|>)
    , satisfy, eof
    , parse
    , parserTestOK
    , parserTestFail
    ) where

import Control.Applicative
import Test.HUnit
import Data.Maybe(isNothing)

newtype Parser lex a = Parser { runParser :: [lex] -> Maybe ([lex],a) }

-- Если парсер p не поглащает весь список, то parse должен возвращать Nothing
parse :: Parser lex a -> [lex] -> Maybe a
parse = undefined

satisfy :: (lex -> Bool) -> Parser lex lex
satisfy = undefined

eof :: Parser lex ()
eof = undefined

instance Functor (Parser lex) where
    fmap f = Parser . fmap (fmap (fmap f)) . runParser

instance Applicative (Parser lex) where
    pure = undefined
    Parser f <*> Parser a = Parser $ \lexs -> case f lexs of
        Nothing -> Nothing
        Just (lexs', f') -> _ -- fmap (fmap f') (a lexs')

instance Alternative (Parser lex) where
    empty = undefined
    (<|>) = undefined

parserTestOK :: (Eq a, Show a, Eq lex, Show lex) => Parser lex a -> [lex] -> ([lex],a) -> Test
parserTestOK (Parser p) s r = p s ~?= pure r

parserTestFail :: (Eq a, Show a) => Parser lex a -> [lex] -> Test
parserTestFail (Parser p) s = TestCase $ assertBool "Parser should fail" $ isNothing (p s)
