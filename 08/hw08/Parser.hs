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

parse :: Parser lex a -> [lex] -> Maybe a
parse p xs = snd <$> runParser p xs

satisfy :: (lex -> Bool) -> Parser lex lex
satisfy p = Parser checkHead where
  checkHead [] = Nothing
  checkHead (x:xs) = case p x of
    False -> Nothing
    True -> Just (xs,x)

eof :: Parser lex ()
eof = Parser checkUnit where
  checkUnit x = case x of [] -> Just ([],())
                          _  -> Nothing

instance Functor (Parser lex) where
    fmap f = Parser . fmap (fmap (fmap f)) . runParser

instance Applicative (Parser lex) where
    -- a -> Parser lex a
    pure x = Parser $ (\lexes -> Just (lexes,x))
    -- Parser lex (a -> b) -> Parser lex a -> Parser lex b
    Parser f <*> Parser a = Parser $ \lexs -> case f lexs of
        Nothing -> Nothing
        Just (lexs', f') -> case a lexs' of
                              Nothing -> Nothing
                              Just (lexs'', a') -> Just (lexs'', f' a')

instance Alternative (Parser lex) where
    -- empty :: Parser lex a
    empty = Parser $ const Nothing
    -- (<|>) :: Parser lex a -> Parser lex a -> Parser lex a
    Parser p1 <|> Parser p2 = Parser $ \lexes ->
                                case p1 lexes of
                                  Nothing -> p2 lexes
                                  r -> r

parserTestOK :: (Eq a, Show a, Eq lex, Show lex) => Parser lex a -> [lex] -> ([lex],a) -> Test
parserTestOK (Parser p) s r = p s ~?= pure r

parserTestFail :: (Eq a, Show a) => Parser lex a -> [lex] -> Test
parserTestFail (Parser p) s = TestCase $ assertBool "Parser should fail" $ isNothing (p s)
