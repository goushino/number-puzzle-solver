module Parser
( Parser (..)
, char
, digit
, many0
, many1
, eos
)
where

import Control.Applicative
import Data.Char
import Data.Bifunctor

newtype Parser s a = Parser { runParser :: s -> Maybe (a, s) }

instance Functor (Parser s) where
  fmap f (Parser p) = Parser $ \s -> fmap (first f) (p s)

instance Applicative (Parser s) where
  pure a = Parser $ \s -> Just (a, s)
  Parser f <*> Parser g =
    Parser $ \s -> case f s of
      Nothing -> Nothing
      Just (a, s') -> fmap (first a) (g s')

instance Alternative (Parser s) where
  empty = Parser $ const Nothing
  Parser f <|> Parser g = Parser $ \s -> f s <|> g s

predHead :: (a -> Bool) -> Parser [a] a
predHead p = Parser $ \s ->
  if not (null s) && p (head s)
    then Just (head s, tail s)
    else Nothing

char :: Char -> Parser String Char
char c = predHead (==c)

digit :: Parser String Char
digit = predHead isDigit

many1 :: Parser s a -> Parser s [a]
many1 = Control.Applicative.some

many0 :: Parser s a -> Parser s [a]
many0 = Control.Applicative.many

eos :: Parser String ()
eos = Parser $ \s -> if null s then Just ((), "") else Nothing
