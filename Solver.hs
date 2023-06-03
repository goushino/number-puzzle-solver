module Solver
  ( Expr (..),
    parse,
    isNatural,
    mkAns,
    calculate,
  )
where

import Control.Applicative
import Data.Ratio
import Parser

data Expr
  = Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Lit Rational
  deriving (Eq, Show)

type Ans = (String, Rational)

eval :: Expr -> Rational
eval (Lit n) = n
eval (Add l r) = eval l + eval r
eval (Sub l r) = eval l - eval r
eval (Mul l r) = eval l * eval r
eval (Div l r) = eval l / eval r

isNatural :: Rational -> Bool
isNatural = (== 1) . denominator

operators :: [String]
operators = ["+", "-", "*", "/"]

permute :: Int -> [a] -> [[a]]
permute 0 _ = [[]]
permute n xs = (:) <$> xs <*> permute (n - 1) xs

insList :: [a] -> [a] -> [a]
insList [] ys = ys
insList xs [] = xs
insList (x : xs) (y : ys) = x : y : insList xs ys

mkExprStr :: [String] -> [String]
mkExprStr xs = map f ops
  where
    f = concat . insList xs
    ops = permute (length xs - 1) operators

mkAns :: [String] -> Maybe [Ans]
mkAns xs = zip <$> Just ys <*> sequence zs
  where
    ys = mkExprStr xs
    zs = map calculate (mkExprStr xs)

literal :: Parser String Expr
literal = Lit . toRational . read <$> many1 digit

add :: Parser String (Expr -> Expr)
add = flip Add <$> (char '+' *> term)

sub :: Parser String (Expr -> Expr)
sub = flip Sub <$> (char '-' *> term)

mul :: Parser String (Expr -> Expr)
mul = flip Mul <$> (char '*' *> literal)

divide :: Parser String (Expr -> Expr)
divide = flip Div <$> (char '/' *> literal)

term :: Parser String Expr
term = leftJoin <$> literal <*> many0 (mul <|> divide)

expr :: Parser String Expr
expr = leftJoin <$> term <*> many0 (add <|> sub)

leftJoin :: a -> [a -> a] -> a
leftJoin = foldl (flip ($))

parse :: String -> Maybe Expr
parse = fmap fst . runParser (expr <* eos)

calculate :: String -> Maybe Rational
calculate = fmap eval . parse
