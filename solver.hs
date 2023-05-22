import Data.Ratio
import Data.Char
import Data.List (sort)
import Data.Tuple (swap)
import Control.Applicative
import Data.Bifunctor ( Bifunctor(first) )

data Expr = Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Lit Rational deriving ( Eq, Show )

type Ans = (String, Rational)

eval :: Expr -> Rational
eval (Lit n) = n
eval (Add l r) = eval l + eval r
eval (Sub l r) = eval l - eval r
eval (Mul l r) = eval l * eval r
eval (Div l r) = eval l / eval r

isNatural :: Rational -> Bool
isNatural = (==1) . denominator

operators :: [String]
operators = ["+","-","*","/"]

permute :: Int -> [a] -> [[a]]
permute 0 _ = [[]]
permute n xs = (:) <$> xs <*> permute (n - 1) xs

insList :: [a] -> [a] -> [a]
insList [] ys = ys
insList xs [] = xs
insList (x:xs) (y:ys) = x:y:insList xs ys

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

add :: Parser String Expr
add = Add <$> term <*> (char '+' *> expr)

sub :: Parser String Expr
sub = Sub <$> term <*> (char '-' *> expr)

mul :: Parser String Expr
mul = Mul <$> literal <*> (char '*' *> term)

divide :: Parser String Expr
divide = Div <$> literal <*> (char '/' *> term)

term :: Parser String Expr
term = mul <|> divide <|> literal

expr :: Parser String Expr
expr = add <|> sub <|> term

parse :: String -> Maybe Expr
parse = fmap fst . runParser (expr <* eos)

calculate :: String -> Maybe Rational
calculate = fmap eval . parse

main :: IO ()
main = do
  putStrLn "Input numbers"
  s <- words <$> getLine
  case mkAns s of
    Nothing -> return ()
    Just xs -> do
      let xs' = filter (isNatural . snd) xs
      print . sort $ map (first numerator . swap) xs'

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

many :: Parser s a -> Parser s [a]
many = Control.Applicative.many

eos :: Parser String ()
eos = Parser $ \s -> if null s then Just ((), "") else Nothing
