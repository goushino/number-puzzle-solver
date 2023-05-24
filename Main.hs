module Main where

import Solver
import Data.Ratio
import Data.List (sort)
import Data.Tuple (swap)
import Data.Bifunctor

main :: IO ()
main = do
  putStrLn "Input numbers"
  s <- words <$> getLine
  case mkAns s of
    Nothing -> return ()
    Just xs -> do
      let xs' = filter (isNatural . snd) xs
      print . sort $ map (first numerator . swap) xs'

