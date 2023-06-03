module Main where

import           Data.Bifunctor
import           Data.List      (sort)
import           Data.Ratio
import           Data.Tuple     (swap)
import           Solver

main :: IO ()
main = do
  putStrLn "Input numbers"
  s <- words <$> getLine
  case mkAns s of
    Nothing -> return ()
    Just xs -> do
      let xs' =  map swap $ filter (isNatural . snd) xs
      printResult $ sort xs'

printResult :: [(Rational, String)] -> IO ()
printResult [] = return ()
printResult ((r, s):xs) = do
  let r' = show . numerator $ r
  putStrLn $ r' ++ " = " ++ s
  printResult xs
