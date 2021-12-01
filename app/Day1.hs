{-# LANGUAGE TypeApplications #-}

module Main where
import SantasSack

f :: [Int] -> [Bool]
f [] = []
f [x]       = []
f (x:y:xys) = (x < y) : f (y:xys)

f' :: (Num a1, Ord a2) => [a2] -> a1
f' xs = sum $ zipWith (\a b -> if a < b then 1 else 0) xs (tail xs)

main :: IO ()
main = do
  inp <- getInput 1
  --inp <- readFile "input/day1_sub.input"
  let l = lines inp
  let r = map (read @Int) l
  let t = f r
  let t' = filter id t
  --print t'
  print $ length t'