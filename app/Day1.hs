{-# LANGUAGE TypeApplications #-}

module Main where
import SantasSack

f :: [Int] -> [Bool]
f [] = []
f [x]       = []
f (x:y:xys) = (x < y) : f (y:xys)

f' []           = []
f' [x]          = []
f' [x, y]       = []
f' (x:y:z:xyzs) = x + y + z : f' (y:z:xyzs)



g :: (Num a1, Ord a2) => [a2] -> a1
g xs = sum $ zipWith (\a b -> if a < b then 1 else 0) xs (tail xs)

part1 :: String -> IO ()
part1 inp = do 
  let l = lines inp
  let r = map (read @Int) l
  let t = f r
  let t' = filter id t
  print $ length t'

part2 :: String -> IO ()
part2 inp = do
  let t = f $ f' $ map (read @Int) $ lines inp
  let t' = filter id t
  print $ length t'

main :: IO ()
main = do
  inp <- getInput 1
  -- part1 inp
  part2 inp
  