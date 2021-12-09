module Main where
import SantasSack
import Data.Char (digitToInt)


f :: [String] -> Int -> Int
f (x:xs) n | digitToInt (x !! n) == 1 = 1 + f xs n
           | otherwise = 0 + f xs n

f [] _ = 0


p l x = (length l) `div` 2 > x

p' x = if x > 500 then '1' else '0'

main :: IO ()
main = do
  inp <- getInput 3
  let l = lines inp 
  let breakOffPoint = (length l) `div` 2
  -- let g = (\x -> p l (f x))
  -- let g = f l [0..12]

  let t = [f l x | x <- [0..11]]

  let gamma = [p' x | x <- t]

  let epsilon = map (\x -> if x == '1' then '0' else '1') gamma

  putStrLn $ show gamma
  putStrLn $ show epsilon

  -- print $ f l 1

  -- print breakOffPoint
  -- epsilon == complement gamma
