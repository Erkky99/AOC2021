{-# LANGUAGE TypeApplications #-}
module Main where
import SantasSack

import Data.List.Split

f :: [String] -> [[Int]]
f [] = []
f (r:rs) -- | r == "" = []
         -- | otherwise 
         = f' (words r) : f rs
  where f' = map (read @Int)

main :: IO ()
main = do
  inp <- getInput 4
  let (numbers : boards) = lines inp

  let n = map (read @Int) $ splitOn "," numbers

  print n
  -- print $ boards !! 1

  let bingoBoards = chunksOf 5 $ f $ filter (not . null) $ tail boards

  print $ bingoBoards !! 0

  putStrLn ""  
