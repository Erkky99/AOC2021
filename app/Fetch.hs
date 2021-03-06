{-# LANGUAGE TypeApplications #-}
module Main where
import SantasSack
import System.Environment

main :: IO ()
main = do
  env <- getArgs
  day <- case env of
    [dayStr] -> return $ read @Integer dayStr
    _ -> error "No day submitted"
  putStrLn $ "Fetching day " <> show day <> "..."
  fetchDescription day
  fetchInput day
  putStrLn "Done!"