module Main where
import SantasSack


day1 :: String -> (Int, Int)
day1 s = do
  let s' = lines s
  f s' (0, 0)

day2 :: String -> (Int, Int, Int)
day2 s = do
  let s' = lines s
  f' s' (0, 0, 0)

f :: [String] -> (Int, Int) -> (Int, Int) -- (pos, depth)
f [] t = t
f (x:xs) (p,d) 
  | head (words x) == "down"    = f xs (p, d + (read (last(words x)) :: Int) )
  | head (words x) == "up"      = f xs (p, d - (read (last(words x)) :: Int) )
  | head (words x) == "forward" = f xs (p + (read (last(words x)) :: Int), d)

f' :: [String] -> (Int, Int, Int) -> (Int, Int, Int) -- (pos, depth, aim)
f' [] t = t
f' (x : xs) (p, d, a)
  | head (words x) == "down"    = f' xs (p, d, a + x')
  | head (words x) == "up"      = f' xs (p, d, a - x')
  | head (words x) == "forward" = f' xs (p + x', d + (x' * a), a)
    where x' = read (last (words x)) :: Int

main :: IO ()
main = do
  inp <- getInput 2
  -- putStrLn $ show $ uncurry (*) (day1 inp)
  let p = fst' (day2 inp)
  let d = snd' (day2 inp)
  putStrLn $ show $ p * d
    where fst' (p, d, a) = p
          snd' (p, d, a) = d
