module Main where
import AoC
import Data.List (sort)


main :: IO ()
main = do
  (xs,ys) <- unzip . map parse . lines <$> getRawInput 2024 1
  print $ sum [abs (x-y) | (x,y) <- zip (sort xs) (sort ys)]
  print $ sum [x * count x ys | x <- xs]


parse :: String -> (Int, Int)
parse s =
  case map read . words $ s of
    [x,y] -> (x,y)
    _ -> error "no parse"