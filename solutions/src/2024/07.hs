module Main (main) where

import AoC.Parsec
import Data.List.Lens (stripSuffix)


inputP :: Parser [(Int, [Int])]
inputP = sepEndBy1 p newline
 where
  p = (,)
    <$> int <* string ": "
    <*> sepBy1 int (char ' ')

main :: IO ()
main = do
  equations <- parseInput 2024 7 inputP
  print $ sum [t | (t,eqn) <- equations, check part1 t eqn]
  print $ sum [t | (t,eqn) <- equations, check part2 t eqn]

check :: (Int -> Int -> [Int]) -> Int -> [Int] -> Bool
check f x xs = 0 `elem` foldr (concatMap . f) [x] xs

part1,part2 :: Int -> Int -> [Int]
part1 x y = [y-x | y >= x] ++ [q | (q,0) <- [y `quotRem` x]]
part2 x y = part1 x y
  ++ [read p | Just p@(_:_) <- [stripSuffix (show x) (show y)]]
