module Main where

import AoC
import Data.List (inits, tails)
import Data.Ix

main :: IO ()
main = do
  inp <- map (map read . words) . lines <$> getRawInput 2024 2
  print $ countIf (safeBy pure) inp
  print $ countIf (safeBy holes) inp

holes :: [a] -> [[a]]
holes xs = [i ++ t | (i,_:t) <- zip (inits xs) (tails xs)]

safeBy :: ([Int] -> [[Int]]) -> [Int] -> Bool
safeBy f = any isSafe . f

isSafe :: [Int] -> Bool
isSafe = check . (zipWith (-) <*> drop 1)
 where
  check xs = any (\r -> all (inRange r) xs) [(-3,-1),(1,3)]