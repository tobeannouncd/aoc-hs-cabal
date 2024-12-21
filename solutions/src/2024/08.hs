-- https://adventofcode.com/2024/day/8

module Main (main) where

import AoC
import AoC.Coord (from2dString)
import qualified Data.Map.Strict as Map
import Data.Ix
import qualified Data.Set as Set

main :: IO ()
main = do
  input <- from2dString <$> getRawInput 2024 8
  let byFreq = Map.fromListWith (++) [(v,[k]) | (k,v) <- input, v /= '.']
      inGrid = inRange (fst $ head input, fst $ last input)
  print $ solveWith (part1 inGrid) byFreq
  print $ solveWith (part2 inGrid) byFreq

solveWith :: Ord a1 => (a2 -> [a1]) -> Map.Map k a2 -> Int
solveWith f = Set.size . Set.unions . map (Set.fromList . f) . Map.elems

part1,part2 :: Num a => (a -> Bool) -> [a] -> [a]
part1 inGrid pts =
  [ pt
  | (a,b) <- pairs pts
  , pt <- [2*a-b, 2*b-a]
  , inGrid pt ]
part2 inGrid pts =
  [ pt
  | (a,b) <- pairs pts
  , (start, dir) <- [(a,a-b),(b,b-a)]
  , pt <- takeWhile inGrid $ iterate (+ dir) start ]

pairs :: [a] -> [(a,a)]
pairs [] = []
pairs (x:xs) = [(x,y) | y <- xs] ++ pairs xs