-- https://adventofcode.com/2024/day/12

module Main (main) where

import qualified Data.Map as Map
import AoC.Coord
import Data.Map (Map, (!?))
import qualified Data.Set as Set
import AoC.Search (dfs)
import Data.List ((\\))
import Control.Monad (guard, void)
import AoC

main :: IO ()
main = do
  garden <- Map.fromList . from2dString <$> getRawInput 2024 12
  let regions = findRegions garden
  print $ sum $ map ((*) <$> length <*> perimeter) regions
  print $ sum $ map ((*) <$> length <*> sides) regions

findRegions :: Map Coord Char -> [[Coord]]
findRegions garden = go Set.empty (Map.keys garden)
 where
  go _ [] = []
  go seen (start:rest) =
    let region = dfs next start
        seen'  = foldr Set.insert seen region
        rest'  = rest \\ region
    in region : go seen' rest'
  next here =
    [ there
    | there <- cardinal here
    , garden !? here == garden !? there ]

perimeter :: [Coord] -> Int
perimeter region = countIf (`notElem` region) $ concatMap cardinal region

sides :: [Coord] -> Int
sides region = sum $ map (length . corners) region
 where
  corners pt = do
    (a,b) <- [(above,left),(left,below),(below,right),(right,above)]
    let edges = [a pt, b pt]
    void . guard $ if a (b pt) `elem` region
      then all (`notElem` region) edges
      else (a pt `elem` region) == (b pt `elem` region)