-- https://adventofcode.com/2024/day/6

module Main (main) where

import AoC.Coord
import Data.Containers.ListUtils (nubOrd)
import Data.Map (Map, (!?))
import Data.Map.Strict qualified as Map
import AoC

main :: IO ()
main = do
  (guardPos, facing, tileMap) <- process . from2dString <$> getRawInput 2024 6
  let guardPath = nubOrd . map fst $ walk tileMap start
      start = (guardPos, facing)
      check pt = isLoop $ walk (Map.insert pt Obstacle tileMap) start
  print $ length guardPath
  print $ countIf check (tail guardPath)

walk :: Map Coord Tile -> (Coord, Coord) -> [(Coord, Coord)]
walk grid = uncurry go
 where
  go pos dir = (pos, dir) :
    case grid !? (pos + dir) of
      Nothing   -> []
      Just Open -> go (pos + dir) dir
      _         -> go pos (clockwise dir)

-- | Tortoise & hare loop detection
isLoop :: Eq a => [a] -> Bool
isLoop lst = go lst lst
 where
  go (x:xs) (_:y:ys) = x == y || go xs ys
  go _ _ = False

data Tile
  = Obstacle
  | Open
  deriving (Eq, Show)

process :: [(Coord, Char)] -> (Coord, Coord, Map Coord Tile)
process xs = (guardPos, facing, tileMap)
 where
  (guardPos, guardChar) = head [t | t <- xs, snd t `elem` "^v<>"]
  facing =
    case guardChar of
      '>' -> east
      '<' -> west
      '^' -> north
      'v' -> south
      _   -> error "no match"
  tileMap =
    Map.fromList
      [ (c, if v == '#' then Obstacle else Open)
      | (c, v) <- xs
      ]