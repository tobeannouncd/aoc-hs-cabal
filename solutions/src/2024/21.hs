-- https://adventofcode.com/2024/day/21

module Main (main) where

import AoC
import AoC.Coord
import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromJust)
import Data.MemoTrie (memo2)

numericStr :: String
numericStr =
  "789\n\
  \456\n\
  \123\n\
  \ 0A"

directionStr :: String
directionStr =
  " ^A\n\
  \<v>"

numPad :: Map Coord Char
numPad = M.fromList [(pt, c) | (pt, c) <- from2dString numericStr, c /= ' ']

dirPad :: Map Coord Char
dirPad = M.fromList [(pt, c) | (pt, c) <- from2dString directionStr, c /= ' ']

main :: IO ()
main = do
  codes <- lines <$> getRawInput 2024 21
  print $ solve 2 codes
  print $ solve 25 codes

solve :: Int -> [String] -> Int
solve depth = sum . map complexity
 where
  complexity code = read (init code) * minimum options
   where
    options = shortest depth 0 <$> sequences numPad code (C 3 2)

sequences :: Map Coord Char -> String -> Coord -> [String]
sequences pad = ways
 where
  ways "" _ = [""]
  ways (x : xs) start =
    [ front ++ back
    | let end = head [pt | (pt, c) <- M.toList pad, x == c]
    , front <- travel start end
    , back <- ways xs end
    ]
  travel start end
    | start == end = ["A"]
    | otherwise =
        [ fromVec v : rest
        | let C dy dx = signum (end - start)
        , v <- [C dy 0, C 0 dx]
        , v /= 0
        , let next = start + v
        , next `M.member` pad
        , rest <- travel next end
        ]
  fromVec v = fromJust $ lookup v $ zip [north, south, west, east] "^v<>"

shortest :: Int -> Int -> String -> Int
shortest maxDepth = dp
 where
  dp = memo2 \depth code ->
    let f part = minimum . map (dp $ depth + 1) $ sequences dirPad (part ++ "A") (C 0 2)
     in if depth == maxDepth
          then length code
          else sum . map f $ splitOn "A" $ init code