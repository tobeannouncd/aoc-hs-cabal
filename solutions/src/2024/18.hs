-- https://adventofcode.com/2024/day/18

module Main (main) where

import AoC.Parsec
import AoC.Coord
import Data.Ix
import AoC.Search
import qualified Data.Map as M

start, end :: Coord
start = 0
end = 70

inputP :: Parser Coord
inputP = flip C <$> nat <* char ',' <*> nat

main :: IO ()
main = do
  input <- parseInput 2024 18 (sepEndBy1 inputP newline)
  let corrupt = M.fromList $ zip input [0 :: Int ..]
      next pt =
        [ AStep pt' 1 (manhattan pt' end)
        | pt' <- cardinal pt, inRange (start,end) pt'
        , case corrupt M.!? pt' of
            Just i -> i >= 1024
            _      -> True
        ]
  print $ head [cost | (pt,cost) <- astar next start, pt == end]

  let next' n pt =
        [ pt'
        | pt' <- cardinal pt, inRange (start,end) pt'
        , case corrupt M.!? pt' of
            Just i -> i > n
            _      -> True
        ]
      nStop = head [n | n <- [1024..], end `notElem` dfs (next' n) start]
      C yS xS = input !! nStop
  putStrLn $ show xS ++ "," ++ show yS
