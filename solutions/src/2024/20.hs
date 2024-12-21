-- https://adventofcode.com/2024/day/20

{-# LANGUAGE ViewPatterns #-}
module Main (main) where

import AoC (getRawInput, count)
import AoC.Coord
import AoC.Search (dfs)
import Data.Array.Unboxed (UArray, accumArray, (!?))
import Data.Set qualified as S


main :: IO ()
main = do
  (path,pathArr) <- process <$> getRawInput 2024 20
  let cheats = [cheatLen |
                  (cheatLen,saved) <- findCheats path pathArr,
                  saved >= 100 ]
  print $ count 2 cheats
  print $ length cheats

process :: String -> ([Coord], UArray Coord Int)
process (from2dString -> input) = (path,pathArr)
 where
  clear = S.fromList [p | (p,x) <- input, x /= '#']
  start = head [p | (p,'S') <- input]
  next p = [p' | p' <- cardinal p, p' `S.member` clear]
  path = dfs next start
  bnds = (0, fst $ last input)
  pathArr = accumArray (\_ e -> e) (-1) bnds (zip path [0..])

findCheats :: [Coord] -> UArray Coord Int -> [(Int,Int)]
findCheats path pathArr =
  [(cheatLen,timeSaved) |
      (p1,c1) <- zip path [0..],
      p2 <- diamond 20 p1,
      let cheatLen = manhattan p1 p2,
      Just c2 <- [pathArr !? p2],
      let timeSaved = c2 - c1 - cheatLen,
      timeSaved > 0 ]
