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

{-
>>> import Data.List (group, sort)
>>> byTime' xs = [(length g,head g) | g <- group $ sort xs]
>>> byTime xs = error (unlines . map show $ byTime' xs) :: IO ()
>>> (p,pa) = process example
>>> cheats = findCheats p pa
>>> part1 = [t | (2,t) <- cheats]
>>> byTime part1
(14,2)
(14,4)
(2,6)
(4,8)
(2,10)
(3,12)
(1,20)
(1,36)
(1,38)
(1,40)
(1,64)
>>> part2 = [t | (_,t) <- cheats, t >= 50]
>>> byTime part2
(32,50)
(31,52)
(29,54)
(39,56)
(25,58)
(23,60)
(20,62)
(19,64)
(12,66)
(14,68)
(12,70)
(22,72)
(4,74)
(3,76)
-}
findCheats :: [Coord] -> UArray Coord Int -> [(Int,Int)]
findCheats path pathArr =
  [(cheatLen,timeSaved) |
      (p1,c1) <- zip path [0..],
      p2 <- diamond 20 p1,
      let cheatLen = manhattan p1 p2,
      Just c2 <- [pathArr !? p2],
      let timeSaved = c2 - c1 - cheatLen,
      timeSaved > 0 ]

example :: String
example =
  "###############\n\
  \#...#...#.....#\n\
  \#.#.#.#.#.###.#\n\
  \#S#...#.#.#...#\n\
  \#######.#.#.###\n\
  \#######.#.#...#\n\
  \#######.#.###.#\n\
  \###..E#...#...#\n\
  \###.#######.###\n\
  \#...###...#...#\n\
  \#.#####.#.###.#\n\
  \#.#...#.#.#...#\n\
  \#.#.#.#.#.#.###\n\
  \#...#...#...###\n\
  \###############"
