-- https://adventofcode.com/2024/day/4

module Main (main) where
import AoC
import Data.List (transpose, tails, isPrefixOf, sort)

main :: IO ()
main = do
  inp <- lines <$> getRawInput 2024 4
  print $ part1 inp
  print $ part2 inp

part1,part2 :: [String] -> Int
part1 xss = fwd + bck + dwn + up + downRight + upRight + downLeft + upLeft
  where
    height = length xss
    width  = length $ head xss
    downRight = count "XMAS"
      [ [xss !! (i+x) !! (j+x) | x <- [0..3]]
      | i <- [0 .. height-4]
      , j <- [0 .. width-4]
      ]
    downLeft = count "XMAS"
      [ [xss !! (i+x) !! (j-x) | x <- [0..3]]
      | i <- [0 .. height-4]
      , j <- [3 .. width-1]
      ]
    upRight = count "XMAS"
      [ [xss !! (i-x) !! (j+x) | x <- [0..3]]
      | i <- [3 .. height-1]
      , j <- [0 .. width-4]
      ]
    upLeft = count "XMAS"
      [ [xss !! (i-x) !! (j-x) | x <- [0..3]]
      | i <- [3 .. height-1]
      , j <- [3 .. width-1]
      ]
    xmasCount = countIf ("XMAS" `isPrefixOf`) . tails
    fwd = sum [xmasCount row | row <- xss]
    bck = sum [xmasCount $ reverse row | row <- xss]
    dwn = sum [xmasCount row | row <- transpose xss]
    up  = sum [xmasCount $ reverse row | row <- transpose xss]
part2 xss = length
  [ ()
  | i <- [1 .. height-2]
  , j <- [1 .. width-2]
  , xss !! i !! j == 'A'
  , sort [xss !! (i-x) !! (j-x) | x <- [1,-1]] == "MS"
  , sort [xss !! (i+x) !! (j-x) | x <- [1,-1]] == "MS" ]
 where
  height = length xss
  width = length $ head xss