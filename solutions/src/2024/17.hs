module Main (main) where

import AoC.Parsec hiding (State)
import Data.List (intercalate)
import Data.Bits

inputP :: Parser (Int,Int,Int,[Int])
inputP = do
  a <- string "Register A: " *> int <* newline
  b <- string "Register B: " *> int <* newline
  c <- string "Register C: " *> int <* spaces
  prog <- string "Program: " *> sepBy1 nat (char ',')
  return (a,b,c,prog)

main :: IO ()
main = do
  (regA,regB,regC,prog) <- parseInput 2024 17 inputP
  putStrLn $ intercalate "," . map show $ run (Computer regA regB regC) prog
  print $ minimum $ foldr (concatMap . part2 prog) [0] prog

-- | After analyzing the input file, it was noticed that the output values are
--   generated via bit manipulations of each octet of the initial value of
--   register A. It is assumed that the given input will not modify A in any way
--   other than bit shifting right 3 places per output value.
part2 :: [Int] -> Int -> Int -> [Int]
part2 prog expected prefix =
  [ a | x <- [0..7]
      , let a = prefix `shiftL` 3 .|. x
      , head (run (Computer a 0 0) prog) == expected ]

data Computer = Computer {rA,rB,rC :: !Int}

run :: Computer -> [Int] -> [Int]
run comp prog = go comp prog
 where
  go c = \case
    0 : x : xs -> go c{rA = rA c `shiftR` combo x} xs
    1 : x : xs -> go c{rB = rB c `xor` x} xs
    2 : x : xs -> go c{rB = 7 .&. combo x} xs
    3 : x : xs -> go c (if rA c == 0 then xs else drop x prog)
    4 : _ : xs -> go c{rB = rB c `xor` rC c} xs
    5 : x : xs -> combo x .&. 7 : go c xs
    6 : x : xs -> go c{rB = rA c `shiftR` combo x} xs
    7 : x : xs -> go c{rC = rA c `shiftR` combo x} xs
    _ -> []
   where
    combo x = [0, 1, 2, 3, rA c, rB c, rC c] !! x
