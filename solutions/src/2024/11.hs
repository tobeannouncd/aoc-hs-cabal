module Main (main) where

import Data.IntMap qualified as Map
import AoC

main :: IO ()
main = do
  stones <- Map.fromListWith (+) . map ((,1) . read) . words <$> getRawInput 2024 11
  let blinks = map sum $ iterate blink stones
  print $ blinks !! 25
  print $ blinks !! 75

blink :: Map.IntMap Int -> Map.IntMap Int
blink = Map.foldrWithKey f Map.empty
 where
  f s c m = foldr (\s' -> Map.insertWith (+) s' c) m $ step s

step :: Int -> [Int]
step 0 = [1]
step n
  | even len = map read [take k s, drop k s]
  | otherwise = [n * 2024]
 where
  s = show n
  len = length s
  k = len `div` 2