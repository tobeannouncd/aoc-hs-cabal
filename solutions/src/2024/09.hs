module Main (main) where

import AoC
import Data.Char
import Data.Maybe (listToMaybe)
import Data.Array.Unboxed (UArray, bounds, (!), accumArray)
import qualified Data.IntMap as IntMap

main :: IO ()
main = do
  input <- map digitToInt . head . lines <$> getRawInput 2024 9
  let (files, free) = decode input
  print $ part1 (expand (sum input) files)
  print $ part2 files free


-- | converts the input list into a representation of the files and free space
--   in the form of @([(start, fileId, size)], [(start, size)])@
--
--   Both lists are in reversed order from the disk order.
decode :: [Int] -> ([(Int,Int,Int)], [(Int,Int)])
decode = goFile [] [] 0 0
 where
  goFile files free i pos = \case
    []   -> (files, free)
    x:xs -> goFree ((pos,i,x):files) free (succ i) (pos + x) xs
  goFree files free i pos = \case
    []   -> (files, free)
    0:xs -> goFile files free i pos xs
    x:xs -> goFile files ((pos,x):free) i (pos+x) xs

expand :: Int -> [(Int,Int,Int)] -> UArray Int Int
expand end files =
  accumArray (\_ x -> x) (-1) (0,end)
    [ (pos, fileID)
    | (pos0, fileID, size) <- files
    , pos <- [pos0 .. pos0 + size - 1]]

-- | Works from both ends of the disk to calculate the checksum on-the-fly
part1 :: UArray Int Int -> Int
part1 disk = sum $ uncurry go (bounds disk)
 where
  go i j
    | i > j     = []
    | l >= 0    = (i * l) : go (i+1) j
    | r >= 0    = (i * r) : go (i+1) (j-1)
    | otherwise = go i (j-1)
   where
    l = disk ! i
    r = disk ! j

part2 :: [(Int,Int,Int)] -> [(Int,Int)] -> Int
part2 files freeSpace = fst $ foldl move (0, IntMap.fromList freeSpace) files
 where
  move (acc, free) (pos, fileId, fileSize) =
    let free1 = IntMap.takeWhileAntitone (< pos) free in
      case findFree fileSize free1 of
        Nothing            -> (acc + checksum' pos  fileId fileSize, free1)
        Just (pos', free2) -> (acc + checksum' pos' fileId fileSize, free2)

  findFree fileSize free = listToMaybe
    [ (pos, free2)
    | (pos, freeSize) <- IntMap.toAscList free
    , freeSize >= fileSize
    , let free1 = IntMap.delete pos free
          free2 =
            if freeSize == fileSize
              then free1
              else IntMap.insert (pos + fileSize) (freeSize - fileSize) free1
    ]

  checksum' pos fileId fileSize =
    fileId * (2 * pos + fileSize - 1) * fileSize `div` 2

