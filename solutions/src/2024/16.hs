{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Main (main) where

import AoC.Coord
import AoC.Search
import Data.Map (Map)
import Data.Map qualified as M
import Data.Set qualified as S
import AoC
import qualified Data.IntMap as IM

main :: IO ()
main = do
  input <- getRawInput 2024 16
  let walls = S.fromList [pt | (pt,'#') <- from2dString input]
      start = head [pt | (pt,'S') <- from2dString input]
      end   = head [pt | (pt,'E') <- from2dString input]
      step (p,v) = [(1000, (p,clockwise v))]
        ++ [(1000, (p,counterclockwise v))]
        ++ [(1, (p',v)) | let p' = p+v, p' `S.notMember` walls]
      Just (cost, ends, preds) = findPath (start,east) step ((== end) . fst)
      seats = S.map fst (flood (preds M.!) ends)
  print cost
  print (length seats)


-- | Dijkstra's algorithm to find the cost of the shortest path along with the
--   states reachable in that minimal cost and the map of each intermediate
--   state to its immediate predecessors
findPath :: Ord a =>
  a                 {- ^ starting node -} ->
  (a -> [(Int, a)]) {- ^ successors w/ cost -} ->
  (a -> Bool)       {- ^ predicate for nodes at the destination -} ->
  Maybe (Int, [a], Map a [a]) {- ^ cost, ending states, predecessors -}
findPath start step atEnd = go M.empty (IM.singleton 0 (M.singleton start []))
 where
  addWork q (k,v) = IM.insertWith (M.unionWith (++)) k v q
  go seen todo =
    case IM.minViewWithKey todo of
      Nothing -> Nothing
      Just ((cost,states),todo1)
        | null endStates -> go seen' todo2
        | otherwise      -> Just (cost, endStates, seen')
       where
        newStates = M.difference states seen
        endStates = filter atEnd (M.keys newStates)
        seen'     = M.union seen newStates
        todo2 = foldl addWork todo1
          [ (cost + amt, M.singleton next [cur])
          | cur <- M.keys newStates
          , (amt,next) <- step cur ]
