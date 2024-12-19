module AoC.Search (
  dfs, dfsN, dfsOn, dfsOnN,
  bfs, bfsN, bfsOn, bfsOnN, astarOnN, astarOn, astarN, astar, AStep(..), flood
) where
import qualified AoC.Queue as Q
import qualified Data.Set as Set
import qualified AoC.PQueue as PQueue
import Data.Foldable (foldl')

dfs :: Ord a => (a -> [a]) -> a -> [a]
dfs = dfsOn id
{-# INLINE dfs #-}

dfsN :: Ord a => (a -> [a]) -> [a] -> [a]
dfsN = dfsOnN id
{-# INLINE dfsN #-}

dfsOn :: Ord t => (a -> t) -> (a -> [a]) -> a -> [a]
dfsOn rep next = dfsOnN rep next . pure
{-# INLINE dfsOn #-}

dfsOnN :: Ord t => (a -> t) -> (a -> [a]) -> [a] -> [a]
dfsOnN rep next = go Set.empty
 where
  go !seen = \case
    [] -> []
    x:xs
      | r `Set.member` seen -> go seen xs
      | otherwise -> x : go seen' (next x ++ xs)
      where
        r = rep x
        seen' = Set.insert r seen

bfs :: Ord a => (a -> [a]) -> a -> [a]
bfs = bfsOn id
{-# INLINE bfs #-}

bfsN :: Ord a => (a -> [a]) -> [a] -> [a]
bfsN = bfsOnN id
{-# INLINE bfsN #-}

bfsOn :: Ord r => (a -> r) -> (a -> [a]) -> a -> [a]
bfsOn rep nxt = bfsOnN rep nxt . pure
{-# INLINE [0] bfsOn #-}

bfsOnN :: Ord r => (a -> r) -> (a -> [a]) -> [a] -> [a]
bfsOnN rep next starts = go Set.empty (Q.fromList starts)
 where
  go !seen = \case
    Q.Empty -> []
    x Q.:<| q
      | r `Set.member` seen -> go seen q
      | otherwise -> x : go seen' q'
      where
        r = rep x
        seen' = Set.insert r seen
        q' = Q.appendList q (next x)

astar :: Ord a => (a -> [AStep a]) -> a -> [(a,Int)]
astar = astarOn id
{-# INLINE astar #-}

astarN :: Ord a => (a -> [AStep a]) -> [a] -> [(a, Int)]
astarN = astarOnN id
{-# INLINE astarN #-}

astarOn :: Ord b => (a -> b) -> (a -> [AStep a]) -> a -> [(a, Int)]
astarOn rep nexts start = astarOnN rep nexts [start]

astarOnN :: Ord b => (a -> b) -> (a -> [AStep a]) -> [a] -> [(a,Int)]
astarOnN rep nexts starts = go Set.empty (PQueue.fromList [(0, WC 0 s) | s <- starts])
 where
  go !seen = \case
    PQueue.Empty -> []
    WC cost x PQueue.:<| work
      | Set.member r seen -> go seen work
      | otherwise -> (x,cost) : go seen' work'
     where
      r = rep x
      seen' = Set.insert r seen
      work' = foldl' addWork work (nexts x)
      addWork w (AStep x' stepcost heuristic) =
        PQueue.insert (cost' + heuristic) (WC cost' x') w
       where
        cost' = cost + stepcost

data AStep a = AStep {
  aNext      :: a,
  aCost      :: !Int,
  aHeuristic :: !Int
}

data WithCost a = WC !Int a

-- | Returns the set of all reachable nodes from a given list of starting nodes
--   and a successor function.
flood :: Ord a => (a -> [a]) -> [a] -> Set.Set a
flood step = foldl' go Set.empty
 where
  go seen x
    | x `Set.member` seen = seen
    | otherwise = foldl' go (Set.insert x seen) (step x)