{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module AoC.PQueue (
  PQueue (Empty, (:<|)),

  -- * Construction
  singleton,
  fromList,

  -- * Insertion
  insert,

  -- * Query
  AoC.PQueue.null,
  view,
  viewWithPriority,
) where

import Data.IntMap (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Text.Read (readPrec, Lexeme(Ident), parens, prec, lift, step)
import Text.Read.Lex (expect)

{- | Priority queue. No guarantees are made regarding the order
entries with the same priority are returned in.
-}
newtype PQueue a = PQ (IntMap [a]) -- invariant: all values non-empty
  deriving (Functor, Foldable, Traversable)

{- | Show a 'PQueue' using 'fromList'

>>> show (singleton 1 'a')
"fromList [(1,'a')]"
-}
instance (Show a) => Show (PQueue a) where
  showsPrec p (PQ q) = showParen (p >= 10)
                     $ showString "fromList "
                     . showsPrec 11 [ (k, v)
                                    | (k, vs) <- IntMap.toList q
                                    , v <- vs ]

instance (Read a) => Read (PQueue a) where
  readPrec = parens . prec 10 $ do
    lift . expect $ Ident "fromList"
    xs <- step readPrec
    return (fromList xs)

{-# COMPLETE Empty, (:<|) #-}

-- | Empty priority queue
pattern Empty :: PQueue a
pattern Empty <- (AoC.PQueue.null -> True)
  where
    Empty = PQ IntMap.empty

{- | Pattern for extracting an element with the minimum priority
from the queue. See also: 'view'
-}
pattern (:<|) :: a -> PQueue a -> PQueue a
pattern v :<| q <- (view -> Just (v, q))

-- | Test if a queue has no elements.
null :: PQueue a -> Bool
null (PQ q) = IntMap.null q

-- | Construct a priority queue from a single priority and value.
singleton ::
  -- | priority
  Int ->
  -- | value
  a ->
  PQueue a
singleton p v = PQ (IntMap.singleton p [v])

-- | Insert a new value into the queue given a priority.
insert ::
  -- | priority
  Int ->
  -- | value
  a ->
  PQueue a ->
  PQueue a
insert k v (PQ q) = PQ (IntMap.alter aux k q)
 where
  aux Nothing = Just [v]
  aux (Just vs) = Just (v : vs)

{- | Match the lowest priority entry in a queue returning the corresponding
value and queue without that entry. See also: (':<|')
-}
view :: PQueue a -> Maybe (a, PQueue a)
view (PQ q) =
  do
    ((k, xs), q1) <- IntMap.minViewWithKey q
    case xs of
      [] -> error "Advent.PQueue.view: Malformed queue"
      [x] -> Just (x, PQ q1)
      x : xs' ->
        let q2 = PQ (IntMap.insert k xs' q1)
         in q2 `seq` Just (x, q2)

{- | Match the lowest priority entry in a queue returning the corresponding
priority, value and queue without that entry.
-}
viewWithPriority :: PQueue a -> Maybe (Int, a, PQueue a)
viewWithPriority (PQ q) =
  do
    ((k, xs), q1) <- IntMap.minViewWithKey q
    case xs of
      []      -> error "Advent.PQueue.view: Malformed queue"
      [x]     -> Just (k, x, PQ q1)
      x : xs' ->
        let q2 = PQ (IntMap.insert k xs' q1)
         in q2 `seq` Just (k, x, q2)

-- | Construct a priority queue from a list of priorities and values.
fromList :: [(Int, a)] -> PQueue a
fromList xs = PQ (IntMap.fromListWith (++) [(p, [v]) | (p, v) <- xs])