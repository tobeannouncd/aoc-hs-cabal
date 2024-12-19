{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module AoC.Queue (
  Queue((:<|),Empty),
  (|>), snoc, pop,
  singleton, fromList, appendList
) where

import Data.Foldable (Foldable(..))

-- | @Queue front rear difference@
--
--   The following invariants hold:
--
-- > length front >= length rear
-- > difference == length front - length rear
data Queue a = Queue [a] [a] !Int
  deriving Functor

instance Show a => Show (Queue a) where
  show q = "fromList " ++ show (toList q)

{-# COMPLETE (:<|), Empty #-}

pattern Empty :: Queue a
pattern Empty <- Queue [] _ _
 where
  Empty = Queue [] [] 0

pattern (:<|) :: a -> Queue a -> Queue a
pattern x :<| xs <- (pop -> Just (x,xs))

-- | Inline equivalent to @flip 'snoc'@
(|>) :: Queue a -> a -> Queue a
(|>) = flip snoc
{-# INLINE (|>) #-}

instance Foldable Queue where
  foldMap _ (Queue []    _ _) = mempty
  foldMap f (Queue (x:l) r 0) = f x <> go l r
   where
    go []      (y: _) = f y
    go (x':l') (y:ys) = f x' <> go l' ys <> f y
    go _       _      = error "Queue invariant violated"
  foldMap f (Queue (x:l) r d) = f x <> foldMap f (Queue l r (d-1))
  null    (Queue l _ _) = null l
  length  (Queue _ r d) = 2*length r + d
  elem x  (Queue l r _) = elem x l || elem x r
  sum     (Queue l r _) = sum l + sum r
  product (Queue l r _) = product l * product r

singleton :: a -> Queue a
singleton x = Queue [x] [] 1

fromList :: [a] -> Queue a
fromList xs = Queue xs [] (length xs)

appendList :: Queue a -> [a] -> Queue a
appendList = foldl' (|>)

snoc :: a -> Queue a -> Queue a
snoc x (Queue f r d) = rebalance f (x:r) d

-- | Helper function that should be called when either
--   * Taking an element off of the front, or
--   * Adding an item to the rear
--
--   when another queue is desired.
rebalance :: [a] -> [a] -> Int -> Queue a
rebalance f r 0 = fromList (rotate f r [])
rebalance f r i = Queue f r (i-1)

-- | Lazily converts the front and rear of a queue into
--   a single list. Should only be called when front is
--   exactly one element shorter than rear.
rotate :: [a] -> [a] -> [a] -> [a]
rotate [] (y:_) acc = y:acc
rotate (x:xs) (y:ys) acc = x : rotate xs ys (y:acc)
rotate _ _ _ = error "Queue invariant violated"

pop :: Queue a -> Maybe (a, Queue a)
pop (Queue (x:f) r d) = Just (x, rebalance f r d)
pop _ = Nothing