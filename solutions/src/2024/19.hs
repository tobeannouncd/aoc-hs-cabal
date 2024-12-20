module Main (main) where

import AoC (countIf)
import AoC.Parsec
import Data.List (tails)
import Data.Map qualified as M

type Pattern = String
type Design = String

inputP :: Parser ([Pattern], [Design])
inputP =
  (,)
    <$> sepBy1 pattern (string ", ") <* string "\n\n"
    <*> sepEndBy1 design newline
 where
  pattern = many1 color
  design  = many1 color
  color   = oneOf "wubrg"

main :: IO ()
main = do
  (patterns, designs) <- parseInput 2024 19 inputP
  let patternTrie = foldMap toTrie patterns
      ways = map (designWays patternTrie) designs
  print $ countIf (> 0) ways
  print $ sum ways

designWays :: (Ord a) => Trie a -> [a] -> Int
designWays trie str = head memo
 where
  memo = [ if null t then 1 else sum [memo !! j | j <- matches trie i t]
         | (i,t) <- zip [0..] (tails str)
         ]

matches :: (Ord a) => Trie a -> Int -> [a] -> [Int]
matches (Node b xs) !n yys =
  [n | b]
    ++ case yys of
      y : ys | Just t <- M.lookup y xs -> matches t (n + 1) ys
      _ -> []

data Trie a = Node !Bool (M.Map a (Trie a))

toTrie :: (Ord a) => [a] -> Trie a
toTrie = foldr cons (Node True M.empty)

cons :: a -> Trie a -> Trie a
cons x t = Node False (M.singleton x t)

instance (Ord a) => Semigroup (Trie a) where
  Node x xs <> Node y ys = Node (x || y) (M.unionWith (<>) xs ys)

instance (Ord a) => Monoid (Trie a) where
  mempty = Node False M.empty