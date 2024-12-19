module Main (main) where

import AoC.Parsec
import Data.List (sortBy)

inputP :: Parser ([(Int, Int)], [[Int]])
inputP =
  (,)
    <$> sepEndBy1 ((,) <$> nat <* char '|' <*> nat) newline <* newline
    <*> sepEndBy1 (sepBy1 nat $ char ',') newline

main :: IO ()
main = do
  (rules, updates) <- parseInput 2024 5 inputP
  let sort = sortBy \a b -> if (a,b) `elem` rules then LT else GT
      (correct, incorrect) = foldr (rearrangeWith sort) ([],[]) updates
  mapM_ (print . sum . map middle) [correct, incorrect]


rearrangeWith :: Eq a => (a -> a) -> a -> ([a], [a]) -> ([a], [a])
rearrangeWith sort update (good,bad) =
  let update' = sort update in
    if update == update'
      then (update:good, bad)
      else (good, update':bad)

middle :: [a] -> a
middle = go <*> id
  where
    go xs (_:_:ys) = go (tail xs) ys
    go xs _        = head xs
