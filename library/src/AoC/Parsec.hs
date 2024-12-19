module AoC.Parsec (
  module Text.Parsec,
  module Text.Parsec.String,
  nat, int, parseInput,
) where

import Text.Parsec
import Text.Parsec.String
import Data.Char (digitToInt)
import AoC.Prelude ( eitherFail )
import AoC.Input (getRawInput)
import Text.Printf (printf)

nat :: (Stream s m Char, Integral a) => ParsecT s u m a
nat = foldl (\a x -> 10*a + fromIntegral (digitToInt x)) 0 <$> many1 digit

int :: (Stream s m Char, Integral a) => ParsecT s u m a
int = (*) <$> option 1 (-1 <$ char '-') <*> nat

parseInput :: Integer -> Integer -> Parser a -> IO a
parseInput y d p = eitherFail . parse p (printf "%d-%02d" y d) =<< getRawInput y d
