{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module AoC.Input (
  getRawInput,
) where

import Advent
import Data.Text (Text, unpack)
import System.Environment (getArgs, getEnv, lookupEnv)
import System.IO (hPutStrLn, stderr)

getRawInput :: Integer -> Integer -> IO String
getRawInput y d = getArgs >>= \case
  []          -> unpack <$> fromSite y d
  "-":_       -> hPutStrLn stderr "Ready!" >> getContents
  "+":input:_ -> pure input
  fn:_        -> readFile fn

-- | Uses the 'advent-of-code-api' library to get the problem input.
fromSite :: Integer -> Integer -> IO Text
fromSite y d = do
  sess  <- getEnv "AOC_SESSION"
  cache <- lookupEnv "AOC_CACHE"
  day'  <- maybe (fail $ "invalid day: " ++ show d) return (mkDay d)
  let agent = AoCUserAgent "tobeannouncd/aoc-hs" "tobeannouncd@gmail.com"
      opts  = (defaultAoCOpts agent y sess){_aCache = cache}
  runAoC_ opts $ AoCInput day'