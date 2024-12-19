module AoC.Prelude where
import Data.Typeable ( Typeable, cast )

countIf :: (a -> Bool) -> [a] -> Int
countIf p = length . filter p


count :: Eq a => a -> [a] -> Int
count = countIf . (==)

eitherFail :: (MonadFail m, Typeable b, Show b) => Either b a -> m a
eitherFail = either (fail . show') return
 where
  show' x
    | Just s <- cast x = s
    | otherwise = show x