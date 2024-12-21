module AoC.Prelude where
import Data.Typeable ( Typeable, cast )
import Data.Foldable (toList)

countIf :: (Foldable t) => (a -> Bool) -> t a -> Int
countIf p = length . filter p . toList


count :: (Foldable t, Eq a) => a -> t a -> Int
count = countIf . (==)

eitherFail :: (MonadFail m, Typeable b, Show b) => Either b a -> m a
eitherFail = either (fail . show') return
 where
  show' x
    | Just s <- cast x = s
    | otherwise = show x