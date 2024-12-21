{-# LANGUAGE TypeFamilies #-}
module AoC.Coord (
  Coord(..),
  xVal,
  yVal,
  boundingBox,
  drawPicture,
  drawCoords,
  above,
  below,
  left,
  right,
  clockwise,
  counterclockwise,
  turnLeft, turnRight,
  manhattan,
  norm1,
  neighbors,
  cardinal,
  origin,
  north,
  west,
  south,
  east,
  from2dString,
  charToVec, diamond, strToArray,
) where

import Data.Foldable  (toList)
import Data.Ix        (Ix(..))
import Data.Map       (Map)
import Data.Map.Strict qualified as Map
import Data.Bifunctor (bimap)
import Data.List      (findIndex)
import Data.Semigroup (Max (..), Min (..))
import Data.Array.Unboxed (UArray, listArray)
import Data.MonoTraversable

data Coord = C !Int !Int
  deriving (Show, Read, Eq, Ord)

type instance Element Coord = Int

instance MonoFunctor Coord where
  omap f (C y x) = C (f y) (f x)

instance MonoFoldable Coord where
  ofoldMap f (C y x) = f y <> f x
  ofoldr f a (C y x) = y `f` (x `f` a)
  ofoldr1Ex f (C y x) = y `f` x
  ofoldl1Ex' f (C y x) = y `f` x
  ofoldl' f a (C y x) =
    let a' = f a y
    in a' `seq` f a' x

instance MonoPointed Coord where
  opoint x = C x x

yVal :: Coord -> Int
yVal (C y _) = y

xVal :: Coord -> Int
xVal (C _ x) = x

instance Num Coord where
  (+) = czipWith (+)
  (-) = czipWith (-)
  (*) = czipWith (*)
  abs = omap abs
  signum = omap signum
  negate = omap negate
  fromInteger n =
    let n' = fromInteger n
    in C n' n'

instance Ix Coord where
  range (C y1 x1, C y2 x2) = [C y x | (y,x) <- range ((y1,x1),(y2,x2))]
  index (C y1 x1, C y2 x2) (C y x) = index ((y1,x1),(y2,x2)) (y,x)
  inRange (C y1 x1, C y2 x2) (C y x) = inRange ((y1,x1),(y2,x2)) (y,x)

czipWith :: (Int -> Int -> Int) -> Coord -> Coord -> Coord
czipWith f (C y1 x1) (C y2 x2) = C (f y1 y2) (f x1 x2)

newtype Bounds = B (Maybe (Min Coord, Max Coord))

instance Semigroup Bounds where
  B a <> B b = B (a <> b)

instance Monoid Bounds where
  mempty = B Nothing

getBounds :: Bounds -> Maybe (Coord, Coord)
getBounds (B bs) = bimap getMin getMax <$> bs

boundingBox :: (Foldable t) => t Coord -> Maybe (Coord, Coord)
boundingBox = getBounds . foldMap (\c -> B $ Just (Min c, Max c))

drawPicture :: Char -> Map Coord Char -> String
drawPicture bg pixels =
  case boundingBox (Map.keys pixels) of
    Nothing -> ""
    Just (C ylo xlo, C yhi xhi) ->
      unlines
        [[at $ C y x | x <- [xlo .. xhi]] | y <- [ylo .. yhi]]
 where
  at c = Map.findWithDefault bg c pixels

drawCoords :: (Foldable t) => t Coord -> String
drawCoords cs = drawPicture '·' $ Map.fromList [(c, '█') | c <- toList cs]

above :: Coord -> Coord
above (C y x) = C (y - 1) x

below :: Coord -> Coord
below (C y x) = C (y + 1) x

left :: Coord -> Coord
left (C y x) = C y (x - 1)

right :: Coord -> Coord
right (C y x) = C y (x + 1)

clockwise :: Coord -> Coord
clockwise (C y x) = C x (-y)

counterclockwise :: Coord -> Coord
counterclockwise (C y x) = C (-x) y

turnLeft :: Coord -> Coord
turnLeft = counterclockwise

turnRight :: Coord -> Coord
turnRight = clockwise

manhattan :: Coord -> Coord -> Int
manhattan a b = norm1 (a - b)

norm1 :: Coord -> Int
norm1 = osum . abs

cardinal :: Coord -> [Coord]
cardinal c = [above c, left c, right c, below c]

neighbors :: Coord -> [Coord]
neighbors c =
  [ above (left c)
  , above c
  , above (right c)
  , left c
  , right c
  , below (left c)
  , below c
  , below (right c)
  ]

origin :: Coord
origin = C 0 0

north :: Coord
north = C (-1) 0

south :: Coord
south = C 1 0

west :: Coord
west = C 0 (-1)

east :: Coord
east = C 0 1

charToVec :: (MonadFail m) => Char -> m Coord
charToVec = \case
  '^' -> return north
  'v' -> return south
  '<' -> return west
  '>' -> return east
  x   -> fail $ "AoC.Coord.charToVec: invalid char " ++ show x

from2dString :: String -> [(Coord, Char)]
from2dString str =
  [ (C y x, c)
  | (y, row) <- zip [0 ..] (lines str)
  , (x, c) <- zip [0 ..] row
  ]

-- | @diamond n pt@ returns a list of points that are a maximum manhattan
--   distance of @n@ away from @pt@
--
--   Below is an illustration of the points in @diamond 3 pt@ where @pt@ is
--   represented as an @O@ and the surrounding points are @X@
--
-- @
--    X
--   XXX
--  XXXXX
-- XXXOXXX
--  XXXXX
--   XXX
--    X
-- @
diamond :: Int -> Coord -> [Coord]
diamond n (C y x) =
  [ C y' x'
  | y' <- [y-n .. y+ n]
  , let dy = abs (y - y')
  , x' <- [x-n+dy .. x+n-dy]
  ]

strToArray :: (MonadFail m) => String -> m (UArray Coord Char)
strToArray str =
  case lines str of
    [] -> fail "AoC.Coord.strToArray: empty grid"
    xs@(h:t) -> do
      let w = length h
      case findIndex (\y -> length y /= w) t of
        Just i  -> fail ("AoC.Coord.strToArray: bad length on line " ++ show (i+2))
        Nothing -> return $ listArray (0, C (length xs-1) (w-1)) (concat xs)