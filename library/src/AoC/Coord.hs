{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE PatternSynonyms #-}

module AoC.Coord (
  pattern C,
  Coord,
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
  charToVec,
) where

import Data.Foldable  (toList)
import Data.Ix        (Ix)
import Data.Map       (Map)
import Data.Map.Strict qualified as Map
import Data.Semigroup (Max (Max), Min (Min))
import Linear.V2      (V2 (..))
import Text.Read      (readPrec, Lexeme(Ident), parens, lift, prec, step)
import Text.Read.Lex  (expect)


newtype Coord' a = Coord' (V2 a)
  deriving (Eq, Ord, Num, Ix, Foldable, Functor)

type Coord = Coord' Int

instance Show Coord where
  showsPrec p (C y x)
    = showParen (p > 10)
    $ showString "C "
    . showsPrec 11 y
    . showChar ' '
    . showsPrec 11 x

instance Read Coord where
  readPrec = parens (prec 10 p)
   where
    p = do
      lift $ expect (Ident "C")
      y <- step readPrec
      x <- step readPrec
      return (C y x)

pattern C :: Int -> Int -> Coord
pattern C{yVal, xVal} = Coord' (V2 yVal xVal)

{-# COMPLETE C #-}

newtype Bounds = B {getBounds :: Maybe (Coord, Coord)}
  deriving (Semigroup, Monoid) via Maybe (V2 (Min Int), V2 (Max Int))

boundingBox :: (Foldable t) => t Coord -> Maybe (Coord, Coord)
boundingBox = getBounds . foldMap (\c -> B $ Just (c, c))

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

manhattan :: Coord -> Coord -> Int
manhattan a b = norm1 (a - b)

norm1 :: Coord -> Int
norm1 = sum . fmap abs

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
  x   -> fail $ "Coord.charToVec: invalid char " ++ show x

from2dString :: String -> [(Coord, Char)]
from2dString str =
  [ (C y x, c)
  | (y, row) <- zip [0 ..] (lines str)
  , (x, c) <- zip [0 ..] row
  ]