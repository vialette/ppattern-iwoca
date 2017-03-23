{-|
Module      : Data.Algorithm.PPattern.ColorPoint
Description : Short description
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.Geometry.ColorPoint
(
  -- * The @ColorPoint@ type
  ColorPoint(..)

 -- * Querying
, point
, xCoord
, yCoord
, color
, sameColor
, differentColor

  -- * Constructing
, mkColorPoint
, mkColorPoint'

  -- * Rendering
, toTuple

  -- * Modifying
, updateXCoord
, updateYCoord
, updateColor
)
where

  import qualified Data.Algorithm.PPattern.Color as Color
  import qualified Data.Algorithm.PPattern.Geometry.Point as Point

  {-|
    'ColorPoint' type encapsulates a point and a color.
  -}
  newtype ColorPoint = ColorPoint (Point.Point, Color.Color) deriving (Show, Eq, Ord)

  {-|
    'mkColorPoint' makes a colored point from two coordinates and a color.
  -}
  mkColorPoint ::Int -> Int -> Color.Color -> ColorPoint
  mkColorPoint x y c = ColorPoint (Point.mkPoint x y, c)

  {-|
    'mkPoint' makes a colored point from a point and a color.
  -}
  mkColorPoint' :: Point.Point -> Color.Color -> ColorPoint
  mkColorPoint' p c = ColorPoint (p, c)

  {-|
    'mkPoint' makes a colored point from a point and a color.
  -}
  mkColorPoint'' :: Int -> Int -> Color.Color -> ColorPoint
  mkColorPoint'' x y c = ColorPoint (p, c)
    where
      p = Point.mkPoint x y

  {-|
    Transform a color point into a triple (x-ccordinate, y-ccordinate, color).
  -}
  toTuple :: ColorPoint -> (Int, Int, Color.Color)
  toTuple (ColorPoint (p, c)) = (x, y, c)
    where
      (x, y) = Point.toTuple p

  point :: ColorPoint -> Point.Point
  point (ColorPoint (p, _)) = p

  {-|
    Get color point x-coordinate.
  -}
  xCoord :: ColorPoint -> Int
  xCoord (ColorPoint (p, _)) = Point.xCoord p

  {-|
    Get color point y-coordinate.
  -}
  yCoord :: ColorPoint -> Int
  yCoord (ColorPoint (p, _)) = Point.yCoord p

  {-|
    Get color point color.
  -}
  color :: ColorPoint -> Color.Color
  color (ColorPoint (_, c)) = c

  {-|
    Return True if two color points share the same color.
  -}
  sameColor :: ColorPoint -> ColorPoint -> Bool
  sameColor (ColorPoint (_, c)) (ColorPoint (_, c')) = c == c'

  {-|
    Return True if two color points do not share the same color.
  -}
  differentColor :: ColorPoint -> ColorPoint -> Bool
  differentColor = not . sameColor

  {-|
    Update color point x-coordinate.
  -}
  updateXCoord :: Int -> ColorPoint -> ColorPoint
  updateXCoord x (ColorPoint (p, c)) = mkColorPoint' p' c
    where
      p' = Point.updateXCoord x p

  {-|
    Update color point y-coordinate.
  -}
  updateYCoord :: Int -> ColorPoint -> ColorPointInt
  updateYCoord y (ColorPoint (p, c)) = mkColorPoint' p' c
    where
      p' = Point.updateYCoord y p

  {-|
    Update color point color.
  -}
  updateColor :: Color.Color -> ColorPoint -> ColorPoint
  updateColor c' (ColorPoint (p, _)) = mkColorPoint' p c'
