{-|
Module      : Data.Algorithm.PPattern.ColorPoint
Description : Simple color 2D point
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental
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
, mk
, mk2
, mkBlank
, mk2Blank

  -- * Rendering
, toTuple

  -- * Modifying
, updateXCoord
, updateYCoord
, updateColor
)
where

  import qualified Data.Algorithm.PPattern.Color          as Color
  import qualified Data.Algorithm.PPattern.Geometry.Point as Point

  {-|
    'ColorPoint' type encapsulates a point and a color.
  -}
  newtype ColorPoint = ColorPoint (Point.Point, Color.Color) deriving (Show, Eq, Ord)

  {-|
    'mk' makes a colored point from two coordinates and a color.
  -}
  mk ::Int -> Int -> Color.Color -> ColorPoint
  mk x y c = ColorPoint (Point.mk x y, c)

  {-|
    'mkPoint' makes a colored point from a point and a color.
  -}
  mk2 :: Point.Point -> Color.Color -> ColorPoint
  mk2 p c = ColorPoint (p, c)

  {-|
    'mkBlank' makes a blank point from two coordinates.
  -}
  mkBlank ::Int -> Int -> ColorPoint
  mkBlank x y c = ColorPoint (Point.mk x y, Color.blankColor)

  {-|
    'mkPoint' makes a blank point from a point.
  -}
  mk2Blank :: Point.Point -> Color.Color -> ColorPoint
  mk2Blank p = ColorPoint (p, Color.blankColor)

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
  differentColor (ColorPoint (_, c)) (ColorPoint (_, c')) = c /= c'

  {-|
    Update color point x-coordinate.
  -}
  updateXCoord :: Int -> ColorPoint -> ColorPoint
  updateXCoord x (ColorPoint (p, c)) = mk2 p' c
    where
      p' = Point.updateXCoord x p

  {-|
    Update color point y-coordinate.
  -}
  updateYCoord :: Int -> ColorPoint -> ColorPoint
  updateYCoord y (ColorPoint (p, c)) = mk2 p' c
    where
      p' = Point.updateYCoord y p

  {-|
    Update color point color.
  -}
  updateColor :: Color.Color -> ColorPoint -> ColorPoint
  updateColor c' (ColorPoint (p, _)) = mk2 p c'
