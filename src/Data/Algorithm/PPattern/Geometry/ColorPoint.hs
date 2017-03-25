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

  import qualified Data.Algorithm.PPattern.Color          as C
  import qualified Data.Algorithm.PPattern.Geometry.Point as P

  {-|
    'ColorPoint' type encapsulates a point and a color.
  -}
  newtype ColorPoint = ColorPoint (P.Point, C.Color) deriving (Show, Eq, Ord)

  {-|
    'mk' makes a colored point from two coordinates and a color.
  -}
  mk ::Int -> Int -> C.Color -> ColorPoint
  mk x y c = ColorPoint (P.mk x y, c)

  {-|
    'mkPoint' makes a colored point from a point and a color.
  -}
  mk2 :: P.Point -> C.Color -> ColorPoint
  mk2 p c = ColorPoint (p, c)

  {-|
    'mkBlank' makes a blank point from two coordinates.
  -}
  mkBlank :: Int -> Int -> ColorPoint
  mkBlank x y = ColorPoint (P.mk x y, C.blankColor)

  {-|
    'mkPoint' makes a blank point from a point.
  -}
  mk2Blank :: P.Point -> ColorPoint
  mk2Blank p = ColorPoint (p, C.blankColor)

  {-|
    Transform a color point into a triple (x-ccordinate, y-ccordinate, color).
  -}
  toTuple :: ColorPoint -> (Int, Int, C.Color)
  toTuple (ColorPoint (p, c)) = (x, y, c)
    where
      (x, y) = P.toTuple p

  point :: ColorPoint -> P.Point
  point (ColorPoint (p, _)) = p

  {-|
    Get color point x-coordinate.
  -}
  xCoord :: ColorPoint -> Int
  xCoord (ColorPoint (p, _)) = P.xCoord p

  {-|
    Get color point y-coordinate.
  -}
  yCoord :: ColorPoint -> Int
  yCoord (ColorPoint (p, _)) = P.yCoord p

  {-|
    Get color point color.
  -}
  color :: ColorPoint -> C.Color
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
      p' = P.updateXCoord x p

  {-|
    Update color point y-coordinate.
  -}
  updateYCoord :: Int -> ColorPoint -> ColorPoint
  updateYCoord y (ColorPoint (p, c)) = mk2 p' c
    where
      p' = P.updateYCoord y p

  {-|
    Update color point color.
  -}
  updateColor :: C.Color -> ColorPoint -> ColorPoint
  updateColor c' (ColorPoint (p, _)) = mk2 p c'
