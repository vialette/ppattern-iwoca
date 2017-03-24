{-|
Module      : Data.Algorithm.PPattern.Geometry.Point
Description : Simple 2D point
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental
-}

module Data.Algorithm.PPattern.Geometry.Point
(
  -- * The @CPoint@ type
  Point

 -- * Accessing
, xCoord
, yCoord

  -- * Constructing
, mk

  -- * Rendering
, toTuple

  -- * Modifying
, updateXCoord
, updateYCoord
)
where

  newtype Point = Point (Int, Int) deriving (Show, Eq, Ord)

  mk :: Int -> Int -> Point
  mk x y = Point (x, y)

  toTuple :: Point -> (Int, Int)
  toTuple (Point (x, y)) = (x, y)

  xCoord :: Point -> Int
  xCoord (Point (x, _)) = x

  yCoord :: Point -> Int
  yCoord (Point (_, y)) = y

  updateXCoord :: Int -> Point -> Point
  updateXCoord x' (Point (_, y)) = mk x' y

  updateYCoord :: Int -> Point -> Point
  updateYCoord y' (Point (x, _)) = mk x y'
