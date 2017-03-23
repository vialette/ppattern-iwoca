{-|
Module      : Data.Algorithm.PPattern.Point
Description : Short description
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.Point
(
  -- * The @CPoint@ type
  Point(..)

 -- * Accessing
, xCoord
, yCoord

  -- * Constructing
, mkPoint

  -- * Rendering
, toTuple

  -- * Modifying
, updateXCoord
, updateYCoord
)
where

  newtype Point = Point (Int, Int) deriving (Show, Eq, Ord)

  mkPoint :: Int -> Int -> Point
  mkPoint x y = Point (x, y)

  toTuple :: Point -> (Int, Int)
  toTuple (Point (x, y)) = (x, y)

  xCoord :: Point -> Int
  xCoord (Point (x, _)) = x

  yCoord :: Point -> Int
  yCoord (Point (_, y)) = y

  updateXCoord :: Int -> Point -> Point
  updateXCoord x' (Point (_, y)) = mkPoint x' y

  updateYCoord :: Int -> Point -> Point
  updateYCoord y' (Point (x, _)) = mkPoint x y'
