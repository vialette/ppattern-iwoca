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

  -- * Constructing
, mk
, mkFromList
, mkSequential

 -- * Accessing
, xCoord
, yCoord

  -- * Rendering
, toTuple

  -- * Modifying
, updateXCoord
, updateXCoord'
, updateYCoord
, updateYCoord'
)
where

  import qualified Data.Tuple as Tuple
  import qualified Data.List  as List

  newtype Point = Point (Int, Int) deriving (Show, Eq, Ord)

  mk :: Int -> Int -> Point
  mk x y = Point (x, y)

  mkFromList :: [(Int, Int)] -> [Point]
  mkFromList = fmap (Tuple.uncurry mk)

  mkSequential :: [Int] -> [Point]
  mkSequential = mkFromList . List.zip [1..]

  toTuple :: Point -> (Int, Int)
  toTuple (Point (x, y)) = (x, y)

  xCoord :: Point -> Int
  xCoord (Point (x, _)) = x

  yCoord :: Point -> Int
  yCoord (Point (_, y)) = y

  updateXCoord :: Int -> Point -> Point
  updateXCoord x' (Point (_, y)) = mk x' y

  updateXCoord' :: (Int -> Int) -> Point -> Point
  updateXCoord' f (Point (x, y)) = mk x' y
    where
      x' = f x

  updateYCoord :: Int -> Point -> Point
  updateYCoord y' (Point (x, _)) = mk x y'

  updateYCoord' :: (Int -> Int) -> Point -> Point
  updateYCoord' f (Point (x, y)) = mk x y'
    where
      y' = f y
