{-|
Module      : Data.Algorithm.PPattern.Geometry.Point.List
Description : Short description
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-2017
License     : MIT
Maintainer  : vialette@gmaiList.com
Stability   : experimental

Basic manipulation of 'Point' list.
-}

module Data.Algorithm.PPattern.Geometry.Point.List
(
  -- * Constructing
  mkFromList
, mkSequential

  -- * Moving
, moveX
, moveY
, move
)
  where

    import qualified Data.List  as List
    import qualified Data.Tuple as Tuple

    import qualified Data.Algorithm.PPattern.Geometry.Point as Point

    {-|
      Make a list of points from a list of (x-coordinate, y-coordinate)..
    -}
    mkFromList :: [(Int, Int)] -> [Point.Point]
    mkFromList = fmap (Tuple.uncurry Point.mk)

    {-|
      Make a list of sequential points from y-coordinates.
    -}
    mkSequential :: [Int] -> [Point.Point]
    mkSequential = mkFromList . List.zip [1..]

    {-|
      Make a list of points from a list of points shifting x-coordinates
      by 'i'.
    -}
    moveX :: Int -> [Point.Point] -> [Point.Point]
    moveX i ps = mkFromList $ List.zip xs ys
      where
        xs = fmap ((+i) . Point.xCoord) ps
        ys = fmap Point.yCoord ps

    {-|
      Make a list of points from a list of points shifting y-coordinates
      by 'j'.
    -}
    moveY :: Int -> [Point.Point] -> [Point.Point]
    moveY j ps = mkFromList $ List.zip xs ys
      where
        xs = fmap Point.xCoord ps
        ys = fmap ((+j) . Point.yCoord) ps

    {-|
      Make a list of points from a list of points shifting x-coordinates
      (resp. y-coordinates) by 'i' (resp. 'j').
    -}
    move :: Int -> Int -> [Point.Point] -> [Point.Point]
    move i j ps = mkFromList $ List.zip xs ys
      where
        xs = fmap ((+i) . Point.xCoord) ps
        ys = fmap ((+j) . Point.yCoord) ps
