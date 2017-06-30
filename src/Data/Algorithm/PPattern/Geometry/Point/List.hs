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

    mkFromList :: [(Int, Int)] -> [Point.Point]
    mkFromList = fmap (Tuple.uncurry Point.mk)

    mkSequential :: [Int] -> [Point.Point]
    mkSequential = mkFromList . List.zip [1..]

    moveX :: Int -> [Point.Point] -> [Point.Point]
    moveX i ps = mkFromList xys
      where
        xs  = fmap (+i) $ fmap Point.xCoord ps
        ys  = fmap Point.yCoord ps
        xys = List.zip xs ys

    moveY :: Int -> [Point.Point] -> [Point.Point]
    moveY j ps = mkFromList xys
      where
        xs  = fmap Point.xCoord ps
        ys  = fmap (+j) $ fmap Point.yCoord ps
        xys = List.zip xs ys

    move :: Int -> Int -> [Point.Point] -> [Point.Point]
    move i j ps = mkFromList xys
      where
        xs  = fmap (+i) $ fmap Point.xCoord ps
        ys  = fmap (+j) $ fmap Point.yCoord ps
        xys = List.zip xs ys
