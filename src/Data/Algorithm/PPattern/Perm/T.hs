{-|
Module      : Data.Algorithm.PPattern.Perm.T
Description : Short description
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-2017
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Permutation element type.
-}

module Data.Algorithm.PPattern.Perm.T
(
  -- * The @Perm.T@ type
  T

  -- * Querying
, point
, xCoord
, yCoord
, annotation

  -- * Constructing
, mk
, mk'

  -- * Rendering
, toTuple
)
where

  import qualified Data.Algorithm.PPattern.Geometry.Point as Point

  -- Perm type element
  newtype T a = T (Point.Point, a) deriving (Eq, Ord, Show)

  mk :: Point.Point -> a -> T a
  mk p a = T (p, a)

  mk' :: Int -> Int -> a -> T a
  mk' x y a = T (cp, a)
    where
      cp = Point.mk x y

  point :: T a -> Point.Point
  point (T (p, _)) = p

  xCoord :: T a -> Int
  xCoord (T (p, _)) = Point.xCoord p

  yCoord :: T a -> Int
  yCoord (T (p, _)) = Point.yCoord p

  annotation :: T a -> a
  annotation (T (_, a)) = a

  toTuple :: T a -> (Point.Point, a)
  toTuple (T (cp, a)) = (cp, a)
