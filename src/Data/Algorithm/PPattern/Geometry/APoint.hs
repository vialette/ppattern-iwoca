{-|
Module      : Data.Algorithm.PPattern.Geometry.Point
Description : Simple 2D point
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-2017
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental
-}

module Data.Algorithm.PPattern.Geometry.APoint
(
  -- * The @CPoint@ type
  APoint(..)

  -- * Constructing
, mk

  -- * Updating
, update

 -- * Accessing
, point
, annotation

  -- * Rendering
, toTuple
)
where

  import qualified Data.Algorithm.PPattern.Geometry.Point as Point

  -- APoint element type
  newtype APoint a = APoint (Point.Point, a) deriving (Eq, Ord, Show)

  -- Make an annotated point from a point and anannotation.
  mk :: Point.Point -> a -> APoint a
  mk p a = APoint (p, a)

  -- Point projection.
  point :: APoint a -> Point.Point
  point (APoint (p, _)) = p

  -- Annotation projection.
  annotation :: APoint a -> a
  annotation (APoint (_, a)) = a

  -- Transform an annotated point into a piar (point, annotation).
  toTuple :: APoint a -> (Point.Point, a)
  toTuple (APoint (p, a)) = (p, a)

  -- Update the point part of an annotated point.
  -- update ap p = mk p (annotation ap)
  update :: APoint a -> Point.Point -> APoint a
  update (APoint (_, a)) p = APoint (p, a)
