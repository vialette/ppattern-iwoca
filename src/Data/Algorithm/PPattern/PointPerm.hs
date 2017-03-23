{-|
Module      : Data.Algorithm.PPattern.PointPerm
Description : Short description
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.PointPerm
(
  -- * The @Perm.T@ type
  PointPerm

 -- * Querying
, colorPoint
, point
, xCoord
, yCoord
, color
, annotation

  -- * Constructing
, mkT

  -- * Rendering
, toTuple
)
where

  import qualified Data.Algorithm.PPattern.Color as Color
  import qualified Data.Algorithm.PPattern.Geometry.ColorPoint as ColorPoint

  class PointPerm a where
    toPoints :: a -> [Point.Point]
    size :: 
