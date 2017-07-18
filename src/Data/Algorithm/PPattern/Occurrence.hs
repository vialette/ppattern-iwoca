{-|
Module      : Data.Algorithm.PPattern.Occurrence
Description : Short description
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-1017
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Pattern matching occurrence.
-}

module Data.Algorithm.PPattern.Occurrence
(
  -- * The @ColorPoint@ type
  Occurrence

  -- * Constructing
, mk

 -- * Querying
, patternPoints
, patternPerm
, targetPoints
, targetPerm

 -- * Verifying
, check
, size
)
where

  import qualified Data.List  as List
  import qualified Data.Tuple as Tuple

  import qualified Data.Algorithm.PPattern.Geometry.Point      as Point
  import qualified Data.Algorithm.PPattern.Geometry.ColorPoint as ColorPoint
  import qualified Data.Algorithm.PPattern.Perm                as Perm

  -- Permutation pattern occurrence datatype
  newtype Occurrence = Occurrence { getList :: [(Point.Point, Point.Point)] }

  instance Show Occurrence where
    show = show . getList

  -- Construct a pattern matching ocurrence
  mk :: [(ColorPoint.ColorPoint, ColorPoint.ColorPoint)] -> Occurrence
  mk cpcps = Occurrence { getList = List.zip pps qps }
    where
      pps = fmap (ColorPoint.point . Tuple.fst) cpcps
      qps = fmap (ColorPoint.point . Tuple.snd) cpcps

  {-|
    Extract pattern points from an occurrence.

    >>>
  -}
  patternPoints :: Occurrence -> [Point.Point]
  patternPoints = fmap Tuple.fst . getList

  {-|
    Extract pattern permutation from an occurrence.

    >>>
  -}
  patternPerm :: Occurrence -> Perm.Perm
  patternPerm = Perm.mk . fmap Point.yCoord . patternPoints

  {-|
    Extract target  points from an occurrence.

    >>>
  -}
  targetPoints :: Occurrence -> [Point.Point]
  targetPoints = fmap Tuple.fst . getList

  {-|
    Extract target pattern from an occurrence.

    >>>
  -}
  targetPerm :: Occurrence -> Perm.Perm
  targetPerm = Perm.mk . fmap Point.yCoord . targetPoints

  {-|

    >>>
  -}
  size :: Occurrence -> Int
  size = List.length . getList

  {-|

    >>>
  -}
  check :: Occurrence -> Bool
  check occurrence = patternPerm occurrence == targetPerm  occurrence
