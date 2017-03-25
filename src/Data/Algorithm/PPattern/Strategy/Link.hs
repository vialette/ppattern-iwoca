{-|
Module      : Data.Algorithm.PPattern.Strategy.Link
Description : Link strategy element
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-2017
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental
-}

module Data.Algorithm.PPattern.Strategy.Link
(
  -- * The @Link@ type
  Link(..)

  -- * Constructing
, mk

  -- * Querying
, fstColorPoint
, sndColorPoint
)
where

  import qualified Data.Algorithm.PPattern.Geometry.ColorPoint as CP

  -- simple link
  newtype Link = Link (CP.ColorPoint, CP.ColorPoint)
                 deriving (Show, Eq, Ord)

  -- Construct a simple link.
  mk :: CP.ColorPoint -> CP.ColorPoint -> Link
  mk cp1 cp2 = Link (cp1, cp2)

  -- First color point.
  fstColorPoint :: Link -> CP.ColorPoint
  fstColorPoint (Link (cp1, _)) = cp1

  -- Second color point.
  sndColorPoint :: Link -> CP.ColorPoint
  sndColorPoint (Link (_, cp2)) = cp2
