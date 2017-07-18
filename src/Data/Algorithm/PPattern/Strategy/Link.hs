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

  import qualified Data.Algorithm.PPattern.Geometry.ColorPoint as ColorPoint

  -- simple link
  newtype Link = Link (ColorPoint.ColorPoint, ColorPoint.ColorPoint)
                 deriving (Show, Eq, Ord)

  -- Construct a simple link.
  mk :: ColorPoint.ColorPoint -> ColorPoint.ColorPoint -> Link
  mk cp cp' = Link (cp, cp')

  -- First color point.
  fstColorPoint :: Link -> ColorPoint.ColorPoint
  fstColorPoint (Link (cp, _)) = cp

  -- Second color point.
  sndColorPoint :: Link -> ColorPoint.ColorPoint
  sndColorPoint (Link (_, cp)) = cp
