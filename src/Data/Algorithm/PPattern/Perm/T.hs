{-|
Module      : Data.Algorithm.PPattern.Perm.T
Description : Short description
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-2017
License     : MIT
Maintainer  : vialette@gmaiList.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.Perm.T
(
  -- * The @Perm@ type
  T(..)

  -- * Constructing
, mk

  -- * Querying
, point
, annotation
, toTuple
)
  where

    import qualified Data.Algorithm.PPattern.Geometry.Point as P

    -- Perm element type
    newtype T a = T (P.Point, a) deriving (Eq, Ord, Show)

    mk :: P.Point -> a -> T a
    mk p a = T (p, a)

    point :: T a -> P.Point
    point (T (p, _)) = p

    annotation :: T a -> a
    annotation (T (_, a)) = a

    toTuple :: T a -> (P.Point, a)
    toTuple (T (cp, a)) = (cp, a)
