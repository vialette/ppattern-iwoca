{-|
Module      : Data.Algorithm.PPattern.Conflict
Structription : Encapsulate a conflict
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-2017
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Conflict for pattern matching.
-}

module Data.Algorithm.PPattern.Search.Conflict
(
  -- * The @Conflict@ type
  Conflict(..)

  -- * Querying
, colorPoint
, threshold
)
where

  import qualified Data.Algorithm.PPattern.Geometry.ColorPoint as ColorPoint

  -- Encapsulate a conflict (order or value conflict).
  data Conflict = OrderConflict {-# UNPACK #-} !ColorPoint.ColorPoint !Int
                | ValueConflict {-# UNPACK #-} !ColorPoint.ColorPoint !Int
                deriving (Show)

  -- Return the color point of a conflict.
  colorPoint :: Conflict -> ColorPoint.ColorPoint
  colorPoint (OrderConflict cp _) = cp
  colorPoint (ValueConflict cp _) = cp

  -- Return the threshold of a conflict
  threshold :: Conflict -> Int
  threshold (OrderConflict _ t) = t
  threshold (ValueConflict _ t) = t
