{-|
Module      : Data.Algorithm.PPattern.Conflict
Structription : Encapsulate a conflict
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-2017
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Conflict for pattern matching.
-}

module Data.Algorithm.PPattern.Conflict
(
  -- * The @Resolve@ type
  Conflict(..)

  -- * Querying
, colorPoint
, threshold
)
where

  import qualified Data.Algorithm.PPattern.Geometry.ColorPoint as CP

  data Conflict = OrderConflict {-# UNPACK #-} !CP.ColorPoint !Int
                | ValueConflict {-# UNPACK #-} !CP.ColorPoint !Int
                deriving (Show)

  colorPoint :: Conflict -> CP.ColorPoint
  colorPoint (OrderConflict cp _) = cp
  colorPoint (ValueConflict cp _) = cp

  threshold :: Conflict -> Int
  threshold (OrderConflict _ t) = t
  threshold (ValueConflict _ t) = t
