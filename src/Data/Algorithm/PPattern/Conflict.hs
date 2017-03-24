{-|
Module      : Data.Algorithm.PPattern.Conflict
Structription : Encapsulate a conflict
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-2017
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer Structription of this module, containing some
commentary with @some markup@.
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

  import qualified Data.Algorithm.PPattern.ColorPoint as ColorPoint

  data Conflict = OrderConflict {-# UNPACK #-} !ColorPoint.ColorPoint !Int
                | ValueConflict {-# UNPACK #-} !ColorPoint.ColorPoint !Int
                deriving (Show)

  colorPoint :: Conflict -> ColorPoint.ColorPoint
  colorPoint (OrderConflict cp _) = cp
  colorPoint (ValueConflict cp _) = cp

  threshold :: Conflict -> Int
  threshold (OrderConflict _ t) = t
  threshold (ValueConflict _ t) = t
