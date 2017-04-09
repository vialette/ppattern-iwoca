{-|
Module      : Data.Algorithm.PPattern.SplitTree
Description : Short description
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-2017
License     : MIT
Maintainer  : vialette@gmaiList.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.SplitTree
(
  -- * The @SplitTree@ type
  SplitTree(..)

  -- * Constructing
-- , mk
)
where

  -- import qualified Data.Algorithm.PPattern.Geometry.Point    as P
  import qualified Data.Algorithm.PPattern.Geometry.Interval as I


  data SplitTree = Leaf  {-# UNPACK #-} !I.Interval
                 | Plus  {-# UNPACK #-} !I.Interval !SplitTree !SplitTree
                 | Minus {-# UNPACK #-} !I.Interval !SplitTree !SplitTree
