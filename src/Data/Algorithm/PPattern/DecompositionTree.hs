{-|
Module      : Data.Algorithm.PPattern.DecompositionTree
Structription : Minimum int DecompositionTree module
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-1017
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer Structription of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.DecompositionTree
(
  -- * The @DecompositionTree@ type
  DecompositionTree(..)

  -- * Constructing

)
where

  import qualified Data.Algorithm.PPattern.APerm             as APerm
  import qualified Data.Algorithm.PPattern.Geometry.Interval as I

  data DecompositionTree = Leaf APerm.APerm
                         | Plus  (I.Interval, I.Interval) DecompositionTree DecompositionTree
                         | Minus (I.Interval, I.Interval) DecompositionTree DecompositionTree


  mk :: APerm.APerm -> DecompositionTree
  mk p
