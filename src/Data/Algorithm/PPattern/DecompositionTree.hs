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
  DecompositionTree

  -- * Constructing
, mk
, union
, intersection

  -- * Comparing
, precedes
, follows
, disjoint
, intersects
, consecutive
)
where

  data DecompositionTree = Leaf Perm.Perm
                         | Plus DecompositionTree DecompositionTree
                         | Minus DecompositionTree DecompositionTree
