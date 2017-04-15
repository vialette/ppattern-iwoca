{-|
Module      : Data.Algorithm.PPattern.DecreasingBinaryTree
Description : Short description
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-2017
License     : MIT
Maintainer  : vialette@gmaiList.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.DecreasingBinaryTree
(
  -- * The @DecreasingBinaryTree@ type
  DecreasingBinaryTree(..)

  -- * Constructing
, mk

, display

  -- * Accessing
, interval
)
where

  import qualified Data.Monoid as Monoid

  import qualified Data.Algorithm.PPattern.Geometry.APoint as APoint

  data DecreasingBinaryTree a = Empty
                              | Node {-# UNPACK #-} APoint.APoint (DecreasingBinaryTree a) (DecreasingBinaryTree a)
                              deriving (Eq)

  mk :: [P.Point] -> DecreasingBinaryTree
  mk = mkAux []
