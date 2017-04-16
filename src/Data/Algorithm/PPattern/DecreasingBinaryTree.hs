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
                              | Node APoint.APoint (DecreasingBinaryTree a) (DecreasingBinaryTree a)
                              deriving (Eq)

  mk :: [APoint.APoint a] -> DecreasingBinaryTree a
  mk []  = Empty
  mk aps = Node ap (mk aps') (mk aps'')
    where
      (aps', ap, aps'') = Tools.List.splitAtMax aps

  height :: DecreasingBinaryTree a -> Int
  height Empty = 0
  height (DecreasingBinaryTree _ leftTree rightTree) = 1 + max hLeft hRight
    where
      hLeft  = height leftTree
      hRight = height rightTree
