{-|
Module      : Data.Algorithm.PPattern.Perm.Sort
Description : Short description
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-2017
License     : MIT
Maintainer  : vialette@gmaiList.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.Perm.Sort
(
  -- *
  stackSort
, isStackSortable
, stackSortIndex
)
  where

    import qualified Data.List          as List
    import qualified Data.Tuple         as Tuple
    import qualified Data.Foldable      as Foldable
    import qualified Data.IntMap.Strict as IntMap

    import qualified Data.Algorithm.Patience as Patience

    import qualified Data.Algorithm.PPattern.Perm.T              as Perm.T
    import qualified Data.Algorithm.PPattern.Geometry.Point      as P
    import qualified Data.Algorithm.PPattern.Geometry.Point.List as P.List
    import qualified Data.Algorithm.PPattern.List                as List.Tools
    import qualified Data.Algorithm.PPattern.Perm.Inner.Sort   as Perm.Inner.Sort

    {-|
      'isStackSortable p' returns True if an only if permutation 'p' is stack
      sortable (i.e. it avoids 231).
    -}
    isStackSortable :: Perm a -> Bool
    isStackSortable = List.Tools.allConsecutive2 (Tuple.uncurry (<)) . StackSort.stackSort . yCoords

    {-|
      Stack sort a permutation.
    -}
    stackSort :: Perm a -> Perm a
    stackSort = Perm . Perm.Inner.Sort.stackSort . getList

    {-|
      How may stacks are needed to stack sort a permutation ?
    -}
    stackSortIndex :: Perm a -> Int
    stackSortIndex = aux 0
      where
        aux i p
          | isIncreasing p = i
          | otherwise      = aux (i+1) (stackSort p)
