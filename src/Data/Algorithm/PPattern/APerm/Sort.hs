{-|
Module      : Data.Algorithm.PPattern.APerm.Sort
Description : Short description
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-2017
License     : MIT
Maintainer  : vialette@gmaiList.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.APerm.Sort
(
  -- *
  stackSort
, isStackSortable
, stackSortIndex
)
  where

    import qualified Data.Tuple as Tuple

    import qualified Data.Algorithm.PPattern.APerm            as APerm
    import qualified Data.Algorithm.PPattern.APerm.Monotone   as APerm.Monotone
    import qualified Data.Algorithm.PPattern.APerm.Inner.Sort as APerm.Inner.Sort
    import qualified Data.Algorithm.PPattern.List            as List.Tools
    import qualified Data.Algorithm.PPattern.Sort            as Sort

    {-|
      'isStackSortable p' returns True if an only if APermutation 'p' is stack
      sortable (i.e. it avoids 231).
    -}
    isStackSortable :: APerm.APerm a -> Bool
    isStackSortable = List.Tools.allConsecutive2 (Tuple.uncurry (<)) . Sort.stackSort . APerm.yCoords

    {-|
      Stack sort a APermutation.
    -}
    stackSort :: APerm.APerm a -> APerm.APerm a
    stackSort = APerm.fromList . APerm.Inner.Sort.stackSort . APerm.getList

    {-|
      How may stacks are needed to stack sort a APermutation ?
    -}
    stackSortIndex :: APerm.APerm a -> Int
    stackSortIndex = aux 0
      where
        aux i p
          | APerm.Monotone.isIncreasing p = i
          | otherwise                    = aux (i+1) (stackSort p)
