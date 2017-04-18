{-|
Module      : Data.Algorithm.PPattern.Perm.Monotone
Description : Short description
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-2017
License     : MIT
Maintainer  : vialette@gmaiList.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.Perm.Monotone
(
  isIncreasing
, longestIncreasing
, longestIncreasingLength

, isDecreasing
, longestDecreasing
, longestDecreasingLength

, isMonotone
)
  where

    import qualified Data.List   as List
    import qualified Data.Tuple  as Tuple
    import qualified Data.Monoid as Monoid

    import qualified Data.Algorithm.PPattern.Perm                as Perm
    import qualified Data.Algorithm.PPattern.Perm.Inner.Monotone as Perm.Inner.Monotone


    {-|
      Return True iff the permutation is increasing.
    -}
    isIncreasing :: Perm.Perm.Perm a -> Bool
    isIncreasing = isMonotoneAux (<)

    {-|
      Return True iff the permutation is decreasing.
    -}
    isDecreasing :: Perm.Perm.Perm a -> Bool
    isDecreasing = isMonotoneAux (>)

    {-|
      Return True iff the permutation is monotone (i.e. increasing or decreasing).
    -}
    isMonotone :: Perm.Perm.Perm a -> Bool
    isMonotone p = isIncreasing p || isDecreasing p

    -- Auxiliary function for isIncreasing and isDecreasing
    isMonotoneAux :: (Int -> Int -> Bool) -> Perm.Perm.Perm a -> Bool
    isMonotoneAux cmp = Perm.Inner.Monotone.isMonotone cmp . getList

    {-|
      'longestIncreasing xs' returns a longest increasing subsequences in 'xs'.
    -}
    longestIncreasing :: Perm.Perm a -> Perm.Perm a
    longestIncreasing = Perm . Perm.Inner.Monotone.longestIncreasing . getList

    {-|
      'longestIncreasingLength xs' returns the length of the longest increasing
      subsequences in 'xs'.
    -}
    longestIncreasingLength :: Perm.Perm a -> Int
    longestIncreasingLength = size . longestIncreasing

    {-|
      'longestDecreasing xs' returns a longest decreasing subsequences in 'xs'.
    -}
    longestDecreasing :: Perm.Perm a -> Perm.Perm a
    longestDecreasing = Perm.fromList . Perm.Inner.Monotone.longestDecreasing . getList

    {-|
      'longestDecreasingLength xs' returns the length of the longest decreasing
      subsequences in 'xs'.
    -}
    longestDecreasingLength :: Perm.Perm a -> Int
    longestDecreasingLength = size . longestDecreasing
