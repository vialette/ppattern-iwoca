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

    import qualified Data.Algorithm.PPattern.Perm                as Perm
    import qualified Data.Algorithm.PPattern.Perm.Inner.Monotone as Perm.Inner.Monotone

    {-|
      Return True iff the Permutation is increasing.
    -}
    isIncreasing :: Perm.Perm -> Bool
    isIncreasing = isMonotoneAux (<)

    {-|
      Return True iff the Permutation is decreasing.
    -}
    isDecreasing :: Perm.Perm -> Bool
    isDecreasing = isMonotoneAux (>)

    -- Auxiliary function for isIncreasing and isDecreasing
    isMonotoneAux :: (Int -> Int -> Bool) -> Perm.Perm -> Bool
    isMonotoneAux cmp = Perm.Inner.Monotone.isMonotone cmp . Perm.points

    {-|
      Return True iff the Permutation is monotone (i.e. increasing or decreasing).
    -}
    isMonotone :: Perm.Perm -> Bool
    isMonotone p = isIncreasing p || isDecreasing p

    {-|
      'longestIncreasing xs' returns a longest increasing subsequences in 'xs'.
    -}
    longestIncreasing :: Perm.Perm -> Perm.Perm
    longestIncreasing = Perm.fromList . Perm.Inner.Monotone.longestIncreasing . Perm.points

    {-|
      'longestIncreasingLength xs' returns the length of the longest increasing
      subsequences in 'xs'.
    -}
    longestIncreasingLength :: Perm.Perm -> Int
    longestIncreasingLength = Perm.size . longestIncreasing

    {-|
      'longestDecreasing xs' returns a longest decreasing subsequences in 'xs'.
    -}
    longestDecreasing :: Perm.Perm -> Perm.Perm
    longestDecreasing = Perm.fromList . Perm.Inner.Monotone.longestDecreasing . Perm.points

    {-|
      'longestDecreasingLength xs' returns the length of the longest decreasing
      subsequences in 'xs'.
    -}
    longestDecreasingLength :: Perm.Perm -> Int
    longestDecreasingLength = Perm.size . longestDecreasing
