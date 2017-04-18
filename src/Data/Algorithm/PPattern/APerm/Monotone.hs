{-|
Module      : Data.Algorithm.PPattern.APerm.Monotone
Description : Short description
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-2017
License     : MIT
Maintainer  : vialette@gmaiList.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.APerm.Monotone
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

    import qualified Data.Algorithm.PPattern.APerm                as APerm
    import qualified Data.Algorithm.PPattern.APerm.Inner.Monotone as APerm.Inner.Monotone

    {-|
      Return True iff the APermutation is increasing.
    -}
    isIncreasing :: APerm.APerm a -> Bool
    isIncreasing = isMonotoneAux (<)

    {-|
      Return True iff the APermutation is decreasing.
    -}
    isDecreasing :: APerm.APerm a -> Bool
    isDecreasing = isMonotoneAux (>)

    -- Auxiliary function for isIncreasing and isDecreasing
    isMonotoneAux :: (Int -> Int -> Bool) -> APerm.APerm a -> Bool
    isMonotoneAux cmp = APerm.Inner.Monotone.isMonotone cmp . APerm.getList

    {-|
      Return True iff the APermutation is monotone (i.e. increasing or decreasing).
    -}
    isMonotone :: APerm.APerm a -> Bool
    isMonotone p = isIncreasing p || isDecreasing p

    {-|
      'longestIncreasing xs' returns a longest increasing subsequences in 'xs'.
    -}
    longestIncreasing :: APerm.APerm a -> APerm.APerm a
    longestIncreasing = APerm.fromList . APerm.Inner.Monotone.longestIncreasing . APerm.getList

    {-|
      'longestIncreasingLength xs' returns the length of the longest increasing
      subsequences in 'xs'.
    -}
    longestIncreasingLength :: APerm.APerm a -> Int
    longestIncreasingLength = APerm.size . longestIncreasing

    {-|
      'longestDecreasing xs' returns a longest decreasing subsequences in 'xs'.
    -}
    longestDecreasing :: APerm.APerm a -> APerm.APerm a
    longestDecreasing = APerm.fromList . APerm.Inner.Monotone.longestDecreasing . APerm.getList

    {-|
      'longestDecreasingLength xs' returns the length of the longest decreasing
      subsequences in 'xs'.
    -}
    longestDecreasingLength :: APerm.APerm a -> Int
    longestDecreasingLength = APerm.size . longestDecreasing
