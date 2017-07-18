{-|
Module      : Data.Algorithm.PPattern.Perm.Enumerate
Description : Short description
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-2017
License     : MIT
Maintainer  : vialette@gmaiList.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.Perm.Enumerate
(
  perms
, perms'
, perms''
)
  where

    import Data.List as List
    import Data.Foldable as Foldable

    import Data.Algorithm.PPattern.Perm as Perm

    {-|
      Enumerate all permutations of length 'n'.

      >>> Perm.Enumerate.perms 0
      [[]]
      >>> Perm.Enumerate.perms 1
      [[1]]
      >>> Perm.Enumerate.perms 2
      [[1,2],[2,1]]
      >>> Perm.Enumerate.perms 3
      [[1,2,3],[2,1,3],[3,2,1],[2,3,1],[3,1,2],[1,3,2]]
    -}
    perms :: (Enum a, Num a, Ord a) => a -> [Perm]
    perms n = [Perm.mk ys | ys <- List.permutations [1..n]]

    {-|
      Enumerate all permutations of a given list.

      >>> Perm.Enumerate.perms' ""
      [[]]
      >>> Perm.Enumerate.perms' "a"
      [[1]]
      >>> Perm.Enumerate.perms' "ab"
      [[1,2],[2,1]]
      >>> Perm.Enumerate.perms' "abc"
      [[1,2,3],[2,1,3],[3,2,1],[2,3,1],[3,1,2],[1,3,2]]
    -}
    perms' :: (Ord a) => [a] -> [Perm.Perm]
    perms' xs = [Perm.mk xs' | xs' <- List.permutations xs]

    {-|
      Enumerate all permutations of a given foldable instance.

      >>> Perm.Enumerate.perms'' $ Perm.mk ""
      [[]]
      >>> Perm.Enumerate.perms'' $ Perm.mk "a"
      [[1]]
      >>> Perm.Enumerate.perms'' $ Perm.mk "ab"
      [[1,2],[2,1]]
      >>> Perm.Enumerate.perms'' $ Perm.mk "abc"
      [[1,2,3],[2,1,3],[3,2,1],[2,3,1],[3,1,2],[1,3,2]]
    -}
    perms'' :: (Foldable f, Ord a) => f a -> [Perm.Perm]
    perms'' = perms' . Foldable.toList
