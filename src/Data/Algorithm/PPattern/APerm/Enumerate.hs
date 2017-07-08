{-|
Module      : Data.Algorithm.PPattern.APerm.Enumerate
Description : Short description
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-2017
License     : MIT
Maintainer  : vialette@gmaiList.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.APerm.Enumerate
(
  perms
, perms'
, perms''
)
  where

    import Data.List as List
    import Data.Foldable as Foldable

    import Data.Algorithm.PPattern.APerm as APerm

    {-|
      Enumerate all permutations of length 'n'.

      >>> APerm.Enumerate.perms 0
      [[]]
      >>> APerm.Enumerate.perms 1
      [[1]]
      >>> APerm.Enumerate.perms 2
      [[1,2],[2,1]]
      >>> APerm.Enumerate.perms 3
      [[1,2,3],[2,1,3],[3,2,1],[2,3,1],[3,1,2],[1,3,2]]
    -}
    perms :: (Enum a, Num a, Ord a) => a -> [APerm.APerm a]
    perms n = [APerm.mk xs | xs <- List.permutations [1..n]]

    {-|
      Enumerate all permutations of a given list.

      >>> APerm.Enumerate.perms' ""
      [[]]
      >>> APerm.Enumerate.perms' "a"
      [[1]]
      >>> APerm.Enumerate.perms' "ab"
      [[1,2],[2,1]]
      >>> APerm.Enumerate.perms' "abc"
      [[1,2,3],[2,1,3],[3,2,1],[2,3,1],[3,1,2],[1,3,2]]
    -}
    perms' :: (Ord a) => [a] -> [APerm.APerm a]
    perms' xs = [APerm.mk xs' | xs' <- List.permutations xs]

    {-|
      Enumerate all permutations of a given foldable instance.

      >>> APerm.Enumerate.perms'' $ APerm.mk ""
      [[]]
      >>> APerm.Enumerate.perms'' $ APerm.mk "a"
      [[1]]
      >>> APerm.Enumerate.perms'' $ APerm.mk "ab"
      [[1,2],[2,1]]
      >>> APerm.Enumerate.perms'' $ APerm.mk "abc"
      [[1,2,3],[2,1,3],[3,2,1],[2,3,1],[3,1,2],[1,3,2]]
    -}
    perms'' :: (Foldable f, Ord a) => f a -> [APerm.APerm a]
    perms'' = perms' . Foldable.toList
