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
)
  where

    import Data.List as List

    import Data.Algorithm.PPattern.APerm as APerm

    {-|
    -}
    perms :: (Enum a, Num a, Ord a) => a -> [APerm.APerm a]
    perms n = [APerm.mk xs | xs <- List.permutations [1..n]]

    {-|
    -}
    perms' :: (Ord a) => [a] -> [APerm.APerm a]
    perms' xs = [APerm.mk xs' | xs' <- List.permutations xs]
