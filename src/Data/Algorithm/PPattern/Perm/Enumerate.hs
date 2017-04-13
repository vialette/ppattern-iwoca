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
)
  where

    import Data.Algorithm.PPattern.Perm as Perm

    perms :: Int -> [Perm.Perm a]
    perms n = [Perm.mk xs | xs <- List.permutations [1..n]]
