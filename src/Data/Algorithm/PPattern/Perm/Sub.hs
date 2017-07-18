{-|
Module      : Data.Algorithm.PPattern.Perm.Sub
Description : Short description
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-2017
License     : MIT
Maintainer  : vialette@gmaiList.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.Perm.Sub
(
  -- *
  sub
, subRed
, prefixes

  -- *
, prefixesRed
, suffixes
, suffixesRed
, factors
, factorsRed
)
  where

    import qualified Data.Algorithm.PPattern.Perm           as Perm
    import qualified Data.Algorithm.PPattern.Perm.Inner.Sub as Perm.Inner.Sub
    import qualified Data.Algorithm.PPattern.List           as List.Tools

    {-|
    -}
    sub :: Int -> Int -> Perm.Perm -> Perm.Perm
    sub xMin xMax = Perm.fromList . Perm.Inner.Sub.sub xMin xMax . Perm.getList

    {-|
    -}
    subRed :: Int -> Int -> Perm.Perm -> Perm.Perm
    subRed xMin xMax = Perm.mk . Perm.Inner.Sub.sub xMin xMax . Perm.getList

    {-|
      Construct all Perm prefixes of a Permutation.
    -}
    prefixes :: Perm.Perm -> [Perm.Perm]
    prefixes = fmap Perm.fromList . List.Tools.prefixes . Perm.getList

    {-|
      Construct all reduced Perm prefixes of a Permutation.
    -}
    prefixesRed :: Perm.Perm -> [Perm.Perm]
    prefixesRed = fmap Perm.mk . List.Tools.prefixes . Perm.getList

    {-|
      Construct all Perm suffixes of a Permutation.
    -}
    suffixes :: Perm.Perm -> [Perm.Perm]
    suffixes = fmap Perm.fromList . List.Tools.suffixes . Perm.getList

    {-|
      Construct all reduced Perm suffixes of a Permutation.
    -}
    suffixesRed :: Perm.Perm -> [Perm.Perm]
    suffixesRed = fmap Perm.mk . List.Tools.suffixes . Perm.getList

    {-|
      Construct all Perm factors of a Permutation.
    -}
    factors :: Perm.Perm -> [Perm.Perm]
    factors = fmap Perm.fromList . List.Tools.factors . Perm.getList

    {-|
      Construct all reduced Perm factors of a Permutation.
    -}
    factorsRed :: Perm.Perm -> [Perm.Perm]
    factorsRed = fmap Perm.mk . List.Tools.factors . Perm.getList
