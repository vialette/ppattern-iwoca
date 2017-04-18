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

    import qualified Data.Algorithm.PPattern.Geometry.APoint as APoint
    import qualified Data.Algorithm.PPattern.Perm            as Perm
    import qualified Data.Algorithm.PPattern.Perm.Inner.Sub  as Perm.Inner.Sub

    {-|
    -}
    sub :: Int -> Int -> Perm.Perm a -> Perm.Perm a
    sub xMin xMax = Perm.Perm . Perm.Inner.Sub.sub xMin xMax . Perm.getList

    {-|
    -}
    subRed :: (Ord a) => Int -> Int -> Perm a -> Perm a
    subRed xMin xMax = Perm.mk . fmap APoint.annotation . Perm.Inner.Sub.sub xMin xMax . getList

    {-|
      Construct all Perm prefixes of a permutation.
    -}
    prefixes :: Perm a -> [Perm a]
    prefixes = fmap Perm . List.Tools.prefixes . getList

    {-|
      Construct all reduced Perm prefixes of a permutation.
    -}
    prefixesRed :: (Ord a) => Perm a -> [Perm a]
    prefixesRed = fmap (mk . fmap APoint.annotation) . List.Tools.prefixes . getList

    {-|
      Construct all Perm suffixes of a permutation.
    -}
    suffixes :: Perm a -> [Perm a]
    suffixes = fmap Perm . List.Tools.suffixes . getList

    {-|
      Construct all reduced Perm suffixes of a permutation.
    -}
    suffixesRed :: (Ord a) => Perm a -> [Perm a]
    suffixesRed = fmap (mk . fmap APoint.annotation) . List.Tools.suffixes . getList

    {-|
      Construct all Perm factors of a permutation.
    -}
    factors :: Perm a -> [Perm a]
    factors = fmap Perm . List.Tools.factors . getList

    {-|
      Construct all reduced Perm factors of a permutation.
    -}
    factorsRed :: (Ord a) => Perm a -> [Perm a]
    factorsRed = fmap (mk . fmap APoint.annotation) . List.Tools.factors . getList
