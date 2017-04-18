{-|
Module      : Data.Algorithm.PPattern.APerm.Sub
Description : Short description
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-2017
License     : MIT
Maintainer  : vialette@gmaiList.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.APerm.Sub
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
    import qualified Data.Algorithm.PPattern.APerm            as APerm
    import qualified Data.Algorithm.PPattern.APerm.Inner.Sub  as APerm.Inner.Sub
    import qualified Data.Algorithm.PPattern.List            as List.Tools

    {-|
    -}
    sub :: Int -> Int -> APerm.APerm a -> APerm.APerm a
    sub xMin xMax = APerm.fromList . APerm.Inner.Sub.sub xMin xMax . APerm.getList

    {-|
    -}
    subRed :: (Ord a) => Int -> Int -> APerm.APerm a -> APerm.APerm a
    subRed xMin xMax = APerm.mk . fmap APoint.annotation . APerm.Inner.Sub.sub xMin xMax . APerm.getList

    {-|
      Construct all APerm prefixes of a APermutation.
    -}
    prefixes :: APerm.APerm a -> [APerm.APerm a]
    prefixes = fmap APerm.fromList . List.Tools.prefixes . APerm.getList

    {-|
      Construct all reduced APerm prefixes of a APermutation.
    -}
    prefixesRed :: (Ord a) => APerm.APerm a -> [APerm.APerm a]
    prefixesRed = fmap (APerm.mk . fmap APoint.annotation) . List.Tools.prefixes . APerm.getList

    {-|
      Construct all APerm suffixes of a APermutation.
    -}
    suffixes :: APerm.APerm a -> [APerm.APerm a]
    suffixes = fmap APerm.fromList . List.Tools.suffixes . APerm.getList

    {-|
      Construct all reduced APerm suffixes of a APermutation.
    -}
    suffixesRed :: (Ord a) => APerm.APerm a -> [APerm.APerm a]
    suffixesRed = fmap (APerm.mk . fmap APoint.annotation) . List.Tools.suffixes . APerm.getList

    {-|
      Construct all APerm factors of a APermutation.
    -}
    factors :: APerm.APerm a -> [APerm.APerm a]
    factors = fmap APerm.fromList . List.Tools.factors . APerm.getList

    {-|
      Construct all reduced APerm factors of a APermutation.
    -}
    factorsRed :: (Ord a) => APerm.APerm a -> [APerm.APerm a]
    factorsRed = fmap (APerm.mk . fmap APoint.annotation) . List.Tools.factors . APerm.getList
