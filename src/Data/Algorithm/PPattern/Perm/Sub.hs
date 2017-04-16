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
