{-|
Module      : Data.Algorithm.PPattern.Perm.T.Statistic
Description : Short description
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-2017
License     : MIT
Maintainer  : vialette@gmaiList.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.Perm.Statistics
(
-- * Statictics
  ascents
, doubleAscents
, descents
, doubleDescents
, peaks
, valleys
, leftToRightMinima
, leftToRightMaxima
, rightToLeftMinima
, rightToLeftMaxima
)
  where

    import qualified Data.Algorithm.PPattern.Perm                  as Perm
    import qualified Data.Algorithm.PPattern.Perm.Inner.Statistics as Perm.Inner.Statistics

    {-|
    -}
    ascents :: Perm.Perm a -> Perm.Perm a
    ascents = Perm.Perm . Perm.T.Statistics.ascents . Perm.getList

    {-|
    -}
    doubleAscents :: Perm.Perm a -> Perm.Perm a
    doubleAscents = Perm.Perm . Perm.T.Statistics.doubleAscents . Perm.getList

    {-|
    -}
    descents :: Perm.Perm a -> Perm.Perm a
    descents = Perm.Perm . Perm.T.Statistics.descents . Perm.getList

    {-|
    -}
    doubleDescents :: Perm.Perm a -> Perm.Perm a
    doubleDescents = Perm.Perm . Perm.T.Statistics.doubleDescents . Perm.getList

    {-|
    -}
    peaks :: Perm.Perm a -> Perm.Perm a
    peaks = Perm.Perm . Perm.T.Statistics.peaks . Perm.getList

    {-|
    -}
    valleys :: Perm.Perm a -> Perm.Perm a
    valleys = Perm.Perm . Perm.T.Statistics.valleys . Perm.getList

    {-|
    -}
    leftToRightMinima :: Perm.Perm a -> Perm.Perm a
    leftToRightMinima = Perm.Perm . Perm.T.Statistics.leftToRightMinima . Perm.getList

    {-|
    -}
    leftToRightMaxima :: Perm.Perm a -> Perm.Perm a
    leftToRightMaxima = Perm.Perm . Perm.T.Statistics.leftToRightMaxima . Perm.getList

    {-|
    -}
    rightToLeftMinima :: Perm.Perm a -> Perm.Perm a
    rightToLeftMinima = Perm.Perm . Perm.T.Statistics.rightToLeftMinima . Perm.getList

    {-|
    -}
    rightToLeftMaxima :: Perm.Perm a -> Perm.Perm a
    rightToLeftMaxima = Perm.Perm . Perm.T.Statistics.rightToLeftMaxima . Perm.getList
