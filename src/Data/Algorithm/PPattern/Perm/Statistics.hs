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
-- * Ascents and descents
  ascents
, doubleAscents
, descents
, doubleDescents
, peaks
, valleys

-- * Minima and maxima
, leftToRightMinima
, leftToRightMaxima
, rightToLeftMinima
, rightToLeftMaxima
)
  where

    import qualified Data.Algorithm.PPattern.Perm                  as Perm
    import qualified Data.Algorithm.PPattern.Perm.Inner.Statistics as Perm.Inner.Statistics
    import qualified Data.Algorithm.PPattern.Geometry.Point        as Point

    {-|
    -}
    ascents :: Perm.Perm -> [Point.Point]
    ascents = Perm.Inner.Statistics.ascents . Perm.getList

    {-|
    -}
    doubleAscents :: Perm.Perm -> [Point.Point]
    doubleAscents = Perm.Inner.Statistics.doubleAscents . Perm.getList

    {-|
    -}
    descents :: Perm.Perm -> [Point.Point]
    descents = Perm.Inner.Statistics.descents . Perm.getList

    {-|
    -}
    doubleDescents :: Perm.Perm -> [Point.Point]
    doubleDescents = Perm.Inner.Statistics.doubleDescents . Perm.getList

    {-|
    -}
    peaks :: Perm.Perm -> [Point.Point]
    peaks = Perm.Inner.Statistics.peaks . Perm.getList

    {-|
    -}
    valleys :: Perm.Perm -> [Point.Point]
    valleys = Perm.Inner.Statistics.valleys . Perm.getList

    {-|
    -}
    leftToRightMinima :: Perm.Perm -> [Point.Point]
    leftToRightMinima = Perm.Inner.Statistics.leftToRightMinima . Perm.getList

    {-|
    -}
    leftToRightMaxima :: Perm.Perm -> [Point.Point]
    leftToRightMaxima = Perm.Inner.Statistics.leftToRightMaxima . Perm.getList

    {-|
    -}
    rightToLeftMinima :: Perm.Perm -> [Point.Point]
    rightToLeftMinima = Perm.Inner.Statistics.rightToLeftMinima . Perm.getList

    {-|
    -}
    rightToLeftMaxima :: Perm.Perm -> [Point.Point]
    rightToLeftMaxima = Perm.Inner.Statistics.rightToLeftMaxima . Perm.getList
