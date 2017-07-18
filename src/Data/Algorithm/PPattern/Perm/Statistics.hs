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
, ascentsPerm
, doubleAscents
, doubleAscentsPerm
, descents
, descentsPerm
, doubleDescents
, doubleDescentsPerm
, peaks
, peaksPerm
, valleys
, valleysPerm

-- * Minima and maxima
, leftToRightMinima
, leftToRightMinimaPerm
, leftToRightMaxima
, leftToRightMaximaPerm
, rightToLeftMinima
, rightToLeftMinimaPerm
, rightToLeftMaxima
, rightToLeftMaximaPerm
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
    ascentsPerm :: Perm.Perm -> Perm.Perm
    ascentsPerm = Perm.fromList . ascents

    {-|
    -}
    doubleAscents :: Perm.Perm -> [Point.Point]
    doubleAscents = Perm.Inner.Statistics.doubleAscents . Perm.getList

    {-|
    -}
    doubleAscentsPerm :: Perm.Perm -> Perm.Perm
    doubleAscentsPerm = Perm.fromList . doubleAscents

    {-|
    -}
    descents :: Perm.Perm -> [Point.Point]
    descents = Perm.Inner.Statistics.descents . Perm.getList

    {-|
    -}
    descentsPerm :: Perm.Perm -> Perm.Perm
    descentsPerm = Perm.fromList . descents

    {-|
    -}
    doubleDescents :: Perm.Perm -> [Point.Point]
    doubleDescents = Perm.Inner.Statistics.doubleDescents . Perm.getList

    {-|
    -}
    doubleDescentsPerm :: Perm.Perm -> Perm.Perm
    doubleDescentsPerm = Perm.fromList . doubleDescents

    {-|
    -}
    peaks :: Perm.Perm -> [Point.Point]
    peaks = Perm.Inner.Statistics.peaks . Perm.getList

    {-|
    -}
    peaksPerm :: Perm.Perm -> Perm.Perm
    peaksPerm = Perm.fromList . peaks

    {-|
    -}
    valleys :: Perm.Perm -> [Point.Point]
    valleys = Perm.Inner.Statistics.valleys . Perm.getList

    {-|
    -}
    valleysPerm :: Perm.Perm -> Perm.Perm
    valleysPerm = Perm.fromList . valleys

    {-|
    -}
    leftToRightMinima :: Perm.Perm -> [Point.Point]
    leftToRightMinima = Perm.Inner.Statistics.leftToRightMinima . Perm.getList

    {-|
    -}
    leftToRightMinimaPerm :: Perm.Perm -> Perm.Perm
    leftToRightMinimaPerm = Perm.fromList . leftToRightMinima

    {-|
    -}
    leftToRightMaxima :: Perm.Perm -> [Point.Point]
    leftToRightMaxima = Perm.Inner.Statistics.leftToRightMaxima . Perm.getList

    {-|
    -}
    leftToRightMaximaPerm :: Perm.Perm -> Perm.Perm
    leftToRightMaximaPerm = Perm.fromList . leftToRightMaxima

    {-|
    -}
    rightToLeftMinima :: Perm.Perm -> [Point.Point]
    rightToLeftMinima = Perm.Inner.Statistics.rightToLeftMinima . Perm.getList

    {-|
    -}
    rightToLeftMinimaPerm :: Perm.Perm -> Perm.Perm
    rightToLeftMinimaPerm = Perm.fromList . rightToLeftMinima

    {-|
    -}
    rightToLeftMaxima :: Perm.Perm -> [Point.Point]
    rightToLeftMaxima = Perm.Inner.Statistics.rightToLeftMaxima . Perm.getList

    {-|
    -}
    rightToLeftMaximaPerm :: Perm.Perm -> Perm.Perm
    rightToLeftMaximaPerm = Perm.fromList . rightToLeftMaxima
