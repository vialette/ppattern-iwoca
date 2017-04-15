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

    {-|
    -}
    ascents :: Perm.Perm a -> APoint.APoint a
    ascents = Perm.T.Statistics.ascents . Perm.getList

    {-|
    -}
    ascentsPerm :: Perm.Perm a -> Perm.Perm a
    ascentsPerm = Perm.Perm . ascents

    {-|
    -}
    doubleAscents :: Perm.Perm a -> APoint.APoint a
    doubleAscents = Perm.T.Statistics.doubleAscents . Perm.getList

    {-|
    -}
    doubleAscentsPerm :: Perm.Perm a -> Perm.Perm a
    doubleAscentsPerm = Perm.Perm . doubleAscents

    {-|
    -}
    descents :: Perm.Perm a -> APoint.APoint a
    descents = Perm.T.Statistics.descents . Perm.getList

    {-|
    -}
    descentsPerm :: Perm.Perm a -> Perm.Perm a
    descentsPerm = Perm.Perm . descents

    {-|
    -}
    doubleDescents :: Perm.Perm a -> APoint.APoint a
    doubleDescents = Perm.T.Statistics.doubleDescents . Perm.getList

    {-|
    -}
    doubleDescentsPerm :: Perm.Perm a -> Perm.Perm a
    doubleDescentsPerm = Perm.Perm . doubleDescents

    {-|
    -}
    peaks :: Perm.Perm a -> APoint.APoint a
    peaks = Perm.T.Statistics.peaks . Perm.getList

    {-|
    -}
    peaksPerm :: Perm.Perm a -> Perm.Perm a
    peaksPerm = Perm.Perm . peaks

    {-|
    -}
    valleys :: Perm.Perm a -> APoint.APoint a
    valleys = Perm.T.Statistics.valleys . Perm.getList

    {-|
    -}
    valleysPerm :: Perm.Perm a -> Perm.Perm a
    valleysPerm = Perm.Perm . valleys

    {-|
    -}
    leftToRightMinima :: Perm.Perm a -> APoint.APoint a
    leftToRightMinima = Perm.T.Statistics.leftToRightMinima . Perm.getList

    {-|
    -}
    leftToRightMinimaPerm :: Perm.Perm a -> Perm.Perm a
    leftToRightMinimaPerm = Perm.Perm . leftToRightMinima

    {-|
    -}
    leftToRightMaxima :: Perm.Perm a -> APoint.APoint a
    leftToRightMaxima = Perm.T.Statistics.leftToRightMaxima . Perm.getList

    {-|
    -}
    leftToRightMaximaPerm :: Perm.Perm a -> Perm.Perm a
    leftToRightMaximaPerm = Perm.Perm . leftToRightMaxima

    {-|
    -}
    rightToLeftMinima :: Perm.Perm a -> APoint.APoint a
    rightToLeftMinima = Perm.T.Statistics.rightToLeftMinima . Perm.getList

    {-|
    -}
    rightToLeftMinimaPerm :: Perm.Perm a -> Perm.Perm a
    rightToLeftMinimaPerm = Perm.Perm . rightToLeftMinima

    {-|
    -}
    rightToLeftMaxima :: Perm.Perm a -> APoint.APoint a
    rightToLeftMaxima = Perm.T.Statistics.rightToLeftMaxima . Perm.getList

    {-|
    -}
    rightToLeftMaximaPerm :: Perm.Perm a -> Perm.Perm a
    rightToLeftMaximaPerm = Perm.Perm . rightToLeftMaxima
