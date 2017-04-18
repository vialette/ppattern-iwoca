{-|
Module      : Data.Algorithm.PPattern.APerm.T.Statistic
Description : Short description
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-2017
License     : MIT
Maintainer  : vialette@gmaiList.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.APerm.Statistics
(
-- * Ascents and descents
  ascents
, ascentsAPerm
, doubleAscents
, doubleAscentsAPerm
, descents
, descentsAPerm
, doubleDescents
, doubleDescentsAPerm
, peaks
, peaksAPerm
, valleys
, valleysAPerm

-- * Minima and maxima
, leftToRightMinima
, leftToRightMinimaAPerm
, leftToRightMaxima
, leftToRightMaximaAPerm
, rightToLeftMinima
, rightToLeftMinimaAPerm
, rightToLeftMaxima
, rightToLeftMaximaAPerm
)
  where

    import qualified Data.Algorithm.PPattern.APerm                  as APerm
    import qualified Data.Algorithm.PPattern.APerm.Inner.Statistics as APerm.Inner.Statistics
    import qualified Data.Algorithm.PPattern.Geometry.APoint       as APoint

    {-|
    -}
    ascents :: APerm.APerm a -> [APoint.APoint a]
    ascents = APerm.Inner.Statistics.ascents . APerm.getList

    {-|
    -}
    ascentsAPerm :: APerm.APerm a -> APerm.APerm a
    ascentsAPerm = APerm.fromList . ascents

    {-|
    -}
    doubleAscents :: APerm.APerm a -> [APoint.APoint a]
    doubleAscents = APerm.Inner.Statistics.doubleAscents . APerm.getList

    {-|
    -}
    doubleAscentsAPerm :: APerm.APerm a -> APerm.APerm a
    doubleAscentsAPerm = APerm.fromList . doubleAscents

    {-|
    -}
    descents :: APerm.APerm a -> [APoint.APoint a]
    descents = APerm.Inner.Statistics.descents . APerm.getList

    {-|
    -}
    descentsAPerm :: APerm.APerm a -> APerm.APerm a
    descentsAPerm = APerm.fromList . descents

    {-|
    -}
    doubleDescents :: APerm.APerm a -> [APoint.APoint a]
    doubleDescents = APerm.Inner.Statistics.doubleDescents . APerm.getList

    {-|
    -}
    doubleDescentsAPerm :: APerm.APerm a -> APerm.APerm a
    doubleDescentsAPerm = APerm.fromList . doubleDescents

    {-|
    -}
    peaks :: APerm.APerm a -> [APoint.APoint a]
    peaks = APerm.Inner.Statistics.peaks . APerm.getList

    {-|
    -}
    peaksAPerm :: APerm.APerm a -> APerm.APerm a
    peaksAPerm = APerm.fromList . peaks

    {-|
    -}
    valleys :: APerm.APerm a -> [APoint.APoint a]
    valleys = APerm.Inner.Statistics.valleys . APerm.getList

    {-|
    -}
    valleysAPerm :: APerm.APerm a -> APerm.APerm a
    valleysAPerm = APerm.fromList . valleys

    {-|
    -}
    leftToRightMinima :: APerm.APerm a -> [APoint.APoint a]
    leftToRightMinima = APerm.Inner.Statistics.leftToRightMinima . APerm.getList

    {-|
    -}
    leftToRightMinimaAPerm :: APerm.APerm a -> APerm.APerm a
    leftToRightMinimaAPerm = APerm.fromList . leftToRightMinima

    {-|
    -}
    leftToRightMaxima :: APerm.APerm a -> [APoint.APoint a]
    leftToRightMaxima = APerm.Inner.Statistics.leftToRightMaxima . APerm.getList

    {-|
    -}
    leftToRightMaximaAPerm :: APerm.APerm a -> APerm.APerm a
    leftToRightMaximaAPerm = APerm.fromList . leftToRightMaxima

    {-|
    -}
    rightToLeftMinima :: APerm.APerm a -> [APoint.APoint a]
    rightToLeftMinima = APerm.Inner.Statistics.rightToLeftMinima . APerm.getList

    {-|
    -}
    rightToLeftMinimaAPerm :: APerm.APerm a -> APerm.APerm a
    rightToLeftMinimaAPerm = APerm.fromList . rightToLeftMinima

    {-|
    -}
    rightToLeftMaxima :: APerm.APerm a -> [APoint.APoint a]
    rightToLeftMaxima = APerm.Inner.Statistics.rightToLeftMaxima . APerm.getList

    {-|
    -}
    rightToLeftMaximaAPerm :: APerm.APerm a -> APerm.APerm a
    rightToLeftMaximaAPerm = APerm.fromList . rightToLeftMaxima
