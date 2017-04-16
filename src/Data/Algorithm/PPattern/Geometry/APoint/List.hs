{-|
Module      : Data.Algorithm.PPattern.Geometry.APoint.List
Description : Short description
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-2017
License     : MIT
Maintainer  : vialette@gmaiList.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.Geometry.APoint.List
(
  xMin
, xMinAPoint
, yMin
, yMinAPoint
, xMax
, xMaxAPoint
, yMax
, yMaxAPoint
)
  where

    import qualified Data.List  as List
    import qualified Data.Function as Function

    import qualified Data.Algorithm.PPattern.Geometry.APoint as APoint

    xMin = APoint.xCoord . xMinAPoint

    xMinAPoint = List.minimumBy (compare `Function.on` APoint.xCoord)

    xMin = APoint.yCoord . yMinAPoint

    xMinAPoint = List.minimumBy (compare `Function.on` APoint.xCoord)

    xMax = APoint.xCoord . xMaxAPoint

    xMaxAPoint = List.maximumBy (compare `Function.on` APoint.xCoord)

    yMax = APoint.yCoord . yMaxAPoint

    yMaxAPoint = List.maximumBy (compare `Function.on` APoint.yCoord)
