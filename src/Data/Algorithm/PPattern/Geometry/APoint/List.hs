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

    -- Return the min x-coordinate of a list of APoint
    xMin = APoint.xCoord . xMinAPoint

    -- Return the APoint with min x-coordinate of a list of APoint
    xMinAPoint = List.minimumBy (compare `Function.on` APoint.xCoord)

    -- Return the min y-coordinate of a list of APoint
    yMin = APoint.yCoord . yMinAPoint

    -- Return the APoint with min y-coordinate of a list of APoint
    yMinAPoint = List.minimumBy (compare `Function.on` APoint.xCoord)

    -- Return the max x-coordinate of a list of APoint
    xMax = APoint.xCoord . xMaxAPoint

    -- Return the APoint with max x-coordinate of a list of APoint
    xMaxAPoint = List.maximumBy (compare `Function.on` APoint.xCoord)

    -- Return the max y-coordinate of a list of APoint
    yMax = APoint.yCoord . yMaxAPoint

    -- Return the APoint with max y-coordinate of a list of APoint
    yMaxAPoint = List.maximumBy (compare `Function.on` APoint.yCoord)
