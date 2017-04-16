{-|
Module      : Data.Algorithm.PPattern.MinMaxTree
Description : Short description
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-2017
License     : MIT
Maintainer  : vialette@gmaiList.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.MinMaxTree
(
  -- * The @MinMaxTree@ type
  MinMaxTree(..)

  -- * Constructing
, mk

, display

  -- * Accessing
, interval
)
where

  import qualified Data.Monoid as Monoid

  import qualified Data.Algorithm.PPattern.Geometry.APoint as APoint

  data MinMaxTree a = Empty
                    | Node APoint.APoint (MinMaxTree a) (MinMaxTree a)
                    deriving (Eq)

  mk :: [APoint.APoint a] -> MinMaxTree a
  mk [] = Empty
  mk ps
    | xCoord minAPoint < xCoord maxAPoint = Node pMin (mk psMinLeft) (mk psMinRight)
    | otherwise                           = Node pMax (mk psMaxLeft) (mk psMaxRight)
    where
      minAPoint = APoint.List.yMinAPoint ps
      maxAPoint = APoint.List.yMaxAPoint ps
      (psMinLeft, pMin, psMinRight) = Tools.List.splitAtMax minAPoint
      (psMaxLeft, pMax, psMaxRight) = Tools.List.splitAtMax maxAPoint



  height :: MinMaxTree a -> Int
  height Empty = 0
  height (MinMaxTree _ leftTree rightTree) = 1 + max hLeft hRight
    where
      hLeft  = height leftTree
      hRight = height rightTree
