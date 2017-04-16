{-|
Module      : Data.Algorithm.PPattern.Inner.Monotone
Description : Short description
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-2017
License     : MIT
Maintainer  : vialette@gmaiList.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.Inner.Monotone
(
  isMonotone

, longestIncreasing
, longestDecreasing
)
  where

    import qualified Data.List          as List
    import qualified Data.Tuple         as Tuple
    import qualified Data.Foldable      as Foldable
    import qualified Data.IntMap.Strict as IntMap

    import qualified Data.Algorithm.Patience as Patience

    import qualified Data.Algorithm.PPattern.Geometry.Point      as Point
    import qualified Data.Algorithm.PPattern.Geometry.Point.List as P.List
    import qualified Data.Algorithm.PPattern.List                as List.Tools
    import qualified Data.Algorithm.PPattern.StackSort           as StackSort

    -- Auxiliary function for isIncreasing and isDecreasing
    isMonotone :: (Int -> Int -> Bool) -> [APoint.APoint a] -> Bool
    isMonotone cmp = go
      where
        go []  = True
        go [_] = True
        go ts   = Foldable.foldl f True $ List.Tools.consecutive2 ts
          where
            f acc (Perm.T.T (p, _), Perm.T.T (p', _)) = acc && Point.yCoord p `cmp` Point.yCoord p'


    longestIncreasing :: [APoint.APoint a] -> [APoint.APoint a]
    longestIncreasing = unformat . List.reverse . doSearch . format
      where
        format   = fmap (\ t@(Perm.T.T (p, _)) -> (Point.yCoord p, t))
        doSearch = Patience.longestIncreasing
        unformat = fmap Tuple.snd

    longestDecreasing :: [APoint.APoint a] -> [APoint.APoint a]
    longestDecreasing = unformat . doSearch . format
      where
        format   = List.reverse . fmap (\ t@(Perm.T.T (p, _)) -> (Point.yCoord p, t))
        doSearch = Patience.longestIncreasing
        unformat = fmap Tuple.snd
