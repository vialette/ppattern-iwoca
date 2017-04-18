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

module Data.Algorithm.PPattern.Perm.Inner.Statistics
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

    import qualified Data.List     as List
    import qualified Data.Tuple    as Tuple
    import qualified Data.Foldable as Foldable

    import qualified Data.Algorithm.PPattern.Geometry.Point  as Point
    import qualified Data.Algorithm.PPattern.Geometry.APoint as APoint
    import qualified Data.Algorithm.PPattern.List            as List.Tools

    {-|
    -}
    ascents :: [APoint.APoint a] -> [APoint.APoint a]
    ascents = fmap Tuple.fst . consecutive2 (<)

    {-|
    -}
    doubleAscents :: [APoint.APoint a] -> [APoint.APoint a]
    doubleAscents = fmap (\ (t, _, _) -> t) . consecutive3 (<) (<)

    {-|
    -}
    descents :: [APoint.APoint a] -> [APoint.APoint a]
    descents = fmap Tuple.fst . consecutive2 (>)

    {-|
    -}
    doubleDescents :: [APoint.APoint a] -> [APoint.APoint a]
    doubleDescents = fmap (\ (t, _, _) -> t) . consecutive3 (>) (>)

    {-|
    -}
    peaks :: [APoint.APoint a] -> [APoint.APoint a]
    peaks = fmap (\ (_, t, _) -> t) . consecutive3 (<) (>)

    {-|
    -}
    valleys :: [APoint.APoint a] -> [APoint.APoint a]
    valleys = fmap (\ (_, t, _) -> t) . consecutive3 (>) (<)

    {-|
    -}
    consecutive2 :: (Int -> Int -> Bool) -> [APoint.APoint a] -> [(APoint.APoint a, APoint.APoint a)]
    consecutive2 cmp = Foldable.foldr f [] . List.Tools.consecutive2
      where
        f (ap, ap') acc
          | y `cmp` y' = (ap, ap') : acc
          | otherwise  = acc
          where
            y  = Point.yCoord $ APoint.point ap
            y' = Point.yCoord $ APoint.point ap'

    {-|
    -}
    consecutive3 :: (Int -> Int -> Bool) -> (Int -> Int -> Bool) -> [APoint.APoint a] -> [(APoint.APoint a, APoint.APoint a, APoint.APoint a)]
    consecutive3 cmp1 cmp2 = Foldable.foldr f [] . List.Tools.consecutive3
      where
        f (ap, ap', ap'') acc
          | y `cmp1` y' && y' `cmp2` y'' = (ap, ap', ap'') : acc
          | otherwise  = acc
          where
            y   = Point.yCoord $ APoint.point ap
            y'  = Point.yCoord $ APoint.point ap'
            y'' = Point.yCoord $ APoint.point ap''

    {-|
    -}
    leftToRightMinima :: [APoint.APoint a] -> [APoint.APoint a]
    leftToRightMinima = leftToRightAux (>)

    {-|
    -}
    leftToRightMaxima :: [APoint.APoint a] -> [APoint.APoint a]
    leftToRightMaxima = leftToRightAux (<)

    {-|
    -}
    rightToLeftMinima :: [APoint.APoint a] -> [APoint.APoint a]
    rightToLeftMinima = List.reverse . leftToRightAux (>) . List.reverse

    {-|
    -}
    rightToLeftMaxima :: [APoint.APoint a] -> [APoint.APoint a]
    rightToLeftMaxima = List.reverse . leftToRightAux (<) . List.reverse

    {-|
    -}
    leftToRightAux :: (Int -> Int -> Bool) -> [APoint.APoint a] -> [APoint.APoint a]
    leftToRightAux cmp = aux []
      where
        aux acc []       = List.reverse acc
        aux []  (ap : aps) = aux [ap] aps
        aux acc@(ap' : _) (ap : aps)
          | y' `cmp` y = aux (ap : acc) aps
          | otherwise  = aux acc aps
          where
            y  = Point.yCoord $ APoint.point ap
            y' = Point.yCoord $ APoint.point ap'
