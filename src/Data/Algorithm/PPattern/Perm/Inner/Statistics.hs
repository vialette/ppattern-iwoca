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
        f (t, t') acc
          | y `cmp` y' = (t, t') : acc
          | otherwise  = acc
          where
            y  = Point.yCoord $ Perm.T.point t
            y' = Point.yCoord $ Perm.T.point t'

    {-|
    -}
    consecutive3 :: (Int -> Int -> Bool) -> (Int -> Int -> Bool) -> [APoint.APoint a] -> [(APoint.APoint a, APoint.APoint a, APoint.APoint a)]
    consecutive3 cmp1 cmp2 = Foldable.foldr f [] . List.Tools.consecutive3
      where
        f (t, t', t'') acc
          | y `cmp1` y' && y' `cmp2` y'' = (t, t', t'') : acc
          | otherwise  = acc
          where
            y   = Point.yCoord $ Perm.T.point t
            y'  = Point.yCoord $ Perm.T.point t'
            y'' = Point.yCoord $ Perm.T.point t''

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
        aux []  (t : ts) = aux [t] ts
        aux acc@(t' : _) (t : ts)
          | y' `cmp` y = aux (t : acc) ts
          | otherwise  = aux acc ts
          where
            y  = Point.yCoord $ Perm.T.point t
            y' = Point.yCoord $ Perm.T.point t'
