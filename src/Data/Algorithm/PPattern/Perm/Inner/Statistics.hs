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
    import qualified Data.Algorithm.PPattern.List            as List.Tools

    {-|
    -}
    ascents :: [Point.Point] -> [Point.Point]
    ascents = fmap Tuple.fst . consecutive2 (<)

    {-|
    -}
    doubleAscents :: [Point.Point] -> [Point.Point]
    doubleAscents = fmap (\ (t, _, _) -> t) . consecutive3 (<) (<)

    {-|
    -}
    descents :: [Point.Point] -> [Point.Point]
    descents = fmap Tuple.fst . consecutive2 (>)

    {-|
    -}
    doubleDescents :: [Point.Point] -> [Point.Point]
    doubleDescents = fmap (\ (t, _, _) -> t) . consecutive3 (>) (>)

    {-|
    -}
    peaks :: [Point.Point] -> [Point.Point]
    peaks = fmap (\ (_, t, _) -> t) . consecutive3 (<) (>)

    {-|
    -}
    valleys :: [Point.Point] -> [Point.Point]
    valleys = fmap (\ (_, t, _) -> t) . consecutive3 (>) (<)

    {-|
    -}
    consecutive2 :: (Int -> Int -> Bool) -> [Point.Point] -> [(Point.Point, Point.Point)]
    consecutive2 cmp = Foldable.foldr f [] . List.Tools.consecutive2
      where
        f (p, p') acc
          | y `cmp` y' = (p, p') : acc
          | otherwise  = acc
          where
            y  = Point.yCoord p
            y' = Point.yCoord p'

    {-|
    -}
    consecutive3 :: (Int -> Int -> Bool) -> (Int -> Int -> Bool) -> [Point.Point] -> [(Point.Point, Point.Point, Point.Point)]
    consecutive3 cmp1 cmp2 = Foldable.foldr f [] . List.Tools.consecutive3
      where
        f (p, p', p'') acc
          | y `cmp1` y' && y' `cmp2` y'' = (p, p', p'') : acc
          | otherwise  = acc
          where
            y   = Point.yCoord p
            y'  = Point.yCoord p'
            y'' = Point.yCoord p''

    {-|
    -}
    leftToRightMinima :: [Point.Point] -> [Point.Point]
    leftToRightMinima = leftToRightAux (>)

    {-|
    -}
    leftToRightMaxima :: [Point.Point] -> [Point.Point]
    leftToRightMaxima = leftToRightAux (<)

    {-|
    -}
    rightToLeftMinima :: [Point.Point] -> [Point.Point]
    rightToLeftMinima = List.reverse . leftToRightAux (>) . List.reverse

    {-|
    -}
    rightToLeftMaxima :: [Point.Point] -> [Point.Point]
    rightToLeftMaxima = List.reverse . leftToRightAux (<) . List.reverse

    {-|
    -}
    leftToRightAux :: (Int -> Int -> Bool) -> [Point.Point] -> [Point.Point]
    leftToRightAux cmp = aux []
      where
        aux acc []       = List.reverse acc
        aux []  (p : ps) = aux [p] ps
        aux acc@(p' : _) (p : ps)
          | y' `cmp` y = aux (p : acc) ps
          | otherwise  = aux acc ps
          where
            y  = Point.yCoord p
            y' = Point.yCoord p'
