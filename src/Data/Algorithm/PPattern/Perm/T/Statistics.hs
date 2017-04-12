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

module Data.Algorithm.PPattern.Perm.T.Statistics
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

    import qualified Data.Algorithm.PPattern.Geometry.Point as P
    import qualified Data.Algorithm.PPattern.Perm.T         as Perm.T
    import qualified Data.Algorithm.PPattern.List           as List.Tools

    ascents :: [Perm.T.T a] -> [Perm.T.T a]
    ascents = fmap Tuple.fst . consecutive2 (<)

    doubleAscents :: [Perm.T.T a] -> [Perm.T.T a]
    doubleAscents = fmap (\ (t, _, _) -> t) . consecutive3 (<) (<)

    descents :: [Perm.T.T a] -> [Perm.T.T a]
    descents = fmap Tuple.fst . consecutive2 (>)

    doubleDescents :: [Perm.T.T a] -> [Perm.T.T a]
    doubleDescents = fmap (\ (t, _, _) -> t) . consecutive3 (>) (>)

    peaks :: [Perm.T.T a] -> [Perm.T.T a]
    peaks = fmap (\ (_, t, _) -> t) . consecutive3 (<) (>)

    valleys :: [Perm.T.T a] -> [Perm.T.T a]
    valleys = fmap (\ (_, t, _) -> t) . consecutive3 (>) (<)

    consecutive2 :: (Int -> Int -> Bool) -> [Perm.T.T a] -> [(Perm.T.T a, Perm.T.T a)]
    consecutive2 cmp = Foldable.foldr f [] . List.Tools.consecutive2
      where
        f (t, t') acc
          | y `cmp` y' = (t, t') : acc
          | otherwise  = acc
          where
            y  = P.yCoord $ Perm.T.point t
            y' = P.yCoord $ Perm.T.point t'

    consecutive3 :: (Int -> Int -> Bool) -> (Int -> Int -> Bool) -> [Perm.T.T a] -> [(Perm.T.T a, Perm.T.T a, Perm.T.T a)]
    consecutive3 cmp1 cmp2 = Foldable.foldr f [] . List.Tools.consecutive3
      where
        f (t, t', t'') acc
          | y `cmp1` y' && y' `cmp2` y'' = (t, t', t'') : acc
          | otherwise  = acc
          where
            y   = P.yCoord $ Perm.T.point t
            y'  = P.yCoord $ Perm.T.point t'
            y'' = P.yCoord $ Perm.T.point t''

    leftToRightMinima :: [Perm.T.T a] -> [Perm.T.T a]
    leftToRightMinima = leftToRightAux (>)

    leftToRightMaxima :: [Perm.T.T a] -> [Perm.T.T a]
    leftToRightMaxima = leftToRightAux (<)

    rightToLeftMinima :: [Perm.T.T a] -> [Perm.T.T a]
    rightToLeftMinima = List.reverse . leftToRightAux (>) . List.reverse

    rightToLeftMaxima :: [Perm.T.T a] -> [Perm.T.T a]
    rightToLeftMaxima = List.reverse . leftToRightAux (<) . List.reverse

    leftToRightAux :: (Int -> Int -> Bool) -> [Perm.T.T a] -> [Perm.T.T a]
    leftToRightAux cmp = aux []
      where
        aux acc []       = List.reverse acc
        aux []  (t : ts) = aux [t] ts
        aux acc@(t' : _) (t : ts)
          | y' `cmp` y = aux (t : acc) ts
          | otherwise  = aux acc ts
          where
            y  = P.yCoord $ Perm.T.point t
            y' = P.yCoord $ Perm.T.point t'
