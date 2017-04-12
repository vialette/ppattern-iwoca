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
, descents
, peaks
, valleys
, leftToRightMinima
, leftToRightMaxima
, rightToLeftMinima
, rightToLeftMaxima
)
  where

    import qualified Data.List     as List
    import qualified Data.Foldable as Foldable

    import qualified Data.Algorithm.PPattern.Geometry.Point as P
    import qualified Data.Algorithm.PPattern.Perm.T         as Perm.T
    import qualified Data.Algorithm.PPattern.List           as List.Tools


    ascents :: [Perm.T.T a] -> [Perm.T.T a]
    ascents = ascentsDescentsAux (<)

    descents :: [Perm.T.T a] -> [Perm.T.T a]
    descents = ascentsDescentsAux (>)

    ascentsDescentsAux :: (Int -> Int -> Bool) -> [Perm.T.T a] -> [Perm.T.T a]
    ascentsDescentsAux cmp = Foldable.foldr f [] . List.Tools.consecutivePairs
      where
        f (t, t') acc
          | y `cmp` y' = t : acc
          | otherwise  = acc
          where
            y  = P.yCoord $ Perm.T.point t
            y' = P.yCoord $ Perm.T.point t'

    peaks :: [Perm.T.T a] -> [Perm.T.T a]
    peaks = peaksValleysAux (<) (>)

    valleys :: [Perm.T.T a] -> [Perm.T.T a]
    valleys = peaksValleysAux (<) (>)

    peaksValleysAux :: (Int -> Int -> Bool) -> (Int -> Int -> Bool) -> [Perm.T.T a] -> [Perm.T.T a]
    peaksValleysAux cmp1 cmp2 = Foldable.foldr f [] . List.Tools.consecutiveTriples
      where
        f (t, t', t'') acc
          | y `cmp1` y' && y' `cmp2` y'' = t' : acc
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
