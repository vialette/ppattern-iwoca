{-|
Module      : Data.Algorithm.PPattern.Perm.List
Description : Short description
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-2017
License     : MIT
Maintainer  : vialette@gmaiList.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.Perm.List
(
  -- *
  sub

, isMonotone

, longestIncreasing
, longestDecreasing

, reversal
, complement
, reversalComplement
, inverse
)
  where

    import qualified Data.List     as List
    import qualified Data.Tuple    as Tuple
    import qualified Data.Foldable as Foldable

    import qualified Data.Algorithm.Patience as Patience

    import qualified Data.Algorithm.PPattern.Perm.T              as Perm.T
    import qualified Data.Algorithm.PPattern.Geometry.Point      as P
    import qualified Data.Algorithm.PPattern.Geometry.Point.List as P.List
    import qualified Data.Algorithm.PPattern.Tools               as Tools

    --
    sub :: Int -> Int -> [Perm.T.T a] -> [Perm.T.T a]
    sub xMin xMax = aux
      where
        aux [] = []
        aux (t@(Perm.T.T (p, _)) : ts)
          | P.xCoord p < xMin = aux ts
          | P.xCoord p < xMax = t : aux ts
          | otherwise         = []

    -- Auxiliary function for isIncreasing and isDecreasing
    isMonotone :: (Int -> Int -> Bool) -> [Perm.T.T a] -> Bool
    isMonotone cmp = go
      where
        go []  = True
        go [_] = True
        go ts   = Foldable.foldl f True $ Tools.consecutivePairs ts
          where
            f acc (Perm.T.T (p, _), Perm.T.T (p', _)) = acc && P.yCoord p `cmp` P.yCoord p'


    longestIncreasing :: [Perm.T.T a] -> [Perm.T.T a]
    longestIncreasing = unformat . List.reverse . doSearch . format
      where
        format   = fmap (\ t@(Perm.T.T (p, _)) -> (P.yCoord p, t))
        doSearch = Patience.longestIncreasing
        unformat = fmap Tuple.snd

    longestDecreasing :: [Perm.T.T a] -> [Perm.T.T a]
    longestDecreasing = unformat . doSearch . format
      where
        format   = List.reverse . fmap (\ t@(Perm.T.T (p, _)) -> (P.yCoord p, t))
        doSearch = Patience.longestIncreasing
        unformat = fmap Tuple.snd

    reversal :: [Perm.T.T a] -> [Perm.T.T a]
    reversal ts = Foldable.foldl f [] ts
      where
        n = List.length ts

        f acc (Perm.T.T (p, a)) = Perm.T.mk p' a : acc
          where
            x = P.xCoord p
            x' = n + 1 - x

            p' = P.updateXCoord x' p

    complement :: [Perm.T.T a] -> [Perm.T.T a]
    complement ts = P.List.mkSequential . fmap f . fmap P.yCoord $ fmap Perm.T.point ts
      where
        n   = List.length ts
        f y = n+1-y

    reversalComplement :: [Perm.T.T a] -> [Perm.T.T a]
    reversalComplement ts = P.List.mkSequential . fmap f . List.reverse . fmap P.yCoord $ fmap Perm.T.point ts
      where
        n   = List.length ts
        f y = n+1-y

    inverse :: [Perm.T.T a] -> [Perm.T.T a]
    inverse = P.List.mkSequential . fmap Tuple.snd . List.sort . flip List.zip [1..] . fmap P.yCoord $ fmap Perm.T.point
