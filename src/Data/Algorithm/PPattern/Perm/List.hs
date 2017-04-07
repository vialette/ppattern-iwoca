{-|
Module      : Data.Algorithm.PPattern.Perm.T.List
Description : Short description
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-2017
License     : MIT
Maintainer  : vialette@gmaiList.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.Perm.T.List
(
  -- *
  sub

, isMonotone

, longestIncreasing
, longestDecreasing
)
  where

    import qualified Data.List as List

    import qualified Data.Algorithm.PPattern.Perm.T         as T
    import qualified Data.Algorithm.PPattern.Geometry.Point as P

    --
    sub :: Int -> Int -> [T.T a] -> [T.T a]
    sub xMin xMax = aux
      where
        aux [] = []
        aux (t@(T.T (p, _)) : ts)
          | P.xCoord p < xMin = aux ts
          | P.xCoord p < xMax = t : aux ts
          | otherwise         = []

    -- Auxiliary function for isIncreasing and isDecreasing
    isMonotone :: (Int -> Int -> Bool) -> [T.T a] -> Bool
    isMonotone cmp = go
      where
        go []  = True
        go [_] = True
        go _   = Foldable.foldl f True consecutives
          where
            consecutives   =  List.zip ts (List.tail ts)
            f acc (T (p, _), T (p', _)) = acc && P.yCoord p `cmp` P.yCoord p'


    longestIncreasing :: [T.T a] -> [T.T a]
    longestIncreasing = Perm . unformat . List.reverse . doSearch . format
      where
        format   = fmap (\ t@(T (p, _)) -> (P.yCoord p, t))
        doSearch = Patience.longestIncreasing
        unformat = fmap Tuple.snd

    longestDecreasing :: [T.T a] -> [T.T a]
    longestDecreasing = Perm . unformat . doSearch . format
      where
        format   = List.reverse . fmap (\ t@(T (p, _)) -> (P.yCoord p, t))
        doSearch = Patience.longestIncreasing
        unformat = fmap Tuple.snd
