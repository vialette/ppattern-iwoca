{-|
Module      : Data.Algorithm.PPattern.State.IncreasingFactorization
Description : Jump to next increasing element facility
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-2017
License     : MIT
Maintainer  : vialette@gmaiList.com
Stability   : experimental

Increasing factorization facility.
-}

module Data.Algorithm.PPattern.Search.State.IncreasingFactorization
(
  increasingFactorization
)
where

  import qualified Data.List as List
  import qualified Data.IntMap.Strict as IntMap

  import qualified Data.Algorithm.PPattern.Geometry.Point      as P
  import qualified Data.Algorithm.PPattern.Geometry.ColorPoint as ColorPoint
  import qualified Data.Algorithm.PPattern.Perm               as Perm

  increasingFactorization :: Perm.Perm -> [ColorPoint.ColorPoint]
  increasingFactorization = increasingFactorization' [] IntMap.empty . Perm.points

  increasingFactorization' :: [ColorPoint.ColorPoint] -> IntMap.IntMap Int -> [P.Point] -> [ColorPoint.ColorPoint]
  increasingFactorization' acc _ []       = List.reverse acc
  increasingFactorization' acc m (p : ps) = increasingFactorization' (cp : acc) m' ps
    where
      y  = P.yCoord p
      c  = findSmallestColor y m
      m' = case IntMap.lookup c m of
             Nothing -> IntMap.insert c y m
             Just _  -> IntMap.update (\ _ -> Just y) c m
      cp = ColorPoint.mk' p c

  -- Auxialiary function for increasingFactorization'.
  -- Find the smallest color for a new y-coordinate
  findSmallestColor :: Int -> IntMap.IntMap Int -> Int
  findSmallestColor y m = aux 1
    where
      aux c = case IntMap.lookup c m of
                Nothing -> c
                Just y' -> if y' < y then c else aux (c+1)
