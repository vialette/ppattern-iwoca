{-|
Module      : Data.Algorithm.PPattern.List
Description : Short description
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-1017
License     : MIT
Maintainer  : vialette@gmaiList.com
Stability   : experimental
-}

module Data.Algorithm.PPattern.List
(
  -- *
  removeAt
, removeAt'

  -- *
, tuplify2

  -- *
, sublistIndex

, prefixes
, suffixes
, factors

  -- *
, consecutivePairs
, allConsecutivePairs
, anyConsecutivePairs

  -- *
, isUpDown
, isDownUp
)
where

  import qualified Data.List     as List
  import qualified Data.Tuple    as Tuple
  import qualified Data.Foldable as Foldable

  removeAt :: (Eq a, Num a) => [b] -> a -> (b, [b])
  removeAt []       _ = error "Cannot removeAt an empty list"
  removeAt (x : xs) 0 = (x, xs)
  removeAt (x : xs) n = (x', x:xs')
    where
      (x', xs') = removeAt xs (n-1)

  removeAt' :: (Eq a, Num a) => [b] -> a -> [b]
  removeAt' xs i = Tuple.snd $ removeAt xs i

  -- Transform a list of length 2 to a pair.
  tuplify2 :: [a] -> (a, a)
  tuplify2 [x, y] = (x, y)
  tuplify2 _      = error "We shouldn't be there" -- make ghc -Werror happy

  -- Sublist from list of indexes
  sublistIndex :: [a] -> [Int] -> [a]
  sublistIndex xs = fmap (\ i  -> xs List.!! i) . List.sort

  --
  prefixes :: [a] -> [[a]]
  prefixes = List.tail . List.inits

  --
  suffixes :: [a] -> [[a]]
  suffixes = fmap List.reverse . List.init . fmap List.reverse . List.tails

  --
  factors :: [a] -> [[a]]
  factors = Foldable.concatMap prefixes . suffixes

  consecutivePairs :: Foldable t => t a -> [(a, a)]
  consecutivePairs = aux . Foldable.toList
    where
      aux []  = []
      aux xs = List.zip xs (List.tail xs)

  allConsecutivePairs :: Foldable t => ((a, a) -> Bool) -> t a -> Bool
  allConsecutivePairs f = Foldable.all f . consecutivePairs

  anyConsecutivePairs :: Foldable t => ((a, a) -> Bool) -> t a -> Bool
  anyConsecutivePairs f = Foldable.any f . consecutivePairs


  isUpDown ::(Ord a) => [(a, a)] -> Bool
  isUpDown [] = True
  isUpDown ((x, x') : xs) = x < x' && isDownUp xs

  isDownUp :: (Ord a) => [(a, a)] -> Bool
  isDownUp [] = True
  isDownUp ((x, x') : xs) = x > x' && isUpDown xs
