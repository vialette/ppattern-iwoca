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
, splitAtMax

  -- *
, tuplify2

  -- *
, sublistIndex

, prefixes
, suffixes
, factors

  -- *
, consecutive2
, allConsecutive2
, anyConsecutive2

-- *
, consecutive3
, allConsecutive3
, anyConsecutive3

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

  splitAtMax :: (Ord a) => [a] -> Maybe ([a], a, [a])
  splitAtMax [] = Nothing
  splitAtMax xs = Just $ aux (Foldable.maximum xs) [] xs
    where
      aux _ _  [] = error "We shouldn't be there"
      aux m ys (z : zs)
        | m == z    = (List.reverse ys, z, zs)
        | otherwise = aux m (z : ys) zs

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

  consecutive2 :: Foldable t => t a -> [(a, a)]
  consecutive2 = aux . Foldable.toList
    where
      aux []  = []
      aux xs = List.zip xs (List.tail xs)

  allConsecutive2 :: Foldable t => ((a, a) -> Bool) -> t a -> Bool
  allConsecutive2 f = Foldable.all f . consecutive2

  anyConsecutive2 :: Foldable t => ((a, a) -> Bool) -> t a -> Bool
  anyConsecutive2 f = Foldable.any f . consecutive2

  consecutive3 :: Foldable t => t a -> [(a, a, a)]
  consecutive3 = aux . Foldable.toList
    where
      aux []  = []
      aux [_] = []
      aux [_, _] = []
      aux xs = List.zip3 xs xs' xs''
        where
          xs'  = List.tail xs
          xs'' = List.tail xs'

  allConsecutive3 :: Foldable t => ((a, a, a) -> Bool) -> t a -> Bool
  allConsecutive3 f = Foldable.all f . consecutive3

  anyConsecutive3 :: Foldable t => ((a, a, a) -> Bool) -> t a -> Bool
  anyConsecutive3 f = Foldable.any f . consecutive3

  isUpDown ::(Ord a) => [(a, a)] -> Bool
  isUpDown [] = True
  isUpDown ((x, x') : xs) = x < x' && isDownUp xs

  isDownUp :: (Ord a) => [(a, a)] -> Bool
  isDownUp [] = True
  isDownUp ((x, x') : xs) = x > x' && isUpDown xs
