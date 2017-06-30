{-|
Module      : Data.Algorithm.PPattern.List
Description : Short description
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-1017
License     : MIT
Maintainer  : vialette@gmaiList.com
Stability   : experimental

Convenient function to operate on lists.
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

  --
  removeAt :: (Eq a, Num a) => [b] -> a -> (b, [b])
  removeAt []       _ = error "Cannot removeAt an empty list"
  removeAt (x : xs) 0 = (x, xs)
  removeAt (x : xs) n = (x', x:xs')
    where
      (x', xs') = removeAt xs (n-1)

  --
  removeAt' :: (Eq a, Num a) => [b] -> a -> [b]
  removeAt' xs i = Tuple.snd $ removeAt xs i

  splitAtMax :: (Ord a) => [a] -> ([a], a, [a])
  splitAtMax [] = error "Data.PPattern.Algorithm.List.splitAtMax: empty list "
  splitAtMax xs = aux (Foldable.maximum xs) [] xs
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

  {-|
    Return the list of all non-empty prefixes.

    >>> prefixes ""
    []
    >>> prefixes "abcd"
    ["a","ab","abc","abcd"]
  -}
  prefixes :: [a] -> [[a]]
  prefixes = List.tail . List.inits

  {-|
    Return the list of all non-empty suffixes.

    >>> suffixes ""
    []
    >>> suffixes "abcd"
    ["abcd","bcd","cd","d"]
  -}
  suffixes :: [a] -> [[a]]
  suffixes = fmap List.reverse . List.init . fmap List.reverse . List.tails

  {-|
    Return the list of all non-empty factors.

    >>> factors ""
    []
    >>> factors "abcd"
    ["a","ab","abc","abcd","b","bc","bcd","c","cd","d"]
  -}
  factors :: [a] -> [[a]]
  factors = Foldable.concatMap prefixes . suffixes

  {-|
    Return the list of all pairs of consective elements.

    >>> consecutive2 []
    []
    >>> consecutive2 [0]
    []
    >>> [1..10]
    [(1,2),(2,3),(3,4),(4,5),(5,6),(6,7),(7,8),(8,9),(9,10)]
  -}
  consecutive2 :: Foldable t => t a -> [(a, a)]
  consecutive2 = aux . Foldable.toList
    where
      aux []  = []
      aux xs = List.zip xs (List.tail xs)

  {-|
    Return True iff a given predicate is True for all pairs of consective elements.

    >>> allConsecutive2 (\ (x, y) -> y - x <= 1) [1..10]
    True
    >>> allConsecutive2 (\ (x, y) -> y - x <= 1) [1,3..10]
    False
  -}
  allConsecutive2 :: Foldable t => ((a, a) -> Bool) -> t a -> Bool
  allConsecutive2 f = Foldable.all f . consecutive2

  {-|
    Return True iff a given predicate is True for at least one pair of consective
    elements.

    >>> anyConsecutive2 (\ (x, y) -> 3*x == y) [1,3..10]
    True
  -}
  anyConsecutive2 :: Foldable t => ((a, a) -> Bool) -> t a -> Bool
  anyConsecutive2 f = Foldable.any f . consecutive2

  {-|
    Return the list of all triples of consective elements.

    >>> consecutive3 []
    []
    >>> consecutive3 [1]
    []
    >>> consecutive3 [1,2]
    []
    >>> consecutive3 [1..10]
    [(1,2,3),(2,3,4),(3,4,5),(4,5,6),(5,6,7),(6,7,8),(7,8,9),(8,9,10)]
  -}
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

  {-|
    Return True iff a given predicate is True for all triples of
    consective elements.
  -}
  allConsecutive3 :: Foldable t => ((a, a, a) -> Bool) -> t a -> Bool
  allConsecutive3 f = Foldable.all f . consecutive3

  {-|
    Return True iff a given predicate is True for at least one triple of
    consective elements.
  -}
  anyConsecutive3 :: Foldable t => ((a, a, a) -> Bool) -> t a -> Bool
  anyConsecutive3 f = Foldable.any f . consecutive3

  isUpDown ::(Ord a) => [(a, a)] -> Bool
  isUpDown [] = True
  isUpDown ((x, x') : xs) = x < x' && isDownUp xs

  isDownUp :: (Ord a) => [(a, a)] -> Bool
  isDownUp [] = True
  isDownUp ((x, x') : xs) = x > x' && isUpDown xs
