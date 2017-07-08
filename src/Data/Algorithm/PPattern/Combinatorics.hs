{-|
Module      : Data.Algorithm.PPattern.Combinatorics
Description : Combinatorial tools
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-1017
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental
-}

module Data.Algorithm.PPattern.Combinatorics
(
  choose
)
where

  import qualified Data.Monoid as Monoid

  {-|
    'xs `choose` k' returns all sublists of size 'k' of 'xs'.

    Choosing from the empty set.

    >>> [] `Combinatorics.choose` (-1)
    []
    >>> [] `Combinatorics.choose` 0
    [[]]
    >>> [] `Combinatorics.choose` 1
    []

    Choosing from a non-empty set.
    
    >>> [1..5] `Combinatorics.choose` (-1)
    []
    >>> [1..5] `Combinatorics.choose` 0
    [[]]
    >>> [1..5] `Combinatorics.choose` 1
    [[1],[2],[3],[4],[5]]
    >>> [1..5] `Combinatorics.choose` 2
    [[1,2],[1,3],[1,4],[1,5],[2,3],[2,4],[2,5],[3,4],[3,5],[4,5]]
    >>> [1..5] `Combinatorics.choose` 3
    [[1,2,3],[1,2,4],[1,2,5],[1,3,4],[1,3,5],[1,4,5],[2,3,4],[2,3,5],[2,4,5],[3,4,5]]
    >>>  [1..5] `Combinatorics.choose` 4
    [[1,2,3,4],[1,2,3,5],[1,2,4,5],[1,3,4,5],[2,3,4,5]]
    >>> [1..5] `Combinatorics.choose` 5
    [[1,2,3,4,5]]
    >>>  [1..5] `Combinatorics.choose` 6
    []
  -}
  choose :: [a] -> Int -> [[a]]
  xs `choose` k
    | k < 0 = []
    | otherwise = xs `chooseAux` k

  chooseAux :: [a] -> Int -> [[a]]
  _        `chooseAux` 0 = [[]]
  []       `chooseAux` _ =  []
  (x : xs) `chooseAux` k =  xss `Monoid.mappend` xss'
    where
      xss  = (x :) `fmap` (xs `chooseAux` (k-1))
      xss' = xs `chooseAux` k
