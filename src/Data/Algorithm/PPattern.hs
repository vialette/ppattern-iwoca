{-|
Module      : Data.Algorithm.PPattern
Structription : Short Structription
Copyright   : (c) anonymous, 2016-1017, 2016-2017
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Pattern matching for Permutations.
-}

module Data.Algorithm.PPattern
(
  -- * Searching with default ConflictSelection
  search
, occursIn
, avoids
, contains

  -- * Searching with conflict selection strategy
, searchWithConflictSelectionStrategy
, searchLeftmostConflictFirst
, searchLeftmostHorizontalConflictFirst
, searchLeftmostVerticalConflictFirst
, searchRightmostConflictFirst
, searchRightmostHorizontalConflictFirst
, searchRightmostVerticalConflictFirst
)
where

  import qualified Data.Maybe as Maybe

  import qualified Data.Algorithm.PPattern.Perm                     as Perm
  import qualified Data.Algorithm.PPattern.Search                   as Search
  import qualified Data.Algorithm.PPattern.Search.ConflictSelection as ConflictSelection
  import qualified Data.Algorithm.PPattern.Search.Occurrence        as Occurrence

  {-|
    Alias for function 'search'.
  -}
  occursIn :: Perm.Perm -> Perm.Perm -> Bool
  p `occursIn` q = Maybe.isJust $ search p q

  {-|
    Return True if there does not exist an order-isomorphic occurrence of 'p'
    into 'q'.
  -}
  avoids :: Perm.Perm -> Perm.Perm -> Bool
  q `avoids` p = not $ p `occursIn` q

  {-|
    Return True if there exists an order-isomorphic occurrence of 'p' into 'q'.
  -}
  contains :: Perm.Perm -> Perm.Perm -> Bool
  q `contains` p = p `occursIn` q

  {-|
    Search for an order-isomorphic occurrence of 'p' into 'q' according
    to a given ConflictSelection.
  -}
  searchWithConflictSelectionStrategy :: Perm.Perm -> Perm.Perm -> ConflictSelection.Strategy -> Maybe Occurrence.Occurrence
  searchWithConflictSelectionStrategy = Search.search

  {-|
    Search for an order-isomorphic occurrence of 'p' into 'q' using the default
    conflict resolution strategy.
  -}
  search :: Perm.Perm -> Perm.Perm -> Maybe Occurrence.Occurrence
  search p q = searchWithConflictSelectionStrategy p q ConflictSelection.DefaultStrategy

  {-|
    Search for an order-isomorphic occurrence of 'p' into 'q' according
    to the leftmost conflict ConflictSelection.
  -}
  searchLeftmostConflictFirst :: Perm.Perm -> Perm.Perm -> Maybe Occurrence.Occurrence
  searchLeftmostConflictFirst p q = searchWithConflictSelectionStrategy p q ConflictSelection.LeftmostConflictFirst

  {-|
    Search for an order-isomorphic occurrence of 'p' into 'q'.
    Resolve conflicts according to a given strategy.
  -}
  searchLeftmostHorizontalConflictFirst :: Perm.Perm -> Perm.Perm -> Maybe Occurrence.Occurrence
  searchLeftmostHorizontalConflictFirst p q = searchWithConflictSelectionStrategy p q ConflictSelection.LeftmostHorizontalConflictFirst

  {-|
    Search for an order-isomorphic occurrence of 'p' into 'q' according
    to the rightmost order conflict first strategy.
  -}
  searchRightmostHorizontalConflictFirst :: Perm.Perm -> Perm.Perm -> Maybe Occurrence.Occurrence
  searchRightmostHorizontalConflictFirst p q = searchWithConflictSelectionStrategy p q ConflictSelection.LeftmostVerticalConflictFirst

  {-|
    Search for an order-isomorphic occurrence of 'p' into 'q' according
    to the rightmost conflict strategy.
  -}
  searchRightmostConflictFirst :: Perm.Perm -> Perm.Perm -> Maybe Occurrence.Occurrence
  searchRightmostConflictFirst p q = searchWithConflictSelectionStrategy p q ConflictSelection.RightmostConflictFirst

  {-|
    Search for an order-isomorphic occurrence of 'p' into 'q' according
    to the leftmost value conflict first strategy.
  -}
  searchLeftmostVerticalConflictFirst :: Perm.Perm -> Perm.Perm -> Maybe Occurrence.Occurrence
  p `searchLeftmostVerticalConflictFirst` q = searchWithConflictSelectionStrategy p q ConflictSelection.RightmostHorizontalConflictFirst

  {-|
    Search for an order-isomorphic occurrence of 'p' into 'q' according
    to the rightmost value conflict first strategy.
  -}
  searchRightmostVerticalConflictFirst :: Perm.Perm -> Perm.Perm -> Maybe Occurrence.Occurrence
  searchRightmostVerticalConflictFirst p q = searchWithConflictSelectionStrategy p q ConflictSelection.RightmostVerticalConflictFirst
