{-|
Module      : Data.Algorithm.PPattern
Structription : Short Structription
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-2017
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

  -- * Searching with ConflictSelection
, searchLeftmostOrderConflictFirst
, searchRightmostOrderConflictFirst
, searchLeftmostValueConflictFirst
, searchRightmostValueConflictFirst
, searchLeftmostConflict
, searchRightmostConflict
)
where

  import qualified Data.Maybe as Maybe

  import qualified Data.Algorithm.PPattern.Perm              as Perm
  import qualified Data.Algorithm.PPattern.Search            as Search
  import qualified Data.Algorithm.PPattern.Search.ConflictSelection   as ConflictSelection
  import qualified Data.Algorithm.PPattern.Search.Occurrence as Occurrence

  {-|
    Search for an order-isomorphic occurrence of 'p' into 'q'.
  -}
  search :: Perm.Perm -> Perm.Perm -> Maybe Occurrence.Occurrence
  search p q = searchWithConflictSelection p q ConflictSelection.defaultConflictSelection

  {-|
    Alias for function 'search'.
  -}
  occursIn :: Perm.Perm -> Perm.Perm -> Bool
  p `occursIn` q = Maybe.isJust $ search p q

  {-|
    Return True if there does not exist an order-isomorphic occurrence of 'p' into 'q'.
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
  searchWithConflictSelection :: Perm.Perm -> Perm.Perm -> ConflictSelection.ConflictSelection -> Maybe Occurrence.Occurrence
  searchWithConflictSelection = Search.search

  {-|
    Search for an order-isomorphic occurrence of 'xs' into 'ys'.
    Resolve conflicts according to a given ConflictSelection.
  -}
  searchLeftmostOrderConflictFirst :: Perm.Perm -> Perm.Perm -> Maybe Occurrence.Occurrence
  searchLeftmostOrderConflictFirst p q = searchWithConflictSelection p q ConflictSelection.leftmostOrderConflictFirst

  {-|
    Search for an order-isomorphic occurrence of 'xs' into 'ys' according
    to the rightmost order conflict first ConflictSelection.
  -}
  searchRightmostOrderConflictFirst :: Perm.Perm -> Perm.Perm -> Maybe Occurrence.Occurrence
  searchRightmostOrderConflictFirst p q = searchWithConflictSelection p q ConflictSelection.rightmostOrderConflictFirst

  {-|
    Search for an order-isomorphic occurrence of 'xs' into 'ys' according
    to the leftmost value conflict first ConflictSelection.
  -}
  searchLeftmostValueConflictFirst :: Perm.Perm -> Perm.Perm -> Maybe Occurrence.Occurrence
  p `searchLeftmostValueConflictFirst` q = searchWithConflictSelection p q ConflictSelection.leftmostValueConflictFirst

  {-|
    Search for an order-isomorphic occurrence of 'xs' into 'ys' according
    to the rightmost value conflict first ConflictSelection.
  -}
  searchRightmostValueConflictFirst :: Perm.Perm -> Perm.Perm -> Maybe Occurrence.Occurrence
  searchRightmostValueConflictFirst p q = searchWithConflictSelection p q ConflictSelection.rightmostValueConflictFirst

  {-|
    Search for an order-isomorphic occurrence of 'xs' into 'ys' according
    to the leftmost conflict ConflictSelection.
  -}
  searchLeftmostConflict :: Perm.Perm -> Perm.Perm -> Maybe Occurrence.Occurrence
  searchLeftmostConflict p q = searchWithConflictSelection p q ConflictSelection.leftmostConflict

  {-|
    Search for an order-isomorphic occurrence of 'xs' into 'ys' according
    to the rightmost conflict ConflictSelection.
  -}
  searchRightmostConflict :: Perm.Perm -> Perm.Perm -> Maybe Occurrence.Occurrence
  searchRightmostConflict p q = searchWithConflictSelection p q ConflictSelection.rightmostConflict
