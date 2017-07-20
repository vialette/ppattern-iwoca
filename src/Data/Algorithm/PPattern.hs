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
  -- * Searching with default strategy
  search
, occursIn
, avoids
, contains

  -- * Searching with strategy
, searchLeftmostOrderConflictFirst
, searchRightmostOrderConflictFirst
, searchLeftmostValueConflictFirst
, searchRightmostValueConflictFirst
, searchLeftmostConflict
, searchRightmostConflict
)
where

  import qualified Data.Maybe as Maybe

  import qualified Data.Algorithm.PPattern.Perm       as Perm
  import qualified Data.Algorithm.PPattern.Search     as Search
  import qualified Data.Algorithm.PPattern.Strategy   as Strategy
  import qualified Data.Algorithm.PPattern.Occurrence as Occurrence

  {-|
    Search for an order-isomorphic occurrence of 'p' into 'q'.
  -}
  search :: Perm.Perm -> Perm.Perm -> Maybe Occurrence.Occurrence
  search p q = searchWithStrategy p q Strategy.defaultStrategy

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
    to a given strategy.
  -}
  searchWithStrategy :: Perm.Perm -> Perm.Perm -> Strategy.Strategy -> Maybe Occurrence.Occurrence
  searchWithStrategy = Search.search

  {-|
    Search for an order-isomorphic occurrence of 'xs' into 'ys'.
    Resolve conflicts according to a given strategy.
  -}
  searchLeftmostOrderConflictFirst :: Perm.Perm -> Perm.Perm -> Maybe Occurrence.Occurrence
  searchLeftmostOrderConflictFirst p q = searchWithStrategy p q Strategy.leftmostOrderConflictFirst

  {-|
    Search for an order-isomorphic occurrence of 'xs' into 'ys' according
    to the rightmost order conflict first strategy.
  -}
  searchRightmostOrderConflictFirst :: Perm.Perm -> Perm.Perm -> Maybe Occurrence.Occurrence
  searchRightmostOrderConflictFirst p q = searchWithStrategy p q Strategy.rightmostOrderConflictFirst

  {-|
    Search for an order-isomorphic occurrence of 'xs' into 'ys' according
    to the leftmost value conflict first strategy.
  -}
  searchLeftmostValueConflictFirst :: Perm.Perm -> Perm.Perm -> Maybe Occurrence.Occurrence
  p `searchLeftmostValueConflictFirst` q = searchWithStrategy p q Strategy.leftmostValueConflictFirst

  {-|
    Search for an order-isomorphic occurrence of 'xs' into 'ys' according
    to the rightmost value conflict first strategy.
  -}
  searchRightmostValueConflictFirst :: Perm.Perm -> Perm.Perm -> Maybe Occurrence.Occurrence
  searchRightmostValueConflictFirst p q = searchWithStrategy p q Strategy.rightmostValueConflictFirst

  {-|
    Search for an order-isomorphic occurrence of 'xs' into 'ys' according
    to the leftmost conflict strategy.
  -}
  searchLeftmostConflict :: Perm.Perm -> Perm.Perm -> Maybe Occurrence.Occurrence
  searchLeftmostConflict p q = searchWithStrategy p q Strategy.leftmostConflict

  {-|
    Search for an order-isomorphic occurrence of 'xs' into 'ys' according
    to the rightmost conflict strategy.
  -}
  searchRightmostConflict :: Perm.Perm -> Perm.Perm -> Maybe Occurrence.Occurrence
  searchRightmostConflict p q = searchWithStrategy p q Strategy.rightmostConflict
