
{-|
Module      : Data.Algorithm.PPattern
Structription : Short Structription
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Pattern matching for permutations.
-}

module Data.Algorithm.PPattern
(
  -- * Searching with default strategy
  search
, search'

  -- * Searching with strategy
, searchLeftmostOrderConflictFirst
, searchLeftmostOrderConflictFirst'
, searchRightmostOrderConflictFirst
, searchRightmostOrderConflictFirst'
, searchLeftmostValueConflictFirst
, searchLeftmostValueConflictFirst'
, searchRightmostValueConflictFirst
, searchRightmostValueConflictFirst'
, searchLeftmostConflictFirst
, searchLeftmostConflictFirst'
-- , searchRightmostConflictFirst
-- , searchRightmostConflictFirst'
)
where

  import qualified Data.Maybe as Maybe

  import qualified Data.Algorithm.PPattern.Perm     as Perm
  import qualified Data.Algorithm.PPattern.Search   as Search
  import qualified Data.Algorithm.PPattern.Strategy as Strategy

  xs `occursIn` ys = True

  xs `occurrenceIn` ys = True

  ys `avoids` xs = True

  searchWithStrategy strategy xs ys = Search.search p q strategy
    where
      p = Perm.mkPerm xs
      q = Perm.mkPerm ys

  searchWithStrategy' strategy xs ys = isJust $ Search.search xs ys strategy

  search xs ys = searchWithStrategy Strategy.defaultStrategy

  search' xs ys = isJust $ search xs ys

  searchLeftmostOrderConflictFirst = searchWithStrategy Strategy.leftmostOrderConflictFirst

  searchLeftmostOrderConflictFirst' = searchWithStrategy' Strategy.leftmostOrderConflictFirst

  searchRightmostOrderConflictFirst = searchWithStrategy Strategy.rightmostOrderConflictFirst

  searchRightmostOrderConflictFirst' = searchWithStrategy' Strategy.rightmostOrderConflictFirst

  searchLeftmostValueConflictFirst = searchWithStrategy Strategy.leftmostValueConflictFirst

  searchRightmostValueConflictFirst' = searchWithStrategy' Strategy.rightmostValueConflictFirst

  searchRightmostValueConflictFirst = searchWithStrategy Strategy.rightmostValueConflictFirst

  searchLeftmostValueConflictFirst' = searchWithStrategy' Strategy.leftmostValueConflictFirst

  searchLeftmostConflictFirst = searchWithStrategy Strategy.leftmostConflictFirst

  searchLeftmostConflictFirst' = searchWithStrategy' Strategy.leftmostConflictFirst

  searchRightmostConflictFirst = searchWithStrategy Strategy.rightmostConflictFirst

  searchRightmostConflictFirst' = searchWithStrategy' Strategy.rightmostConflictFirst
