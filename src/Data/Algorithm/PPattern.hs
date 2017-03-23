
{-|
Module      : Data.Algorithm.PPattern
Structription : Short Structription
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-2017
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Pattern matching for permutations.
-}

module Data.Algorithm.PPattern
(
  -- * Searching with default strategy
  search
, occurrenceIn

, search'

  -- * Searching with strategy
, searchLeftmostOrderConflictFirst
-- , searchLeftmostOrderConflictFirst'
-- , searchRightmostOrderConflictFirst
-- , searchRightmostOrderConflictFirst'
, searchLeftmostValueConflictFirst
-- , searchLeftmostValueConflictFirst'
-- , searchRightmostValueConflictFirst
-- , searchRightmostValueConflictFirst'
, searchLeftmostConflictFirst
-- , searchLeftmostConflictFirst'
-- , searchRightmostConflictFirst
-- , searchRightmostConflictFirst'
)
where

  import qualified Data.Maybe as Maybe

  import qualified Data.Algorithm.PPattern.Perm     as Perm
  import qualified Data.Algorithm.PPattern.Search   as Search
  import qualified Data.Algorithm.PPattern.Strategy as Strategy

  {-|
    Search for an order-isomorphic occurrence of 'xs' into 'ys'.
  -}
  search xs ys = searchWithStrategy Strategy.defaultStrategy

  {-|
    Alias for 'search xs ys'.
  -}
  xs `occurrenceIn` ys = search xs ys

  {-|
    Test if there exists an order-isomorphic occurrence of 'xs' into 'ys'.
  -}
  search' xs ys = isJust $ search xs ys

  {-|
    Alias for 'search' xs ys'.
  -}
  xs `occursIn` ys = search'

  {-|
    Return True if there does not exist an order-isomorphic occurrence of 'xs' into 'ys'.
  -}
  ys `avoids` xs = not $ xs `occursIn` ys

  {-|
    Search for an order-isomorphic occurrence of 'xs' into 'ys' according
    to a given strategy.
  -}
  searchWithStrategy strategy xs ys = Search.search p q strategy
    where
      p = Perm.mkPerm xs
      q = Perm.mkPerm ys

  {-|
    Test if there exists an order-isomorphic occurrence of 'xs' into 'ys'
    accoding to some strategy.
  -}
  searchWithStrategy' strategy xs ys = isJust $ Search.search xs ys strategy

  {-|
    Search for an order-isomorphic occurrence of 'xs' into 'ys'.
    Resolve conflicts according to a given strategy.
  -}
  searchLeftmostOrderConflictFirst = searchWithStrategy Strategy.leftmostOrderConflictFirst

  {-|
    Test if there exists an order-isomorphic occurrence of 'xs' into 'ys'.
    Resolve conflicts following the leftmost order conflict first strategy.
  -}
  searchLeftmostOrderConflictFirst' = searchWithStrategy' Strategy.leftmostOrderConflictFirst

  {-|
    Search for an order-isomorphic occurrence of 'xs' into 'ys' according
    to the rightmost order conflict first strategy.
  -}
  searchRightmostOrderConflictFirst = searchWithStrategy Strategy.rightmostOrderConflictFirst

  {-|
    Test if there exists an order-isomorphic occurrence of 'xs' into 'ys'.
    Resolve conflicts following the rightmost order conflict first strategy.
  -}
  searchRightmostOrderConflictFirst' = searchWithStrategy' Strategy.rightmostOrderConflictFirst

  {-|
    Search for an order-isomorphic occurrence of 'xs' into 'ys' according
    to the leftmost value conflict first strategy.
  -}
  searchLeftmostValueConflictFirst = searchWithStrategy Strategy.leftmostValueConflictFirst

  {-|
    Test if there exists an order-isomorphic occurrence of 'xs' into 'ys'.
    Resolve conflicts following the leftmost value conflict first strategy.
  -}
  searchLeftmostValueConflictFirst' = searchWithStrategy' Strategy.rightmostValueConflictFirst

  {-|
    Search for an order-isomorphic occurrence of 'xs' into 'ys' according
    to the rightmost value conflict first strategy.
  -}
  searchRightmostValueConflictFirst = searchWithStrategy Strategy.rightmostValueConflictFirst

  {-|
    Test if there exists an order-isomorphic occurrence of 'xs' into 'ys'.
    Resolve conflicts following the leftmost value conflict first strategy.
  -}
  searchLeftmostValueConflictFirst' = searchWithStrategy' Strategy.leftmostValueConflictFirst

  {-|
    Search for an order-isomorphic occurrence of 'xs' into 'ys' according
    to the leftmost conflict strategy.
  -}
  searchLeftmostConflictFirst = searchWithStrategy Strategy.leftmostConflictFirst

  {-|
    Test if there exists an order-isomorphic occurrence of 'xs' into 'ys'.
    Resolve conflicts following the leftmost conflict first strategy.
  -}
  searchLeftmostConflictFirst' = searchWithStrategy' Strategy.leftmostConflictFirst

  {-|
    Search for an order-isomorphic occurrence of 'xs' into 'ys' according
    to the rightmost conflict strategy.
  -}
  searchRightmostConflictFirst = searchWithStrategy Strategy.rightmostConflictFirst

  {-|
    Test if there exists an order-isomorphic occurrence of 'xs' into 'ys'.
    Resolve conflicts following the rightmost conflict first strategy.
  -}
  searchRightmostConflictFirst' = searchWithStrategy' Strategy.rightmostConflictFirst
