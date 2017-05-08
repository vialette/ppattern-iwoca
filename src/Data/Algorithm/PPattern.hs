
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
, occurrenceIn
, search'
, occursIn
, avoids

  -- * Searching with strategy
, searchLeftmostOrderConflictFirst
, searchLeftmostOrderConflictFirst'
, searchRightmostOrderConflictFirst
, searchRightmostOrderConflictFirst'
, searchLeftmostValueConflictFirst
, searchLeftmostValueConflictFirst'
, searchRightmostValueConflictFirst
, searchRightmostValueConflictFirst'
, searchLeftmostConflict
, searchLeftmostConflict'
, searchRightmostConflict
, searchRightmostConflict'
)
where

  import qualified Data.Maybe as Maybe

  import qualified Data.Algorithm.PPattern.APerm      as APerm
  import qualified Data.Algorithm.PPattern.Search     as Search
  import qualified Data.Algorithm.PPattern.Strategy   as Strategy
  import qualified Data.Algorithm.PPattern.Occurrence as Occurrence

  {-|
    Search for an order-isomorphic occurrence of 'xs' into 'ys'.
  -}
  search :: (Foldable t, Ord a, Foldable f, Ord b) => t a -> f b -> Maybe (Occurrence.Occurrence a b)
  search = searchWithStrategy Strategy.defaultStrategy

  {-|
    Alias for 'search xs ys'.
  -}
  occurrenceIn :: (Foldable t, Ord a, Foldable f, Ord b) => t a -> f b -> Maybe (Occurrence.Occurrence a b)
  xs `occurrenceIn` ys = search xs ys

  {-|
    Test if there exists an order-isomorphic occurrence of 'xs' into 'ys'.
  -}
  search' :: (Foldable t, Ord a, Foldable f, Ord b) => t a -> f b -> Bool
  search' xs ys = Maybe.isJust $ search xs ys

  {-|
    Alias for 'search' xs ys'.
  -}
  occursIn :: (Foldable t, Ord a, Foldable f, Ord b) => t a -> f b -> Bool
  xs `occursIn` ys = search' xs ys

  {-|
    Return True if there does not exist an order-isomorphic occurrence of 'xs' into 'ys'.
  -}
  avoids :: (Foldable t, Ord a, Foldable f, Ord b) => t a -> f b -> Bool
  ys `avoids` xs = not $ xs `occursIn` ys

  {-|
    Search for an order-isomorphic occurrence of 'xs' into 'ys' according
    to a given strategy.
  -}
  searchWithStrategy :: (Foldable t, Ord a, Foldable f, Ord b) => Strategy.Strategy -> t a -> f b -> Maybe (Occurrence.Occurrence a b)
  searchWithStrategy strategy xs ys = Search.search p q strategy
    where
      p = APerm.mk xs
      q = APerm.mk ys

  {-|
    Test if there exists an order-isomorphic occurrence of 'xs' into 'ys'
    accoding to some strategy.
  -}
  searchWithStrategy' :: (Foldable t, Ord a, Foldable f, Ord b) => Strategy.Strategy -> t a -> f b -> Bool
  searchWithStrategy' strategy xs ys = Maybe.isJust $ searchWithStrategy strategy xs ys

  {-|
    Search for an order-isomorphic occurrence of 'xs' into 'ys'.
    Resolve conflicts according to a given strategy.
  -}
  searchLeftmostOrderConflictFirst :: (Foldable t, Ord a, Foldable f, Ord b) => t a -> f b -> Maybe (Occurrence.Occurrence a b)
  searchLeftmostOrderConflictFirst = searchWithStrategy Strategy.leftmostOrderConflictFirst

  {-|
    Test if there exists an order-isomorphic occurrence of 'xs' into 'ys'.
    Resolve conflicts following the leftmost order conflict first strategy.
  -}
  searchLeftmostOrderConflictFirst' :: (Foldable t, Ord a, Foldable f, Ord b) => t a -> f b -> Bool
  searchLeftmostOrderConflictFirst' = searchWithStrategy' Strategy.leftmostOrderConflictFirst

  {-|
    Search for an order-isomorphic occurrence of 'xs' into 'ys' according
    to the rightmost order conflict first strategy.
  -}
  searchRightmostOrderConflictFirst :: (Foldable t, Ord a, Foldable f, Ord b) => t a -> f b -> Maybe (Occurrence.Occurrence a b)
  searchRightmostOrderConflictFirst = searchWithStrategy Strategy.rightmostOrderConflictFirst

  {-|
    Test if there exists an order-isomorphic occurrence of 'xs' into 'ys'.
    Resolve conflicts following the rightmost order conflict first strategy.
  -}
  searchRightmostOrderConflictFirst' :: (Foldable t, Ord a, Foldable f, Ord b) => t a -> f b -> Bool
  searchRightmostOrderConflictFirst' = searchWithStrategy' Strategy.rightmostOrderConflictFirst

  {-|
    Search for an order-isomorphic occurrence of 'xs' into 'ys' according
    to the leftmost value conflict first strategy.
  -}
  searchLeftmostValueConflictFirst :: (Foldable t, Ord a, Foldable f, Ord b) => t a -> f b -> Maybe (Occurrence.Occurrence a b)
  searchLeftmostValueConflictFirst = searchWithStrategy Strategy.leftmostValueConflictFirst

  {-|
    Test if there exists an order-isomorphic occurrence of 'xs' into 'ys'.
    Resolve conflicts following the leftmost value conflict first strategy.
  -}
  searchLeftmostValueConflictFirst' :: (Foldable t, Ord a, Foldable f, Ord b) => t a -> f b -> Bool
  searchLeftmostValueConflictFirst' = searchWithStrategy' Strategy.rightmostValueConflictFirst

  {-|
    Search for an order-isomorphic occurrence of 'xs' into 'ys' according
    to the rightmost value conflict first strategy.
  -}
  searchRightmostValueConflictFirst :: (Foldable t, Ord a, Foldable f, Ord b) => t a -> f b -> Maybe (Occurrence.Occurrence a b)
  searchRightmostValueConflictFirst = searchWithStrategy Strategy.rightmostValueConflictFirst

  {-|
    Test if there exists an order-isomorphic occurrence of 'xs' into 'ys'.
    Resolve conflicts following the rightmost value conflict first strategy.
  -}
  searchRightmostValueConflictFirst' :: (Foldable t, Ord a, Foldable f, Ord b) => t a -> f b -> Bool
  searchRightmostValueConflictFirst' = searchWithStrategy' Strategy.leftmostValueConflictFirst

  {-|
    Search for an order-isomorphic occurrence of 'xs' into 'ys' according
    to the leftmost conflict strategy.
  -}
  searchLeftmostConflict :: (Foldable t, Ord a, Foldable f, Ord b) => t a -> f b -> Maybe (Occurrence.Occurrence a b)
  searchLeftmostConflict = searchWithStrategy Strategy.leftmostConflict

  {-|
    Test if there exists an order-isomorphic occurrence of 'xs' into 'ys'.
    Resolve conflicts following the leftmost conflict first strategy.
  -}
  searchLeftmostConflict' :: (Foldable t, Ord a, Foldable f, Ord b) => t a -> f b -> Bool
  searchLeftmostConflict' = searchWithStrategy' Strategy.leftmostConflict

  {-|
    Search for an order-isomorphic occurrence of 'xs' into 'ys' according
    to the rightmost conflict strategy.
  -}
  searchRightmostConflict :: (Foldable t, Ord a, Foldable f, Ord b) => t a -> f b -> Maybe (Occurrence.Occurrence a b)
  searchRightmostConflict = searchWithStrategy Strategy.rightmostConflict

  {-|
    Test if there exists an order-isomorphic occurrence of 'xs' into 'ys'.
    Resolve conflicts following the rightmost conflict first strategy.
  -}
  searchRightmostConflict' :: (Foldable t, Ord a, Foldable f, Ord b) => t a -> f b -> Bool
  searchRightmostConflict' = searchWithStrategy' Strategy.rightmostConflict
