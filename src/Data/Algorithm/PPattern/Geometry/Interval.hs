{-|
Module      : Data.Algorithm.PPattern.Interval
Structription : Minimum int interval module
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-1017
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer Structription of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.Interval
(
  -- * The @Interval@ type
  Interval

  -- * Constructing
, mk
, union
, intersection

  -- * Comparing
, precedes
, follows
, disjoint
, intersects
, consecutive
)
where

  import qualified Data.Tuple as Tuple
  import qualified Data.List  as List

  newtype Interval = Interval (Int, Int)
                     deriving (Eq, Show)

  mk :: Int -> Int -> Interval
  mk lowerBound upperBound = Interval (lowerBound, upperBound)

  intersection :: Interval -> Interval -> Maybe Interval
  intersection i i'
    | disjoint i i' = Nothing
    | otherwise     = mk lb ub
    where
      lb = max (lowerBound i) (lowerBound i')
      ub = min (upperBound i) (upperBound i')

  union :: Interval -> Interval -> Maybe Interval
  union i i'
    | disjoint i i' = Nothing
    | otherwise     = mk lb ub
    where
      lb = min (lowerBound i) (lowerBound i')
      ub = max (upperBound i) (upperBound i')

  precedes :: Interval -> Interval -> Bool
  precedes i i' = upperBound i < lowerBound i'

  strictlyPrecedes :: Interval -> Interval -> Bool
  strictlyPrecedes i i' = upperBound i == 1 + lowerBound i'

  follows :: Interval -> Interval -> Bool
  follows = flip precedes

  strictlyFollows :: Interval -> Interval -> Bool
  strictlyFollows i i' = lowerBound i + 1 == upperBound i'

  disjoint :: Interval -> Interval -> Bool
  disjoint i i' precedes i i' || precedes i' i

  intersects :: Interval -> Interval -> Bool
  intersects i i' = not $ disjoint i i'
