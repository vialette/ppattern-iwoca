{-|
Module      : Data.Algorithm.PPattern.Geometry.Interval
Structription : Minimum int interval module
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-1017
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer Structription of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.Geometry.Interval
(
  -- * The @Interval@ type
  Interval

  -- * Constructing
, mk
, union
, intersection

 -- * Accessing
, lowerBound
, upperBound

  -- * Comparing
, precedes
, strictlyPrecedes
, follows
, strictlyFollows
, disjoint
, intersects
)
where

  import qualified Data.Tuple as Tuple

  newtype Interval = Interval (Int, Int)
                     deriving (Eq, Show)

  mk :: Int -> Int -> Interval
  mk = Tuple.curry Interval

  lowerBound :: Interval -> Int
  lowerBound (Interval (lb, _)) = lb

  upperBound :: Interval -> Int
  upperBound (Interval (_, ub)) = ub

  intersection :: Interval -> Interval -> Maybe Interval
  intersection i i'
    | disjoint i i' = Nothing
    | otherwise     = Just $ mk lb ub
    where
      lb = max (lowerBound i) (lowerBound i')
      ub = min (upperBound i) (upperBound i')

  union :: Interval -> Interval -> Maybe Interval
  union i i'
    | disjoint i i' = Nothing
    | otherwise     = Just $ mk lb ub
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
  disjoint i i' = precedes i i' || precedes i' i

  intersects :: Interval -> Interval -> Bool
  intersects i i' = not $ disjoint i i'
