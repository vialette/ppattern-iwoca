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

  -- *
, pointIn
, intervalIn
)
where

  import qualified Data.Tuple as Tuple

  data Interval = Interval (Int, Int)
                  deriving (Eq, Show)

  {-|
    Construct an interval from two integers.
  -}
  mk :: Int -> Int -> Interval
  mk = Tuple.curry Interval

  {-|
  -}
  lowerBound :: Interval -> Int
  lowerBound (Interval i) = Tuple.fst i

  {-|
  -}
  upperBound :: Interval -> Int
  upperBound (Interval i) = Tuple.snd i

  {-|
    Return the interval intersection of two intervals.
    Return `Nothing`if the two intervals do not intersect.
  -}
  intersection :: Interval -> Interval -> Maybe Interval
  intersection i i'
    | disjoint i i' = Nothing
    | otherwise     = Just $ mk lb ub
    where
      lb = max (lowerBound i) (lowerBound i')
      ub = min (upperBound i) (upperBound i')

  {-|
    Return the union of two intervals.
    Return `Nothing`if the two intervals do not intersect.
  -}
  union :: Interval -> Interval -> Maybe Interval
  union i i'
    | disjoint i i' = Nothing
    | otherwise     = Just $ mk lb ub
    where
      lb = min (lowerBound i) (lowerBound i')
      ub = max (upperBound i) (upperBound i')

  {-|
    Return `True` if the first interval precedes the second interval.
  -}
  precedes :: Interval -> Interval -> Bool
  precedes i i' = upperBound i < lowerBound i'

  {-|
    Return `True` if the first interval strictly precedes the second interval.
  -}
  strictlyPrecedes :: Interval -> Interval -> Bool
  strictlyPrecedes i i' = upperBound i == 1 + lowerBound i'

  {-|
    Return `True` if the first interval strictly follows the second interval.
  -}
  follows :: Interval -> Interval -> Bool
  follows = flip precedes

  {-|
    Return `True` if the first interval strictly follows the second interval.
  -}
  strictlyFollows :: Interval -> Interval -> Bool
  strictlyFollows i i' = lowerBound i + 1 == upperBound i'

  {-|
    Return `True` if the two intervals do not intersect.
  -}
  disjoint :: Interval -> Interval -> Bool
  disjoint i i' = precedes i i' || precedes i' i

  {-|
    Return `True` if the two intervals do intersect.
  -}
  intersects :: Interval -> Interval -> Bool
  intersects i i' = not $ disjoint i i'

  {-|
    Return `True` if an integer is withing an interval.
  -}
  pointIn :: Int -> Interval -> Bool
  pointIn x i = lowerBound i <= x && x <= upperBound i

  {-|
    Return `True` if the first interval is included in the second interval.
  -}
  intervalIn :: Interval -> Interval -> Bool
  intervalIn i i' = lowerBound i' <= lowerBound i && upperBound i <= upperBound i'
