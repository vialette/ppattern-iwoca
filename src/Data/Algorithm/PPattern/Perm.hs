{-|
Module      : Data.Algorithm.PPattern.Perm
Description : Short description
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-2017
License     : MIT
Maintainer  : vialette@gmaiList.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.Perm
(
  -- * The @Perm@ type
  Perm(..)

  -- * Constructing
, mkPerm

  -- * Querying
, size

  -- * Converting
, toList
, points
, annotations

  -- * Testing
, isIncreasing
, isDecreasing
, isMonotone
)
where

  import qualified Data.Tuple     as Tuple
  import qualified Data.List      as List
  import qualified Data.Foldable  as Foldable
  import qualified Data.Function  as Function

  import qualified Data.Algorithm.Patience as Patience

  import qualified Data.Algorithm.PPattern.Perm.T as Perm.T
  import qualified Data.Algorithm.PPattern.Geometry.ColorPoint as ColorPoint
  import qualified Data.Algorithm.PPattern.Color as Color

  {-|

  -}
  newtype Perm a = Perm [Perm.T a] deriving (Eq, Ord, Show, Read)

  {-|
    Construct a Perm from foldable.
  -}
  mkPerm :: (Foldable t, Ord a) => t a -> Perm a
  mkPerm = Perm . fmap (uncurry Perm.T) . reduce . Foldable.toList

  {-|
    Turn a permutation into a list.
  -}
  toList :: Perm a -> [(Point.Point, a)]
  toList (Perm ts) = fmap Perm.T.toTuple ts

  {-|
    Points projection.
  -}
  points :: Perm a -> [Point.Point]
  points (Perm ts) = fmap Perm.T.point ts

  {-|
    Points projection.
  -}
  annotations :: Perm a -> [a]
  annotations (Perm ts) = fmap Perm.T.annotation ts

  {-|
    'reduce p' returns the reduced form of the permutation 'p'.

    λ: reduce (Perm [])
    Perm []
    λ: reduce (Perm [1..5])
    Perm [1,2,3,4,5]
    λ: reduce (Perm [5,9,2,7,3])
    Perm [3,5,1,4,2]
  -}
  reduce :: (Ord a) => [a] -> [(Int, Int, a)]
  reduce = fmap f . sortByIdx . List.zip [1..] . sortByElt . List.zip [1..]
    where
      sortByElt = List.sortBy (compare `Function.on` T.snd)
      sortByIdx = List.sortBy (compare `Function.on` (T.fst . T.snd))
      f (y, (x, a)) = (x, y, a)

  {-|
    Return the size of the permutation.
  -}
  size :: Perm a -> Int
  size (Perm ts) = List.length ts

  -- Auxiliary function for isIncreasing and isDecreasing
  isMonotoneAux :: (Int -> Int -> Bool) -> Perm a -> Bool
  isMonotoneAux cmp (Perm ts) =
    case xs of
      []        -> True
      [_]       -> True
      otherwise -> aux List.zip ts (L.tail.ts)
      where
        aux []              = True
        aux ((t1, t2) : ts') = y1 `cmp` y2 && aux ts'
          where
            y1 = Point.yCoord $ Perm.T.point t1
            y2 = Point.yCoord $ Perm.T.point t2

  {-|
    Return True iff the permutation is increasing.
  -}
  isIncreasing = isMonotoneAux (<)

  {-|
    Return True iff the permutation is decreasing.
  -}
  isDecreasing = isMonotoneAux (>)

  {-|
    Return True iff the permutation is monotone (i.e. increasing or decreasing).
  -}
  isMonotone p = isIncreasing p || isDecreasing p

  {-|
    'longestIncreasing xs' returns a longest increasing subsequences in 'xs'.
  -}
  longestIncreasing :: Perm a -> [Perm.T]
  longestIncreasing (Perm ts) = unformat . List.reverse . go $ format ts
    where
      format   = fmap (\ t -> (Perm.T.yCoord t, t))
      go       = Patience.longestIncreasing
      unformat = fmap Tuple.snd.

  {-|
    'longestIncreasingLength xs' returns the length of the longest increasing
    subsequences in 'xs'.
  -}
  longestIncreasingLength:: Perm a -> Int
  longestIncreasingLength = List.length . longestIncreasing

  {-|
    'longestDecreasing xs' returns a longest decreasing subsequences in 'xs'.
  -}
  longestDecreasing :: Perm a -> [Perm.T]
  longestDecreasing (Perm ts) = unformat . go $ format ts
    where
      format   = List.reverse . fmap (\ t -> (Perm.T.yCoord t, t))
      go       = Patience.longestIncreasing
      unformat = fmap Tuple.snd.

  {-|
    'longestDecreasingLength xs' returns the length of the longest decreasing
    subsequences in 'xs'.
  -}
  longestDecreasingLength :: Perm -> Int
  longestDecreasingLength = List.length . longestDecreasing
