{-|
Module      : Data.Algorithm.PPattern.APerm
Description : Short description
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-2017
License     : MIT
Maintainer  : vialette@gmaiList.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.APerm
(
  -- * The @APerm@ type
  APerm(..)

  -- * Constructing
, mk

  -- * Transforming
, reversal

  -- * Querying
, size
, longestIncreasing
, longestIncreasingLength
, longestDecreasing
, longestDecreasingLength

  -- * Converting
, toAnnotedList
, points
, annotations

  -- * Testing
, isIncreasing
, isDecreasing
, isMonotone
)
where

  import qualified Data.Tuple      as Tuple
  import qualified Data.List       as List
  import qualified Data.Foldable   as Foldable
  import qualified Data.Function   as Function

  import qualified Data.Algorithm.Patience as Patience

  import qualified Data.Algorithm.PPattern.Geometry.Point as P

  -- Perm element type
  newtype T a = T (P.Point, a) deriving (Eq, Ord, Show)

  mkT :: P.Point -> a -> T a
  mkT p a = T (p, a)

  point :: T a -> P.Point
  point (T (p, _)) = p

  annotation :: T a -> a
  annotation (T (_, a)) = a

  toTuple :: T a -> (P.Point, a)
  toTuple (T (cp, a)) = (cp, a)

  {-|
    Permutation type.
  -}
  newtype APerm a = APerm { toList :: [T a] } deriving (Eq, Ord, Show)

  instance Foldable.Foldable APerm  where
    foldr f z (APerm xs) = List.foldr f' z xs
      where
        f' (T (_, a)) = f a

  {-|
    Construct a Perm from foldable.
  -}
  mk :: (Foldable t, Ord a) => t a -> APerm a
  mk = APerm . fmap (uncurry mkT) . reduce . Foldable.toList

  {-|
    Reverse a permutation.
  -}
  reversal :: APerm a -> APerm a
  reversal (APerm ts) = APerm $ Foldable.foldl f [] ts
    where
      n = List.length ts

      f acc (T (p, a)) = mkT p' a : acc
        where
          x = P.xCoord p
          x' = n + 1 - x

          p' = P.updateXCoord x' p

  {-|
    Turn a permutation into a list with annotations.
  -}
  toAnnotedList :: APerm a -> [(P.Point, a)]
  toAnnotedList (APerm ts) = fmap toTuple ts

  {-|
    Points projection.
  -}
  points :: APerm a -> [P.Point]
  points (APerm ts) = fmap point ts

  {-|
    Points projection.
  -}
  annotations :: APerm a -> [a]
  annotations (APerm ts) = fmap annotation ts

  {-|
    'reduce p' returns the reduced form of the permutation 'p'.
  -}
  reduce :: (Ord a) => [a] -> [(P.Point, a)]
  reduce = fmap f . sortByIdx . List.zip [1..] . sortByElt . List.zip [1..]
    where
      sortByElt = List.sortBy (compare `Function.on` Tuple.snd)
      sortByIdx = List.sortBy (compare `Function.on` (Tuple.fst . Tuple.snd))
      f (y, (x, a)) = (P.mk x y, a)

  {-|
    Return the size of the permutation.
  -}
  size :: APerm a -> Int
  size (APerm ts) = List.length ts

  -- Auxiliary function for isIncreasing and isDecreasing
  isMonotoneAux :: (Int -> Int -> Bool) -> APerm a -> Bool
  isMonotoneAux cmp (APerm ts) = aux ts
    where
      aux  []      = True
      aux (_ : []) = True
      aux _        = Foldable.foldl f True consecutives
        where
          consecutives   =  List.zip ts (List.tail ts)
          f acc (T (p, _), T (p', _)) = acc && (P.yCoord p) `cmp` (P.yCoord p')

  {-|
    Return True iff the permutation is increasing.
  -}
  isIncreasing :: APerm a -> Bool
  isIncreasing = isMonotoneAux (<)

  {-|
    Return True iff the permutation is decreasing.
  -}
  isDecreasing :: APerm a -> Bool
  isDecreasing = isMonotoneAux (>)

  {-|
    Return True iff the permutation is monotone (i.e. increasing or decreasing).
  -}
  isMonotone :: APerm a -> Bool
  isMonotone p = isIncreasing p || isDecreasing p

  {-|
    'longestIncreasing xs' returns a longest increasing subsequences in 'xs'.
  -}
  longestIncreasing :: APerm a -> APerm a
  longestIncreasing (APerm ts) = APerm . unformat . List.reverse . doSearch $ format ts
    where
      format   = fmap (\ t@(T (p, _)) -> (P.yCoord p, t))
      doSearch = Patience.longestIncreasing
      unformat = fmap Tuple.snd

  {-|
    'longestIncreasingLength xs' returns the length of the longest increasing
    subsequences in 'xs'.
  -}
  longestIncreasingLength :: APerm a -> Int
  longestIncreasingLength = size . longestIncreasing

  {-|
    'longestDecreasing xs' returns a longest decreasing subsequences in 'xs'.
  -}
  longestDecreasing :: APerm a -> APerm a
  longestDecreasing (APerm ts) = APerm . unformat . doSearch $ format ts
    where
      format   = List.reverse . fmap (\ t@(T (p, _)) -> (P.yCoord p, t))
      doSearch = Patience.longestIncreasing
      unformat = fmap Tuple.snd

  {-|
    'longestDecreasingLength xs' returns the length of the longest decreasing
    subsequences in 'xs'.
  -}
  longestDecreasingLength :: APerm a -> Int
  longestDecreasingLength = size . longestDecreasing
