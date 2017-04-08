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
, mk
, sub
, reversal
, skewSum
, directSum
, prefixes
, prefixesRed
, suffixes
, suffixesRed
, factors
, factorsRed

  -- * Querying
, size
, longestIncreasing
, longestIncreasingLength
, longestDecreasing
, longestDecreasingLength

  -- * Converting
, toAnnotedList
, toPoints
, xCoords
, yCoords
, toAnnotations

  -- * Testing
, isIncreasing
, isDecreasing
, isMonotone
, isSeparable
, isStackSortable
)
where

  import qualified Data.Tuple    as Tuple
  import qualified Data.List     as List
  import qualified Data.Foldable as Foldable
  import qualified Data.Function as Function
  import qualified Data.Monoid   as Monoid
  import qualified Data.Maybe    as Maybe
  import qualified Control.Applicative as Applicative

  import qualified Data.Algorithm.PPattern.Perm.T         as Perm.T
  import qualified Data.Algorithm.PPattern.Perm.List      as Perm.List
  import qualified Data.Algorithm.PPattern.Geometry.Point as P
  import qualified Data.Algorithm.PPattern.SeparatingTree as ST
  import qualified Data.Algorithm.PPattern.StackSort      as StackSort
  import qualified Data.Algorithm.PPattern.Tools          as Tools

  {-|
    Permutation type.
  -}
  newtype Perm a = Perm { toList :: [Perm.T.T a] } deriving (Eq, Ord, Show)

  instance Foldable.Foldable Perm  where
    foldr f z (Perm xs) = List.foldr f' z xs
      where
        f' (Perm.T.T (_, a)) = f a

  {-|
    Construct a Perm from foldable.
  -}
  mk :: (Foldable t, Ord a) => t a -> Perm a
  mk = Perm . fmap (uncurry Perm.T.mk) . reduce . Foldable.toList


  sub :: Int -> Int -> Perm a -> Perm a
  sub xMin xMax = Perm . Perm.List.sub xMin xMax . toList

  subRed :: Int -> Int -> Perm a -> Perm a
  subRed xMin xMax = mk . fmap Perm.T.annotation . Perm.List.sub xMin xMax . toList

  {-|
    Construct all Perm prefixes of a permutation.
  -}
  prefixes :: Perm a -> [Perm a]
  prefixes = fmap Perm . Tools.prefixes . toList

  {-|
    Construct all reduced Perm prefixes of a permutation.
  -}
  prefixesRed :: (Ord a) => Perm a -> [Perm a]
  prefixesRed = fmap (mk . fmap Perm.T.annotation) . Tools.prefixes . toList


  {-|
    Construct all Perm suffixes of a permutation.
  -}
  suffixes :: Perm a -> [Perm a]
  suffixes = fmap Perm . Tools.suffixes . toList

  {-|
    Construct all reduced Perm suffixes of a permutation.
  -}
  suffixesRed :: (Ord a) => Perm a -> [Perm a]
  suffixesRed = fmap (mk . fmap Perm.T.annotation) . Tools.suffixes . toList

  {-|
    Construct all Perm factors of a permutation.
  -}
  factors :: Perm a -> [Perm a]
  factors = fmap Perm . Tools.factors . toList

  {-|
    Construct all reduced Perm factors of a permutation.
  -}
  factorsRed :: (Ord a) => Perm a -> [Perm a]
  factorsRed = fmap (mk . fmap Perm.T.annotation) . Tools.factors . toList

  {-|
    Reverse a permutation.
  -}
  reversal :: Perm a -> Perm a
  reversal = Perm . Perm.List.reversal . toList

  complement :: Perm a -> Perm a
  complement = Perm . Perm.List.complement . toList

  reversalComplement :: Perm a -> Perm a
  reversalComplement = Perm . Perm.List.reversalComplement . toList

  inverse :: Perm a  -> Perm a
  inverse = Perm . Perm.List.inverse . toList


  {-|
    Turn a permutation into a list with annotations.
  -}
  toAnnotedList :: Perm a -> [(P.Point, a)]
  toAnnotedList = fmap Perm.T.toTuple . toList

  {-|
    Points projection.
  -}
  toPoints :: Perm a -> [P.Point]
  toPoints = fmap Perm.T.point . toList

  {-|
    x-ccordinates projection.
  -}
  xCoords :: Perm a -> [Int]
  xCoords = fmap P.xCoord . toPoints

  {-|
    y-ccordinates projection.
  -}
  yCoords :: Perm a -> [Int]
  yCoords = fmap P.yCoord . toPoints

  {-|
    Points projection.
  -}
  toAnnotations :: Perm a -> [a]
  toAnnotations = fmap Perm.T.annotation . toList

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
  size :: Perm a -> Int
  size = List.length . toList

  {-|
    Return True iff the permutation is increasing.
  -}
  isIncreasing :: Perm a -> Bool
  isIncreasing = isMonotoneAux (<)

  {-|
    Return True iff the permutation is decreasing.
  -}
  isDecreasing :: Perm a -> Bool
  isDecreasing = isMonotoneAux (>)

  {-|
    Return True iff the permutation is monotone (i.e. increasing or decreasing).
  -}
  isMonotone :: Perm a -> Bool
  isMonotone p = isIncreasing p || isDecreasing p

  -- Auxiliary function for isIncreasing and isDecreasing
  isMonotoneAux :: (Int -> Int -> Bool) -> Perm a -> Bool
  isMonotoneAux cmp = Perm.List.isMonotone cmp . toList

  {-|
    Return True iff the permutation is alternating and starts with an up-step.
  -}
  isUpDown :: Perm a -> Bool
  isUpDown = isUpDown' . Tools.consecutivePairs . yCoords

  isUpDown' [] = True
  isUpDown' ((x, x') : xs) = x < x' && isDownUp' xs

  {-|
    Return True iff the permutation is alternating and starts with an down-step.
  -}
  isDownUp :: Perm a -> Bool
  isDownUp = isUpDown' . Tools.consecutivePairs . yCoords

  isDownUp' [] = True
  isDownUp' ((x, x') : xs) = x > x' && isUpDown' xs

  {-|
    Return True iff the permutation is alternating.
  -}
  isAlernating :: Perm a -> Bool
  isAlernating p = isUpDown p || isDownUp p

  {-|
    'longestIncreasing xs' returns a longest increasing subsequences in 'xs'.
  -}
  longestIncreasing :: Perm a -> Perm a
  longestIncreasing = Perm . Perm.List.longestIncreasing . toList

  {-|
    'longestIncreasingLength xs' returns the length of the longest increasing
    subsequences in 'xs'.
  -}
  longestIncreasingLength :: Perm a -> Int
  longestIncreasingLength = size . longestIncreasing

  {-|
    'longestDecreasing xs' returns a longest decreasing subsequences in 'xs'.
  -}
  longestDecreasing :: Perm a -> Perm a
  longestDecreasing Perm = Perm.List.longestDecreasing . toList

  {-|
    'longestDecreasingLength xs' returns the length of the longest decreasing
    subsequences in 'xs'.
  -}
  longestDecreasingLength :: Perm a -> Int
  longestDecreasingLength = size . longestDecreasing

  skewSum :: Perm a -> Perm a -> Perm a
  skewSum p q = Perm $ ppas `Monoid.mappend` qpas
    where
      m = size p
      n = size q
      pps  = P.updateYCoord' (+n) Applicative.<$> toPoints p
      ppas = fmap (Tuple.uncurry Perm.T.mk) . List.zip pps $ toAnnotations p
      qps  = P.List.mkFromList . List.zip [(m+1)..] . fmap P.yCoord $ toPoints q
      qpas = fmap (Tuple.uncurry Perm.T.mk) . List.zip qps $ toAnnotations q

  directSum :: Perm a -> Perm a -> Perm a
  directSum Perm { toList = ppas } q = Perm $ ppas `Monoid.mappend` qpas
    where
      m = List.length ppas
      qps  = fmap (\ (x, p) -> P.mk x (m + P.yCoord p)) . List.zip [(m+1)..] $ toPoints q
      qpas = fmap (Tuple.uncurry Perm.T.mk) . List.zip qps $ toAnnotations q

  {-|
    'isSeparable p' returns True if an only if permutation 'p' is separable
    (i.e., it avoids both 2413 and 3142).
  -}
  isSeparable :: Perm a -> Bool
  isSeparable = Maybe.isJust . ST.mk . toPoints

  {-|
    Stack sort a permutation.
  -}
  stackSort :: Perm a -> Perm a
  stackSort = Perm . StackSort.stackSort . yCoords

  {-|
    'is231Avoiding p' returns True if an only if permutation 'p' avoids 231.
  -}
  is231Avoiding :: Perm a -> Bool
  is231Avoiding = isStackSortable

  {-|
    'isStackSortable p' returns True if an only if permutation 'p' is stack
    sortable (i.e. it avoids 231).
  -}
  isStackSortable :: Perm a -> Bool
  isStackSortable = Tools.allConsecutivePairs (<) . StackSort.stackSort . yCoords
