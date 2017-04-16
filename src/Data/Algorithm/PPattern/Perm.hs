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

  -- *
, reversal
, prefixes
, prefixesRed
, suffixes
, suffixesRed
, factors
, factorsRed
, complement
, reversalComplement
, inverse
, stackSort

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
, annotations

  -- * Testing
, isAlernating
, isUpDown
, isDownUp
, isIncreasing
, isDecreasing
, isMonotone
, isSeparable


  -- * Avoiding a pattern of length 3
, is123Avoiding
, is132Avoiding
, is213Avoiding
, is231Avoiding
, is312Avoiding
, is321Avoiding

  -- * Separating tree
, separatingTree
)
where

  import qualified Data.Tuple    as Tuple
  import qualified Data.List     as List
  import qualified Data.Foldable as Foldable
  import qualified Data.Function as Function
  import qualified Data.Maybe    as Maybe

  import qualified Data.Algorithm.PPattern.Perm.List           as Perm.T.List
  import qualified Data.Algorithm.PPattern.Geometry.Point      as P
  import qualified Data.Algorithm.PPattern.Geometry.APoint     as APoint
  import qualified Data.Algorithm.PPattern.SeparatingTree      as ST
  import qualified Data.Algorithm.PPattern.List                as List.Tools
  import qualified Data.Algorithm.PPattern.StackSort           as StackSort

  {-|
    Permutation type.
  -}
  newtype Perm a = Perm { getList :: [APoint.APoint a] }
                   deriving (Eq, Ord)

  instance Show (Perm a) where
    show = show . yCoords

  instance Foldable.Foldable Perm  where
    foldr f z (Perm xs) = List.foldr f' z xs
      where
        f' (APoint.APoint (_, a)) = f a

  {-|
    Construct a Perm from foldable.
  -}
  mk :: (Foldable t, Ord a) => t a -> Perm a
  mk = Perm . fmap (uncurry Perm.T.mk) . reduce . Foldable.toList

  {-|
    Construct all Perm prefixes of a permutation.
  -}
  prefixes :: Perm a -> [Perm a]
  prefixes = fmap Perm . List.Tools.prefixes . getList

  {-|
    Construct all reduced Perm prefixes of a permutation.
  -}
  prefixesRed :: (Ord a) => Perm a -> [Perm a]
  prefixesRed = fmap (mk . fmap Perm.T.annotation) . List.Tools.prefixes . getList

  {-|
    Construct all Perm suffixes of a permutation.
  -}
  suffixes :: Perm a -> [Perm a]
  suffixes = fmap Perm . List.Tools.suffixes . getList

  {-|
    Construct all reduced Perm suffixes of a permutation.
  -}
  suffixesRed :: (Ord a) => Perm a -> [Perm a]
  suffixesRed = fmap (mk . fmap Perm.T.annotation) . List.Tools.suffixes . getList

  {-|
    Construct all Perm factors of a permutation.
  -}
  factors :: Perm a -> [Perm a]
  factors = fmap Perm . List.Tools.factors . getList

  {-|
    Construct all reduced Perm factors of a permutation.
  -}
  factorsRed :: (Ord a) => Perm a -> [Perm a]
  factorsRed = fmap (mk . fmap Perm.T.annotation) . List.Tools.factors . getList

  {-|
    Reverse a permutation.
  -}
  reversal :: Perm a -> Perm a
  reversal = Perm . Perm.T.List.reversal . getList

  {-|
  -}
  complement :: Perm a -> Perm a
  complement = Perm . Perm.T.List.complement . getList

  {-|
  -}
  reversalComplement :: Perm a -> Perm a
  reversalComplement = Perm . Perm.T.List.reversalComplement . getList

  {-|
  -}
  inverse :: Perm a  -> Perm a
  inverse = Perm . Perm.T.List.inverse . getList

  {-|
    Turn a permutation into a list with annotations.
  -}
  toAnnotedList :: Perm a -> [(P.Point, a)]
  toAnnotedList = fmap APoint.APointoTuple . getList

  {-|
    Points projection.
  -}
  toPoints :: Perm a -> [P.Point]
  toPoints = fmap Perm.T.point . getList

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
  annotations :: Perm a -> [a]
  annotations = fmap Perm.T.annotation . getList

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
  size = List.length . getList

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
  isMonotoneAux cmp = Perm.T.List.isMonotone cmp . getList

  {-|
    Return True iff the permutation is alternating and starts with an up-step.
  -}
  isUpDown :: Perm a -> Bool
  isUpDown = List.Tools.isUpDown . List.Tools.consecutive2 . yCoords

  {-|
    Return True iff the permutation is alternating and starts with an down-step.
  -}
  isDownUp :: Perm a -> Bool
  isDownUp = List.Tools.isDownUp . List.Tools.consecutive2 . yCoords

  {-|
    Return True iff the permutation is alternating.
  -}
  isAlernating :: Perm a -> Bool
  isAlernating p = isUpDown p || isDownUp p

  {-|
    'longestIncreasing xs' returns a longest increasing subsequences in 'xs'.
  -}
  longestIncreasing :: Perm a -> Perm a
  longestIncreasing = Perm . Perm.T.List.longestIncreasing . getList

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
  longestDecreasing = Perm . Perm.T.List.longestDecreasing . getList

  {-|
    'longestDecreasingLength xs' returns the length of the longest decreasing
    subsequences in 'xs'.
  -}
  longestDecreasingLength :: Perm a -> Int
  longestDecreasingLength = size . longestDecreasing

  {-|
    'isSeparable p' returns True if an only if permutation 'p' is separable
    (i.e., it avoids both 2413 and 3142).
  -}
  isSeparable :: Perm a -> Bool
  isSeparable = Maybe.isJust . separatingTree

  separatingTree :: Perm a -> Maybe ST.SeparatingTree
  separatingTree = ST.mk . toPoints


  {-|
    'is123Avoiding p' returns True if an only if permutation 'p' avoids 123.
  -}
  is123Avoiding :: Perm a -> Bool
  is123Avoiding _ = True

  {-|
    'is132Avoiding p' returns True if an only if permutation 'p' avoids 132.
  -}
  is132Avoiding :: Perm a -> Bool
  is132Avoiding = is231Avoiding . reversal

  {-|
    'is213Avoiding p' returns True if an only if permutation 'p' avoids 213.
  -}
  is213Avoiding :: Perm a -> Bool
  is213Avoiding = is132Avoiding . reversal . complement

  {-|
    'is231Avoiding p' returns True if an only if permutation 'p' avoids 231.
  -}
  is231Avoiding :: Perm a -> Bool
  is231Avoiding = isStackSortable

  {-|
    'is312Avoiding p' returns True if an only if permutation 'p' avoids 312.
  -}
  is312Avoiding :: Perm a -> Bool
  is312Avoiding = is132Avoiding . complement

  {-|
    'is321Avoiding p' returns True if an only if permutation 'p' avoids 321.
  -}
  is321Avoiding :: Perm a -> Bool
  is321Avoiding _ = True


    -- simionSchmidt :: Perm a -> Perm a
    -- simionSchmidt = id
