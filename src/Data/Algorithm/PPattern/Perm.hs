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
, subRed
, reversal
, skewSum
, directSum
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
, toAnnotations

  -- * Testing
, isAlernating
, isUpDown
, isDownUp
, isIncreasing
, isDecreasing
, isMonotone
, isSeparable
, isStackSortable

  -- * Avoiding a pattern of length 3
, is123Avoiding
, is132Avoiding
, is213Avoiding
, is231Avoiding
, is312Avoiding
, is321Avoiding

  -- * Separating tree
, separatingTree

  -- * Statictics
, leftRightMinima
)
where

  import qualified Data.Tuple    as Tuple
  import qualified Data.List     as List
  import qualified Data.Foldable as Foldable
  import qualified Data.Function as Function
  import qualified Data.Monoid   as Monoid
  import qualified Data.Maybe    as Maybe
  import qualified Control.Applicative as Applicative

  import qualified Data.Algorithm.PPattern.Perm.T              as Perm.T
  import qualified Data.Algorithm.PPattern.Perm.List           as Perm.T.List
  import qualified Data.Algorithm.PPattern.Geometry.Point      as P
  import qualified Data.Algorithm.PPattern.Geometry.Point.List as P.List
  import qualified Data.Algorithm.PPattern.SeparatingTree      as ST
  import qualified Data.Algorithm.PPattern.List                as List.Tools
  import qualified Data.Algorithm.PPattern.StackSort           as StackSort

  {-|
    Permutation type.
  -}
  newtype Perm a = Perm { toList :: [Perm.T.T a] } deriving (Eq, Ord)

  instance (Show a) => Show (Perm a) where
    show = show . toAnnotations

  instance Foldable.Foldable Perm  where
    foldr f z (Perm xs) = List.foldr f' z xs
      where
        f' (Perm.T.T (_, a)) = f a

  {-|
    Construct a Perm from foldable.
  -}
  mk :: (Foldable t, Ord a) => t a -> Perm a
  mk = Perm . fmap (uncurry Perm.T.mk) . reduce . Foldable.toList

  {-|
  -}
  sub :: Int -> Int -> Perm a -> Perm a
  sub xMin xMax = Perm . Perm.T.List.sub xMin xMax . toList

  {-|
  -}
  subRed :: (Ord a) => Int -> Int -> Perm a -> Perm a
  subRed xMin xMax = mk . fmap Perm.T.annotation . Perm.T.List.sub xMin xMax . toList

  {-|
    Construct all Perm prefixes of a permutation.
  -}
  prefixes :: Perm a -> [Perm a]
  prefixes = fmap Perm . List.Tools.prefixes . toList

  {-|
    Construct all reduced Perm prefixes of a permutation.
  -}
  prefixesRed :: (Ord a) => Perm a -> [Perm a]
  prefixesRed = fmap (mk . fmap Perm.T.annotation) . List.Tools.prefixes . toList

  {-|
    Construct all Perm suffixes of a permutation.
  -}
  suffixes :: Perm a -> [Perm a]
  suffixes = fmap Perm . List.Tools.suffixes . toList

  {-|
    Construct all reduced Perm suffixes of a permutation.
  -}
  suffixesRed :: (Ord a) => Perm a -> [Perm a]
  suffixesRed = fmap (mk . fmap Perm.T.annotation) . List.Tools.suffixes . toList

  {-|
    Construct all Perm factors of a permutation.
  -}
  factors :: Perm a -> [Perm a]
  factors = fmap Perm . List.Tools.factors . toList

  {-|
    Construct all reduced Perm factors of a permutation.
  -}
  factorsRed :: (Ord a) => Perm a -> [Perm a]
  factorsRed = fmap (mk . fmap Perm.T.annotation) . List.Tools.factors . toList

  {-|
    Reverse a permutation.
  -}
  reversal :: Perm a -> Perm a
  reversal = Perm . Perm.T.List.reversal . toList

  {-|
  -}
  complement :: Perm a -> Perm a
  complement = Perm . Perm.T.List.complement . toList

  {-|
  -}
  reversalComplement :: Perm a -> Perm a
  reversalComplement = Perm . Perm.T.List.reversalComplement . toList

  {-|
  -}
  inverse :: Perm a  -> Perm a
  inverse = Perm . Perm.T.List.inverse . toList

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
  isMonotoneAux cmp = Perm.T.List.isMonotone cmp . toList

  {-|
    Return True iff the permutation is alternating and starts with an up-step.
  -}
  isUpDown :: Perm a -> Bool
  isUpDown = List.Tools.isUpDown . List.Tools.consecutivePairs . yCoords

  {-|
    Return True iff the permutation is alternating and starts with an down-step.
  -}
  isDownUp :: Perm a -> Bool
  isDownUp = List.Tools.isDownUp . List.Tools.consecutivePairs . yCoords

  {-|
    Return True iff the permutation is alternating.
  -}
  isAlernating :: Perm a -> Bool
  isAlernating p = isUpDown p || isDownUp p

  {-|
    'longestIncreasing xs' returns a longest increasing subsequences in 'xs'.
  -}
  longestIncreasing :: Perm a -> Perm a
  longestIncreasing = Perm . Perm.T.List.longestIncreasing . toList

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
  longestDecreasing = Perm . Perm.T.List.longestDecreasing . toList

  {-|
    'longestDecreasingLength xs' returns the length of the longest decreasing
    subsequences in 'xs'.
  -}
  longestDecreasingLength :: Perm a -> Int
  longestDecreasingLength = size . longestDecreasing

  {-|

  -}
  skewSum :: Perm a -> Perm a -> Perm a
  skewSum p q = Perm $ ppas `Monoid.mappend` qpas
    where
      m = size p
      n = size q
      pps  = P.updateYCoord' (+n) Applicative.<$> toPoints p
      ppas = fmap (Tuple.uncurry Perm.T.mk) . List.zip pps $ toAnnotations p
      qps  = P.List.mkFromList . List.zip [(m+1)..] . fmap P.yCoord $ toPoints q
      qpas = fmap (Tuple.uncurry Perm.T.mk) . List.zip qps $ toAnnotations q

  {-|

  -}
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
  isSeparable = Maybe.isJust . separatingTree

  separatingTree :: Perm a -> Maybe ST.SeparatingTree
  separatingTree = ST.mk . toPoints

  {-|
    Stack sort a permutation.
  -}
  stackSort :: Perm a -> Perm a
  stackSort = Perm . Perm.T.List.stackSort . toList

  {-|
    'isStackSortable p' returns True if an only if permutation 'p' is stack
    sortable (i.e. it avoids 231).
  -}
  isStackSortable :: Perm a -> Bool
  isStackSortable = List.Tools.allConsecutivePairs (Tuple.uncurry (<)) . StackSort.stackSort . yCoords

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

  leftRightMinima :: Perm a -> Perm a
  leftRightMinima = Perm . go [] . toList
    where
      go acc []       = List.reverse acc
      go []  (t : ts) = go [t] ts
      go acc@(t' : _) (t : ts)
        | y' > y    = go (t : acc) ts
        | otherwise = go acc ts
        where
          y  = P.yCoord $ Perm.T.point t
          y' = P.yCoord $ Perm.T.point t'

    -- simionSchmidt :: Perm a -> Perm a
    -- simionSchmidt = id
