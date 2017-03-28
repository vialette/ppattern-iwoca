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
, empty

  -- * Transforming
, reversal
, complement
, reversalComplement
, inverse

  -- * composing
, skewSum
, directSum

  -- * Querying
, size
, Data.Algorithm.PPattern.Perm.null
-- , longestIncreasing
-- , longestIncreasingLength
-- , longestDecreasing
-- , longestDecreasingLength
--
  -- * Converting
-- , toAnnotedList
, toPoints
-- , annotations
--
  -- * Testing
, isIncreasing
, isDecreasing
, isMonotone
)
where

  import qualified Data.Tuple    as Tuple
  import qualified Data.List     as List
  import qualified Data.Foldable as Foldable
  import qualified Data.Function as Function
  import qualified Data.Monoid   as Monoid

  import qualified Data.Algorithm.PPattern.Geometry.Point as P

  {-|
    Permutation type.
  -}
  newtype Perm = Perm { toList :: [P.Point] } deriving (Eq, Ord)

  instance Show Perm where
    show = show . fmap P.yCoord . toList

  {-|
    Construct a Perm from foldable.
  -}
  mk :: (Foldable t, Ord a) => t a -> Perm
  mk xs = Perm { toList = reduce $ Foldable.toList xs }

  empty :: Perm
  empty = Perm { toList = [] }

  toPoints :: Perm -> [P.Point]
  toPoints = toList

  null :: Perm -> Bool
  null = List.null . toList

  -- Return the reduced form of the permutation 'p'.
  reduce :: (Ord a) => [a] -> [P.Point]
  reduce = fmap f . sortByIdx . List.zip [1..] . sortByElt . List.zip [1..]
    where
      sortByElt = List.sortBy (compare `Function.on` Tuple.snd)
      sortByIdx = List.sortBy (compare `Function.on` (Tuple.fst . Tuple.snd))
      f (y, (x, _)) = P.mk x y

  size :: Perm -> Int
  size = List.length . toList

  reversal :: Perm -> Perm
  reversal = Perm . P.mkSequential . List.reverse . fmap P.yCoord . toList

  complement :: Perm -> Perm
  complement Perm { toList = xs } = Perm . P.mkSequential . fmap f $ fmap P.yCoord xs
    where
      n   = List.length xs
      f y = n+1-y

  reversalComplement :: Perm -> Perm
  reversalComplement Perm { toList = xs } = Perm . P.mkSequential . fmap f . List.reverse $ fmap P.yCoord xs
    where
      n   = List.length xs
      f y = n+1-y

  inverse :: Perm -> Perm
  inverse Perm { toList = xs } = Perm . P.mkSequential . fmap Tuple.snd . List.sort . flip List.zip [1..] $ fmap P.yCoord xs

  skewSum :: Perm -> Perm -> Perm
  skewSum p q = Perm $ xs `Monoid.mappend` ys
    where
      m = size p
      n = size q
      xs = P.mkSequential . fmap ((+n) . P.yCoord) $ toList p
      ys = P.mkFromList . List.zip [(m+1)..] . fmap P.yCoord $ toList q

  directSum :: Perm -> Perm -> Perm
  directSum p q = Perm $ xs `Monoid.mappend` ys
    where
      m = size p
      xs = toList p
      ys = P.mkFromList . List.zip [(m+1)..] . fmap ((+m) . P.yCoord) $ toList q

  {-|
    Return True iff the permutation is increasing.
  -}
  isIncreasing :: Perm -> Bool
  isIncreasing p = toList p == P.mkSequential [1..n]
    where
      n = size p

  {-|
    Return True iff the permutation is decreasing.
  -}
  isDecreasing :: Perm -> Bool
  isDecreasing p = toList p == P.mkSequential [n,(n-1)..1]
    where
      n = size p

  {-|
    Return True iff the permutation is monotone (i.e. increasing or decreasing).
  -}
  isMonotone :: Perm -> Bool
  isMonotone p = isIncreasing p || isDecreasing p
