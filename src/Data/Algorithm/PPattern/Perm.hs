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

  -- * Transforming
, reversal
, complement
, reversalComplement
, inverse

  -- * Querying
, size
-- , longestIncreasing
-- , longestIncreasingLength
-- , longestDecreasing
-- , longestDecreasingLength
--
--   -- * Converting
-- , toAnnotedList
-- , points
-- , annotations
--
--   -- * Testing
-- , isIncreasing
-- , isDecreasing
-- , isMonotone
)
where

  import qualified Data.Tuple      as Tuple
  import qualified Data.List       as List
  import qualified Data.Foldable   as Foldable
  import qualified Data.Function   as Function

  -- import qualified Data.Algorithm.Patience as Patience

  import qualified Data.Algorithm.PPattern.Geometry.Point as P

  {-|
    Permutation type.
  -}
  newtype Perm = Perm { getList :: [P.Point] } deriving (Eq, Ord, Show)

  {-|
    Construct a Perm from foldable.
  -}
  mk :: (Foldable t, Ord a) => t a -> Perm
  mk xs = Perm { getList = reduce $ Foldable.toList xs }

  {-|
    'reduce p' returns the reduced form of the permutation 'p'.
  -}
  reduce :: (Ord a) => [a] -> [P.Point]
  reduce = fmap f . sortByIdx . List.zip [1..] . sortByElt . List.zip [1..]
    where
      sortByElt = List.sortBy (compare `Function.on` Tuple.snd)
      sortByIdx = List.sortBy (compare `Function.on` (Tuple.fst . Tuple.snd))
      f (y, (x, _)) = P.mk x y

  size :: Perm -> Int
  size = List.length . getList

  reversal :: Perm -> Perm
  reversal = Perm . P.mkSequential . List.reverse . fmap P.yCoord . getList

  complement :: Perm -> Perm
  complement Perm { getList = xs } = Perm . P.mkSequential . fmap f $ fmap P.yCoord xs
    where
      n   = List.length xs
      f y = n+1-y

  reversalComplement :: Perm -> Perm
  reversalComplement Perm { getList = xs } = Perm . P.mkSequential . fmap f . List.reverse $ fmap P.yCoord xs
    where
      n   = List.length xs
      f y = n+1-y

  inverse :: Perm -> Perm
  inverse Perm { getList = xs } = Perm . P.mkSequential . fmap Tuple.snd . List.sort . flip List.zip [1..] $ fmap P.yCoord xs
