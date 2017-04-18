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
, fromList

, apply

  -- * Querying
, size


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
, isSeparable




  -- * Separating tree
, separatingTree
)
where

  import qualified Data.Tuple    as Tuple
  import qualified Data.List     as List
  import qualified Data.Foldable as Foldable
  import qualified Data.Function as Function
  import qualified Data.Maybe    as Maybe

  import qualified Data.Algorithm.PPattern.Geometry.Point      as Point
  import qualified Data.Algorithm.PPattern.Geometry.APoint     as APoint
  import qualified Data.Algorithm.PPattern.SeparatingTree      as ST
  import qualified Data.Algorithm.PPattern.List                as List.Tools

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
  mk = Perm . fmap (uncurry APoint.mk) . reduce . Foldable.toList

  fromList :: [APoint.APoint a] -> Perm a
  fromList = Perm

  apply :: ([APoint.APoint a] -> [APoint.APoint a]) -> Perm a -> Perm a
  apply f = fromList . f . getList


  {-|
    Turn a permutation into a list with annotations.
  -}
  toAnnotedList :: Perm a -> [(Point.Point, a)]
  toAnnotedList = fmap APoint.toTuple . getList

  {-|
    Points projection.
  -}
  toPoints :: Perm a -> [Point.Point]
  toPoints = fmap APoint.point . getList

  {-|
    x-ccordinates projection.
  -}
  xCoords :: Perm a -> [Int]
  xCoords = fmap Point.xCoord . toPoints

  {-|
    y-ccordinates projection.
  -}
  yCoords :: Perm a -> [Int]
  yCoords = fmap Point.yCoord . toPoints

  {-|
    Points projection.
  -}
  annotations :: Perm a -> [a]
  annotations = fmap APoint.annotation . getList

  {-|
    'reduce p' returns the reduced form of the permutation 'p'.
  -}
  reduce :: (Ord a) => [a] -> [(Point.Point, a)]
  reduce = fmap f . sortByIdx . List.zip [1..] . sortByElt . List.zip [1..]
    where
      sortByElt = List.sortBy (compare `Function.on` Tuple.snd)
      sortByIdx = List.sortBy (compare `Function.on` (Tuple.fst . Tuple.snd))
      f (y, (x, a)) = (Point.mk x y, a)

  {-|
    Return the size of the permutation.
  -}
  size :: Perm a -> Int
  size = List.length . getList


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
    'isSeparable p' returns True if an only if permutation 'p' is separable
    (i.e., it avoids both 2413 and 3142).
  -}
  isSeparable :: Perm a -> Bool
  isSeparable = Maybe.isJust . separatingTree

  separatingTree :: Perm a -> Maybe ST.SeparatingTree
  separatingTree = ST.mk . toPoints





    -- simionSchmidt :: Perm a -> Perm a
    -- simionSchmidt = id
