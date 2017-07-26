{-|
Module      : Data.Algorithm.PPattern.Perm
Description : Short description
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-2017
License     : MIT
Maintainer  : vialette@gmaiList.com
Stability   : experimental

Annotated permutation.
-}

module Data.Algorithm.PPattern.Perm
(
  -- * The @Perm@ type
  Perm(..)

  -- * Constructing
, mk
, empty
, fromList

, apply

  -- * Querying
, size

  -- * Converting
, points
, xCoords
, yCoords
)
where

  import qualified Data.Tuple    as Tuple
  import qualified Data.List     as List
  import qualified Data.Foldable as Foldable
  import qualified Data.Function as Function

  import qualified Data.Algorithm.PPattern.Geometry.Point  as Point

  {-|
    Permutation type.
  -}
  newtype Perm = Perm { getList :: [Point.Point] }
                 deriving (Eq, Ord)

  {-|
    Perm Show instance.
  -}
  instance Show Perm where
    show = show . yCoords

  {-|
    Construct a permutation from some foldable instance.

    >>> Perm.mk "acb"
    [1,3,2]
    >>> Perm.mk "acbacb"
    [1,5,3,2,6,4]
    >>> Perm.mk "acbacbacb"
    [1,7,4,2,8,5,3,9,6]
  -}
  mk :: (Foldable t, Ord a) => t a -> Perm
  mk = Perm . reduce . Foldable.toList

  {-|
    Construct a The empty permutation.
  -}
  empty :: Perm
  empty = Perm []

  -- Construct a Perm from a list of points.
  -- No check is done, use with caution.
  fromList :: [Point.Point] -> Perm
  fromList = Perm

  -- Construct a Perm by applying a function (point level) on an existing Perm..
  apply :: ([Point.Point] -> [Point.Point]) -> Perm -> Perm
  apply f = fromList . f . getList

  {-|
    Points projection.

    >>> let p = Perm.mk "acedb"
    >>> p
    [1,3,5,4,2]
    >>> Perm.points p
    [Point (1,1),Point (2,3),Point (3,5),Point (4,4),Point (5,2)]
  -}
  points :: Perm -> [Point.Point]
  points = getList

  {-|
    x-ccordinates projection (i.e. the list of all x-coordinates).

    >>> let p = Perm.mk "acedb"
    >>> p
    [1,3,5,4,2]
    >>> Perm.xCoords p
    [1,2,3,4,5]
  -}
  xCoords :: Perm -> [Int]
  xCoords = fmap Point.xCoord . points

  {-|
    y-ccordinates projection (i.e. the list of all y-coordinates).

    >>> let p = Perm.mk "acedb"
    >>> p
    [1,3,5,4,2]
    >>> Perm.yCoords p
    [1,3,5,4,2]
  -}
  yCoords :: Perm -> [Int]
  yCoords = fmap Point.yCoord . points

  -- 'reduce p' returns the reduced form of the permutation 'p'.
  reduce :: (Ord a) => [a] -> [Point.Point]
  reduce = fmap f . sortByIdx . List.zip [1..] . sortByElt . List.zip [1..]
    where
      sortByElt = List.sortBy (compare `Function.on` Tuple.snd)
      sortByIdx = List.sortBy (compare `Function.on` (Tuple.fst . Tuple.snd))
      f (y, (x, _)) = Point.mk x y

  {-|
    Return the size of a Permutation.

    >>> Perm.size $ Perm.mk ""
    0
    >>> Perm.size $ Perm.mk "acedb"
    5
  -}
  size :: Perm -> Int
  size = List.length . getList
