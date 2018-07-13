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
, mkSafe
, empty
, fromList

, apply

  -- * Querying
, size
, pointAtXCoord
, pointAtYCoord

  -- * Converting
, points
, xCoords
, yCoords
)
where

  import qualified Data.Tuple      as Tuple
  import qualified Data.List       as List
  import qualified Data.List.Extra as List.Extra
  import qualified Data.Foldable   as Foldable
  import qualified Data.Function   as Function

  import qualified Data.Algorithm.PPattern.Geometry.Point as Point

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
    Ties are resolved according to the left-to-tight otrder.

    >>> Perm.mk "acb"
    [1,3,2]
    >>> Perm.mk "acbacb"
    [1,5,3,2,6,4]
    >>> Perm.mk "acbacbacb"
    [1,7,4,2,8,5,3,9,6]
  -}
  mk :: (Foldable t, Ord a) => t a -> Perm
  mk = mk_ . Foldable.toList

  {-|
    Construct a permutation from some foldable instance.
    Return Nothing in case of ties.

    >>> Perm.mk Safe"acb"
    Just [1,3,2]
    >>> Perm.mk "acbacb"
    Nothing
  -}
  mkSafe :: (Foldable t, Ord a) => t a -> Maybe Perm
  mkSafe fs = aux $ mk_ xs
    where
      xs    = Foldable.toList fs
      aux p = if size p == (List.length . List.Extra.nubOrd) xs
              then Just p
              else Nothing

  -- Construct a permutation from a list.
  mk_ :: (Ord a) => [a] -> Perm
  mk_ = Perm . reduce

  {-|
    Construct a The empty permutation.

    >>> Perm.empty
    []
  -}
  empty :: Perm
  empty = Perm []

  -- Construct a permutation from a list of points.
  -- No check is done, use with caution.
  fromList :: [Point.Point] -> Perm
  fromList = Perm

  {-|
    `pointAtXCoord x p` returns the point of permutation `p` with x-coordinate `x`.
    Linear time function.

    >>> let p = Perm.mk "acedb"
    >>> p
    [1,3,5,4,2]
    >>> mapM_ print [Perm.pointAtXCoord x p | x <- [0..6]]
    Nothing
    Just (Point (1,1))
    Just (Point (2,3))
    Just (Point (3,5))
    Just (Point (4,4))
    Just (Point (5,2))
    Nothing
  -}
  pointAtXCoord :: Int -> Perm -> Maybe Point.Point
  pointAtXCoord x = aux . getList
    where
      aux [] = Nothing
      aux (p : ps)
        | x == Point.xCoord p = Just p
        | otherwise           = aux ps

  {-|
    `pointAtYCoord y p` returns the point of permutation `p` with y-coordinate `y`.
    Linear time function.

    >>> let p = Perm.mk "acedb"
    >>> p
    [1,3,5,4,2]
    >>> mapM_ print [Perm.pointAtYCoord y p | y <- [0..6]]
    Nothing
    Just (Point (1,1))
    Just (Point (5,2))
    Just (Point (2,3))
    Just (Point (4,4))
    Just (Point (3,5))
    Nothing
  -}
  pointAtYCoord :: Int -> Perm -> Maybe Point.Point
  pointAtYCoord y = aux . getList
    where
      aux [] = Nothing
      aux (p : ps)
        | y == Point.yCoord p = Just p
        | otherwise           = aux ps

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
