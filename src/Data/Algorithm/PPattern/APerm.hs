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
, fromList

, apply

  -- * Querying
, size

  -- * Converting
, annotatedPoints
, points
, xCoords
, yCoords
, annotations

  -- * Testing
, isAlternating
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
    APermutation type.
  -}
  newtype APerm a = APerm { getList :: [APoint.APoint a] }
                    deriving (Eq, Ord)

  instance Show (APerm a) where
    show = show . yCoords

  instance Foldable.Foldable APerm  where
    foldr f z (APerm xs) = List.foldr f' z xs
      where
        f' (APoint.APoint (_, a)) = f a

  {-|
    Construct a APerm from some foldable instance.

    >>> APerm.mk "acedb"
    [1,3,5,4,2]


    >>> APerm.mk "ababa"
    [1,4,2,5,3]

  -}
  mk :: (Foldable t, Ord a) => t a -> APerm a
  mk = APerm . fmap (uncurry APoint.mk) . reduce . Foldable.toList

  {-|
    Construct a APerm from a list of APoints.

    >>> APerm.annotatedPoints $ APerm.mk "acedb"
    [APoint (Point (1,1),'a'),APoint (Point (2,3),'c'),APoint (Point (3,5),'e'),APoint (Point (4,4),'d'),APoint (Point (5,2),'b')]
    >>> APerm.fromList . APerm.annotatedPoints $ APerm.mk "acedb"
    [1,3,5,4,2]
  -}
  fromList :: [APoint.APoint a] -> APerm a
  fromList = APerm

  {-|
    Construct a APerm by applying function on an existing APerm..
  -}
  apply :: ([APoint.APoint a] -> [APoint.APoint a]) -> APerm a -> APerm a
  apply f = fromList . f . getList

  {-|
    Annotated points projection.

    >>> APerm.annotatedPoints $ APerm.mk "acedb"
    [(Point (1,1),'a'),(Point (2,3),'c'),(Point (3,5),'e'),(Point (4,4),'d'),(Point (5,2),'b')]
  -}
  annotatedPoints :: APerm a -> [APoint.APoint a]
  annotatedPoints = getList

  {-|
    Points projection.

    >>> APerm.points $ APerm.mk "acedb"
    [Point (1,1),Point (2,3),Point (3,5),Point (4,4),Point (5,2)]
  -}
  points :: APerm a -> [Point.Point]
  points = fmap APoint.point . getList

  {-|
    x-ccordinates projection (i.e. the list of all x-coordinates).

    >>> APerm.xCoords $ APerm.mk "acedb"
    [1,2,3,4,5]
  -}
  xCoords :: APerm a -> [Int]
  xCoords = fmap Point.xCoord . points

  {-|
    y-ccordinates projection (i.e. the list of all y-coordinates).

    >>> APerm.yCoords $ APerm.mk "acedb"
    [1,3,5,4,2]
  -}
  yCoords :: APerm a -> [Int]
  yCoords = fmap Point.yCoord . points

  {-|
    Points projection (i.e. the list of all annotations).

    >>> APerm.annotations $ APerm.mk "acedb"
    "acedb"
  -}
  annotations :: APerm a -> [a]
  annotations = fmap APoint.annotation . getList

  {-|
    'reduce p' returns the reduced form of the APermutation 'p'.
  -}
  reduce :: (Ord a) => [a] -> [(Point.Point, a)]
  reduce = fmap f . sortByIdx . List.zip [1..] . sortByElt . List.zip [1..]
    where
      sortByElt = List.sortBy (compare `Function.on` Tuple.snd)
      sortByIdx = List.sortBy (compare `Function.on` (Tuple.fst . Tuple.snd))
      f (y, (x, a)) = (Point.mk x y, a)

  {-|
    Return the size of a APermutation.

    >>> APerm.size $ APerm.mk ""
    0
    >>> APerm.size $ APerm.mk "acedb"
    5
  -}
  size :: APerm a -> Int
  size = List.length . getList


  {-|
    Return True iff a APermutation is alternating and starts with an up-step.

    >>> APerm.isUpDown $ APerm.mk [1,5,4,6,2,3]
    True
    >>> APerm.isUpDown $ APerm.mk [3,1,6,2,5,4]
    False
  -}
  isUpDown :: APerm a -> Bool
  isUpDown = List.Tools.isUpDown . List.Tools.consecutive2 . yCoords

  {-|
    Return True iff a APermutation is alternating and starts with an down-step.

    >>> APerm.isDownUp $ APerm.mk [1,5,4,6,2,3]
    False
    >>> APerm.isDownUp $ APerm.mk [3,1,6,2,5,4]
    True
  -}
  isDownUp :: APerm a -> Bool
  isDownUp = List.Tools.isDownUp . List.Tools.consecutive2 . yCoords

  {-|
    Return True iff a APerm is alternating (it may start with an up-step
    or a down-step).

    >>> APerm.isAlternating $ APerm.mk [1,5,4,6,2,3]
    True
    >>> APerm.isAlternating $ APerm.mk [1,5,4,6,3,2]
    True
    >>> APerm.isAlternating $ APerm.mk [1..6]
    False
  -}
  isAlternating :: APerm a -> Bool
  isAlternating p = isUpDown p || isDownUp p

  {-|
    Return True if an only if a APerm is separable
    (i.e., it avoids both 2413 and 3142).

    >>> > APerm.isSeparable $ APerm.mk [2,5,6,1,4,3]
    False
    >>> APerm.isSeparable $ APerm.mk [4,5,1,2,6,3]
    False
    >>> APerm.isSeparable $ APerm.mk [4,6,5,3,1,2]
    True
  -}
  isSeparable :: APerm a -> Bool
  isSeparable = Maybe.isJust . separatingTree

  {-|
    Return (if it exists) the separating tree of a APerm.

    >>> > APerm.separatingTree $ APerm.mk [4,3,6,1,2,5]
    Nothing
    >>> APerm.separatingTree $ APerm.mk [4,6,3,1,2,5]
    Nothing
    >>> APerm.separatingTree $ APerm.mk [4,6,5,3,1,2]
    Just - Interval (1,6)
         .- Interval (3,6)
         ..+ Interval (4,6)
         ...Point (1,4)
         ...- Interval (5,6)
         ....Point (2,6)
         ....Point (3,5)
         ..Point (4,3)
         .+ Interval (1,2)
         ..Point (5,1)
         ..Point (6,2)
  -}
  separatingTree :: APerm a -> Maybe ST.SeparatingTree
  separatingTree = ST.mk . points





    -- simionSchmidt :: APerm a -> APerm a
    -- simionSchmidt = id
