{-|
Module      : Data.Algorithm.PPattern.ConflictSelection
Structription : Short Structription
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-1017
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Pattern matching search ConflictSelection.
-}

module Data.Algorithm.PPattern.Search.ConflictSelection
(
  -- * The @ConflictSelection@ type
  ConflictSelection

  -- * default ConflictSelection
, defaultConflictSelection

  -- * strategies
, leftmostConflict
, rightmostConflict
, leftmostHorizontalConflictFirst
, rightmostHorizontalConflictFirst
, leftmostVerticalConflictFirst
, rightmostVerticalConflictFirst
)
where

  import qualified Data.Tuple as Tuple
  import qualified Data.List  as List

  import qualified Data.Algorithm.PPattern.Geometry.ColorPoint   as ColorPoint
  import qualified Data.Algorithm.PPattern.Combinatorics         as Combinatorics
  import qualified Data.Algorithm.PPattern.Search.Conflict       as Conflict
  import qualified Data.Algorithm.PPattern.Search.State          as State
  import qualified Data.Algorithm.PPattern.Search.ConflictSelection.Link  as ConflictSelection.Link
  import qualified Data.Algorithm.PPattern.Search.ConflictSelection.Link2 as ConflictSelection.Link2
  import qualified Data.Algorithm.PPattern.List                  as List.Tools

  -- ConflictSelection
  type ConflictSelection = State.State -> Maybe Conflict.Conflict

  -- All pairs of links (i.e. the state embedding as a list of links).
  collect :: State.State -> [ConflictSelection.Link2.Link2]
  collect = fmap f . flip Combinatorics.choose 2 . State.toList
    where
      f = Tuple.uncurry ConflictSelection.Link2.mk . List.Tools.tuplify2 . fmap (Tuple.uncurry ConflictSelection.Link.mk)

  -- Test if two links induce an order conflict.
  horizontalConflict :: ConflictSelection.Link.Link -> ConflictSelection.Link.Link -> Bool
  horizontalConflict l1 l2 = x1 < x2 && x1' > x2'
    where
      x1  = ColorPoint.xCoord $ ConflictSelection.Link.fstColorPoint l1
      x1' = ColorPoint.xCoord $ ConflictSelection.Link.sndColorPoint l1

      x2  = ColorPoint.xCoord $ ConflictSelection.Link.fstColorPoint l2
      x2' = ColorPoint.xCoord $ ConflictSelection.Link.sndColorPoint l2

  -- Make an order conflict from two links.
  mkHorizontalConflict :: ConflictSelection.Link.Link -> ConflictSelection.Link.Link -> Conflict.Conflict
  mkHorizontalConflict link1 link2 = conflict
    where
      qcp1 = ConflictSelection.Link.sndColorPoint link1
      pcp2 = ConflictSelection.Link.fstColorPoint link2
      conflict = Conflict.HorizontalConflict pcp2 (ColorPoint.xCoord qcp1)

  -- Test if two links induce a value conflict.
  verticalConflict :: ConflictSelection.Link.Link -> ConflictSelection.Link.Link -> Bool
  verticalConflict l1 l2
    | horizontalConflict l1 l2 = False
    | otherwise           = y1 < y2 && y1' > y2'
    where
      y1  = ColorPoint.yCoord $ ConflictSelection.Link.fstColorPoint l1
      y1' = ColorPoint.yCoord $ ConflictSelection.Link.sndColorPoint l1

      y2  = ColorPoint.yCoord $ ConflictSelection.Link.fstColorPoint l2
      y2' = ColorPoint.yCoord $ ConflictSelection.Link.sndColorPoint l2

  -- Make an order conflict from two links.
  mkVerticalConflict :: ConflictSelection.Link.Link -> ConflictSelection.Link.Link -> Conflict.Conflict
  mkVerticalConflict link1 link2 = conflict
    where
      qcp1 = ConflictSelection.Link.sndColorPoint link1
      pcp2 = ConflictSelection.Link.fstColorPoint link2
      conflict = Conflict.VerticalConflict pcp2 (ColorPoint.yCoord qcp1)

  -- Default ConflictSelection for resolving conflicts.
  defaultConflictSelection :: ConflictSelection
  defaultConflictSelection = leftmostConflict

  -- Find the first conflict in a list of pairs of links.
  firstConflict :: [ConflictSelection.Link2.Link2] -> Maybe Conflict.Conflict
  firstConflict [] = Nothing
  firstConflict (ConflictSelection.Link2.Link2 (link1, link2) : l2s)
    | horizontalConflict link1 link2 = Just $ mkHorizontalConflict link1 link2
    -- | HorizontalConflict link2 link1 = Just $ mkHorizontalConflict link2 link1
    | verticalConflict link1 link2 = Just $ mkVerticalConflict link1 link2
    | verticalConflict link2 link1 = Just $ mkVerticalConflict link2 link1
    | otherwise                 = firstConflict l2s

  -- Return any leftmost w.r.t. x-ccordinates conflict
  leftmostConflict :: ConflictSelection
  leftmostConflict = firstConflict . collect

  -- Return any rightmost w.r.t. x-ccordinates conflict
  rightmostConflict :: ConflictSelection
  rightmostConflict = firstConflict . List.reverse . collect

  -- Return the leftmost order conflict. If such a conflict does not exists,
  -- return the leftmost value conflict. Return Nothing if there is no conflict.
  leftmostHorizontalConflictFirst :: ConflictSelection
  leftmostHorizontalConflictFirst = horizontalConflictFirst Nothing . collect

  -- Return the rightmost order conflict. If such a conflict does not exists,
  -- return the rightmost value conflict. Return Nothing if there is no conflict.
  rightmostHorizontalConflictFirst :: ConflictSelection
  rightmostHorizontalConflictFirst = horizontalConflictFirst Nothing . List.reverse . collect

  --
  horizontalConflictFirst :: Maybe Conflict.Conflict -> [ConflictSelection.Link2.Link2] -> Maybe Conflict.Conflict
  horizontalConflictFirst Nothing   []                              = Nothing
  horizontalConflictFirst vConflict []                              = vConflict
  horizontalConflictFirst vConflict (ConflictSelection.Link2.Link2 (link1, link2) : pl2s)
    | horizontalConflict link1 link2 = Just $ mkHorizontalConflict link1 link2
    | horizontalConflict link2 link1 = Just $ mkHorizontalConflict link2 link1
    | verticalConflict link1 link2 =
        case vConflict of
          Nothing -> horizontalConflictFirst (Just $ mkVerticalConflict link1 link2) pl2s
          _       -> horizontalConflictFirst vConflict pl2s
    | verticalConflict link2 link1 =
        case vConflict of
          Nothing -> horizontalConflictFirst (Just $ mkVerticalConflict link2 link1) pl2s
          _       -> horizontalConflictFirst vConflict pl2s
    | otherwise                    = horizontalConflictFirst vConflict pl2s

  -- Return the leftmost value conflict. If such a conflict does not exists,
  -- return the leftmost order conflict. Return Nothing if there is no conflict.
  leftmostVerticalConflictFirst :: ConflictSelection
  leftmostVerticalConflictFirst = verticalConflictFirst Nothing . collect

  -- Return the rightmost value conflict. If such a conflict does not exists,
  -- return the rightmost order conflict. Return Nothing if there is no conflict.
  rightmostVerticalConflictFirst :: ConflictSelection
  rightmostVerticalConflictFirst = verticalConflictFirst Nothing . List.reverse . collect

  verticalConflictFirst :: Maybe Conflict.Conflict -> [ConflictSelection.Link2.Link2] -> Maybe Conflict.Conflict
  verticalConflictFirst Nothing   []                             = Nothing
  verticalConflictFirst oConflict []                             = oConflict
  verticalConflictFirst oConflict (ConflictSelection.Link2.Link2 (link1, link2) : pl2s)
    | horizontalConflict link1 link2 =
      case oConflict of
        Nothing -> verticalConflictFirst (Just $ mkHorizontalConflict link1 link2) pl2s
        _       -> verticalConflictFirst oConflict pl2s
    | horizontalConflict link2 link1 =
      case oConflict of
        Nothing -> verticalConflictFirst (Just $ mkHorizontalConflict link2 link1) pl2s
        _       -> verticalConflictFirst oConflict pl2s
    | verticalConflict link1 link2 = Just $ mkVerticalConflict link1 link2
    | verticalConflict link2 link1 = Just $ mkVerticalConflict link2 link1
    | otherwise                    = verticalConflictFirst oConflict pl2s
