{-|
Module      : Data.Algorithm.PPattern.Strategy
Structription : Short Structription
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-1017
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Pattern matching search Strategy.
-}

module Data.Algorithm.PPattern.Search.ConflictSelection
(
  -- * The @Strategy@ type
  Strategy(..)

  -- * default Strategy
, getConflict
)
where

  import qualified Data.Tuple as Tuple
  import qualified Data.List  as List

  import qualified Data.Algorithm.PPattern.Geometry.ColorPoint            as ColorPoint
  import qualified Data.Algorithm.PPattern.Combinatorics                  as Combinatorics
  import qualified Data.Algorithm.PPattern.Search.State                   as State
  import qualified Data.Algorithm.PPattern.Search.Conflict                as Conflict
  import qualified Data.Algorithm.PPattern.Search.ConflictSelection.Link  as Strategy.Link
  import qualified Data.Algorithm.PPattern.Search.ConflictSelection.Link2 as Strategy.Link2
  import qualified Data.Algorithm.PPattern.List                           as List.Tools

  -- Conflict selection
  data Strategy = DefaultStrategy
                | LeftmostConflictFirst
                | LeftmostHorizontalConflictFirst
                | LeftmostVerticalConflictFirst
                | RightmostConflictFirst
                | RightmostHorizontalConflictFirst
                | RightmostVerticalConflictFirst

  instance Show Strategy where
    show DefaultStrategy                  = "default conflict selection strategy"
    show LeftmostConflictFirst            = "leftmost conflict first"
    show LeftmostHorizontalConflictFirst  = "leftmost horizontal conflict first"
    show LeftmostVerticalConflictFirst    = "leftmost vertical conflict first"
    show RightmostConflictFirst           = "rightmost conflict first"
    show RightmostHorizontalConflictFirst = "rightmost horizontal conflict first"
    show RightmostVerticalConflictFirst   = "rightmost vertical conflict first"

  -- CConflict selection function
  type ConflictSelection = State.State -> Maybe Conflict.Conflict

  -- Get the conflict selection function.
  getConflictSelection :: Strategy -> ConflictSelection
  getConflictSelection DefaultStrategy = defaultStrategy
  getConflictSelection LeftmostConflictFirst            = leftmostConflictFirst
  getConflictSelection LeftmostHorizontalConflictFirst  = leftmostHorizontalConflictFirst
  getConflictSelection LeftmostVerticalConflictFirst    = leftmostVerticalConflictFirst
  getConflictSelection RightmostConflictFirst           = rightmostConflictFirst
  getConflictSelection RightmostHorizontalConflictFirst = rightmostHorizontalConflictFirst
  getConflictSelection RightmostVerticalConflictFirst   = rightmostVerticalConflictFirst

  -- Get a conflict according to some conflict selection strategy.
  getConflict :: Strategy -> ConflictSelection
  getConflict cs = f
    where
      f = getConflictSelection cs

  -- All pairs of links (i.e. the state embedding as a list of links).
  collect :: State.State -> [Strategy.Link2.Link2]
  collect = fmap f . flip Combinatorics.choose 2 . State.toList
    where
      f = Tuple.uncurry Strategy.Link2.mk . List.Tools.tuplify2 . fmap (Tuple.uncurry Strategy.Link.mk)

  -- Test if two links induce an order conflict.
  horizontalConflict :: Strategy.Link.Link -> Strategy.Link.Link -> Bool
  horizontalConflict l1 l2 = x1 < x2 && x1' > x2'
    where
      x1  = ColorPoint.xCoord $ Strategy.Link.fstColorPoint l1
      x1' = ColorPoint.xCoord $ Strategy.Link.sndColorPoint l1

      x2  = ColorPoint.xCoord $ Strategy.Link.fstColorPoint l2
      x2' = ColorPoint.xCoord $ Strategy.Link.sndColorPoint l2

  -- Make an order conflict from two links.
  mkHorizontalConflict :: Strategy.Link.Link -> Strategy.Link.Link -> Conflict.Conflict
  mkHorizontalConflict link1 link2 = conflict
    where
      qcp1 = Strategy.Link.sndColorPoint link1
      pcp2 = Strategy.Link.fstColorPoint link2
      conflict = Conflict.HorizontalConflict pcp2 (ColorPoint.xCoord qcp1)

  -- Test if two links induce a value conflict.
  verticalConflict :: Strategy.Link.Link -> Strategy.Link.Link -> Bool
  verticalConflict l1 l2
    | horizontalConflict l1 l2 = False
    | otherwise           = y1 < y2 && y1' > y2'
    where
      y1  = ColorPoint.yCoord $ Strategy.Link.fstColorPoint l1
      y1' = ColorPoint.yCoord $ Strategy.Link.sndColorPoint l1

      y2  = ColorPoint.yCoord $ Strategy.Link.fstColorPoint l2
      y2' = ColorPoint.yCoord $ Strategy.Link.sndColorPoint l2

  -- Make an order conflict from two links.
  mkVerticalConflict :: Strategy.Link.Link -> Strategy.Link.Link -> Conflict.Conflict
  mkVerticalConflict link1 link2 = conflict
    where
      qcp1 = Strategy.Link.sndColorPoint link1
      pcp2 = Strategy.Link.fstColorPoint link2
      conflict = Conflict.VerticalConflict pcp2 (ColorPoint.yCoord qcp1)

  -- Find the first conflict in a list of pairs of links.
  firstConflict :: [Strategy.Link2.Link2] -> Maybe Conflict.Conflict
  firstConflict [] = Nothing
  firstConflict (Strategy.Link2.Link2 (link1, link2) : l2s)
    | horizontalConflict link1 link2 = Just $ mkHorizontalConflict link1 link2
    -- | HorizontalConflict link2 link1 = Just $ mkHorizontalConflict link2 link1
    | verticalConflict link1 link2 = Just $ mkVerticalConflict link1 link2
    | verticalConflict link2 link1 = Just $ mkVerticalConflict link2 link1
    | otherwise                 = firstConflict l2s

  --
  horizontalConflictFirst :: Maybe Conflict.Conflict -> [Strategy.Link2.Link2] -> Maybe Conflict.Conflict
  horizontalConflictFirst Nothing   []                              = Nothing
  horizontalConflictFirst vConflict []                              = vConflict
  horizontalConflictFirst vConflict (Strategy.Link2.Link2 (link1, link2) : pl2s)
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

  --
  verticalConflictFirst :: Maybe Conflict.Conflict -> [Strategy.Link2.Link2] -> Maybe Conflict.Conflict
  verticalConflictFirst Nothing   []                             = Nothing
  verticalConflictFirst oConflict []                             = oConflict
  verticalConflictFirst oConflict (Strategy.Link2.Link2 (link1, link2) : pl2s)
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

  -- Default Strategy for resolving conflicts.
  defaultStrategy :: ConflictSelection
  defaultStrategy = leftmostConflictFirst

  -- Return any leftmost w.r.t. x-ccordinates conflict
  leftmostConflictFirst :: ConflictSelection
  leftmostConflictFirst = firstConflict . collect

  -- Return the leftmost order conflict. If such a conflict does not exists,
  -- return the leftmost value conflict. Return Nothing if there is no conflict.
  leftmostHorizontalConflictFirst :: ConflictSelection
  leftmostHorizontalConflictFirst = horizontalConflictFirst Nothing . collect

  -- Return the rightmost order conflict. If such a conflict does not exists,
  -- return the rightmost value conflict. Return Nothing if there is no conflict.
  rightmostHorizontalConflictFirst :: ConflictSelection
  rightmostHorizontalConflictFirst = horizontalConflictFirst Nothing . List.reverse . collect

  -- Return any rightmost w.r.t. x-ccordinates conflict
  rightmostConflictFirst :: ConflictSelection
  rightmostConflictFirst = firstConflict . List.reverse . collect

  -- Return the leftmost value conflict. If such a conflict does not exists,
  -- return the leftmost order conflict. Return Nothing if there is no conflict.
  leftmostVerticalConflictFirst :: ConflictSelection
  leftmostVerticalConflictFirst = verticalConflictFirst Nothing . collect

  -- Return the rightmost value conflict. If such a conflict does not exists,
  -- return the rightmost order conflict. Return Nothing if there is no conflict.
  rightmostVerticalConflictFirst :: ConflictSelection
  rightmostVerticalConflictFirst = verticalConflictFirst Nothing . List.reverse . collect
