{-|
Module      : Data.Algorithm.PPattern.Strategy
Structription : Short Structription
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-1017
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Pattern matching search strategy.
-}

module Data.Algorithm.PPattern.Strategy
(
  -- * The @Strategy@ type
  Strategy

  -- * default strategy
, defaultStrategy

  -- * strategies
, leftmostConflict
, rightmostConflict
, leftmostOrderConflictFirst
, rightmostOrderConflictFirst
, leftmostValueConflictFirst
, rightmostValueConflictFirst
)
where

  import qualified Data.Tuple as Tuple
  import qualified Data.List  as List

  import qualified Data.Algorithm.PPattern.Geometry.ColorPoint as ColorPoint
  import qualified Data.Algorithm.PPattern.State               as State
  import qualified Data.Algorithm.PPattern.Combinatorics       as Combinatorics
  import qualified Data.Algorithm.PPattern.Conflict            as Conflict
  import qualified Data.Algorithm.PPattern.List                as List.Tools
  import qualified Data.Algorithm.PPattern.Strategy.Link       as Strategy.Link
  import qualified Data.Algorithm.PPattern.Strategy.Link2      as Strategy.Link2

  -- strategy
  type Strategy = State.State -> Maybe Conflict.Conflict

  -- All pairs of links (i.e. the state embedding as a list of links).
  collect :: State.State -> [Strategy.Link2.Link2]
  collect = fmap f . flip Combinatorics.choose 2 . State.toList
    where
      f = Tuple.uncurry Strategy.Link2.mk . List.Tools.tuplify2 . fmap (Tuple.uncurry Strategy.Link.mk)

  -- Test if two links induce an order conflict.
  orderConflict :: Strategy.Link.Link -> Strategy.Link.Link -> Bool
  orderConflict l1 l2 = x1 < x2 && x1' > x2'
    where
      x1  = ColorPoint.xCoord $ Strategy.Link.fstColorPoint l1
      x1' = ColorPoint.xCoord $ Strategy.Link.sndColorPoint l1

      x2  = ColorPoint.xCoord $ Strategy.Link.fstColorPoint l2
      x2' = ColorPoint.xCoord $ Strategy.Link.sndColorPoint l2

  -- Make an order conflict from two links.
  mkOrderConflict :: Strategy.Link.Link -> Strategy.Link.Link -> Conflict.Conflict
  mkOrderConflict link1 link2 = conflict
    where
      qcp1 = Strategy.Link.sndColorPoint link1
      pcp2 = Strategy.Link.fstColorPoint link2
      conflict = Conflict.OrderConflict pcp2 (ColorPoint.xCoord qcp1)

  -- Test if two links induce a value conflict.
  valueConflict :: Strategy.Link.Link -> Strategy.Link.Link -> Bool
  valueConflict l1 l2
    | orderConflict l1 l2 = False
    | otherwise           = y1 < y2 && y1' > y2'
    where
      y1  = ColorPoint.yCoord $ Strategy.Link.fstColorPoint l1
      y1' = ColorPoint.yCoord $ Strategy.Link.sndColorPoint l1

      y2  = ColorPoint.yCoord $ Strategy.Link.fstColorPoint l2
      y2' = ColorPoint.yCoord $ Strategy.Link.sndColorPoint l2

  -- Make an order conflict from two links.
  mkValueConflict :: Strategy.Link.Link -> Strategy.Link.Link -> Conflict.Conflict
  mkValueConflict link1 link2 = conflict
    where
      qcp1 = Strategy.Link.sndColorPoint link1
      pcp2 = Strategy.Link.fstColorPoint link2
      conflict = Conflict.ValueConflict pcp2 (ColorPoint.yCoord qcp1)

  -- Default strategy for resolving conflicts.
  defaultStrategy :: Strategy
  defaultStrategy = leftmostConflict

  -- Find the first conflict in a list of pairs of links.
  firstConflict :: [Strategy.Link2.Link2] -> Maybe Conflict.Conflict
  firstConflict [] = Nothing
  firstConflict (Strategy.Link2.Link2 (link1, link2) : l2s)
    | orderConflict link1 link2 = Just $ mkOrderConflict link1 link2
    -- | orderConflict link2 link1 = Just $ mkOrderConflict link2 link1
    | valueConflict link1 link2 = Just $ mkValueConflict link1 link2
    | valueConflict link2 link1 = Just $ mkValueConflict link2 link1
    | otherwise                 = firstConflict l2s

  -- Return any leftmost w.r.t. x-ccordinates conflict
  leftmostConflict :: Strategy
  leftmostConflict = firstConflict . collect

  -- Return any rightmost w.r.t. x-ccordinates conflict
  rightmostConflict :: Strategy
  rightmostConflict = firstConflict . List.reverse . collect

  -- Return the leftmost order conflict. If such a conflict does not exists,
  -- return the leftmost value conflict. Return Nothing if there is no conflict.
  leftmostOrderConflictFirst :: Strategy
  leftmostOrderConflictFirst = orderConflictFirst Nothing . collect

  -- Return the rightmost order conflict. If such a conflict does not exists,
  -- return the rightmost value conflict. Return Nothing if there is no conflict.
  rightmostOrderConflictFirst :: Strategy
  rightmostOrderConflictFirst = orderConflictFirst Nothing . List.reverse . collect

  --
  orderConflictFirst :: Maybe Conflict.Conflict -> [Strategy.Link2.Link2] -> Maybe Conflict.Conflict
  orderConflictFirst Nothing   []                              = Nothing
  orderConflictFirst vConflict []                              = vConflict
  orderConflictFirst vConflict (Strategy.Link2.Link2 (link1, link2) : pl2s)
    | orderConflict link1 link2 = Just $ mkOrderConflict link1 link2
    | orderConflict link2 link1 = Just $ mkOrderConflict link2 link1
    | valueConflict link1 link2 =
        case vConflict of
          Nothing -> orderConflictFirst (Just $ mkValueConflict link1 link2) pl2s
          _       -> orderConflictFirst vConflict pl2s
    | valueConflict link2 link1 =
        case vConflict of
          Nothing -> orderConflictFirst (Just $ mkValueConflict link2 link1) pl2s
          _       -> orderConflictFirst vConflict pl2s
    | otherwise                 = orderConflictFirst vConflict pl2s

  -- Return the leftmost value conflict. If such a conflict does not exists,
  -- return the leftmost order conflict. Return Nothing if there is no conflict.
  leftmostValueConflictFirst :: Strategy
  leftmostValueConflictFirst = valueConflictFirst Nothing . collect

  -- Return the rightmost value conflict. If such a conflict does not exists,
  -- return the rightmost order conflict. Return Nothing if there is no conflict.
  rightmostValueConflictFirst :: Strategy
  rightmostValueConflictFirst = valueConflictFirst Nothing . List.reverse . collect

  valueConflictFirst :: Maybe Conflict.Conflict -> [Strategy.Link2.Link2] -> Maybe Conflict.Conflict
  valueConflictFirst Nothing   []                             = Nothing
  valueConflictFirst oConflict []                             = oConflict
  valueConflictFirst oConflict (Strategy.Link2.Link2 (link1, link2) : pl2s)
    | orderConflict link1 link2 =
      case oConflict of
        Nothing -> valueConflictFirst (Just $ mkOrderConflict link1 link2) pl2s
        _       -> valueConflictFirst oConflict pl2s
    | orderConflict link2 link1 =
      case oConflict of
        Nothing -> valueConflictFirst (Just $ mkOrderConflict link2 link1) pl2s
        _       -> valueConflictFirst oConflict pl2s
    | valueConflict link1 link2 = Just $ mkValueConflict link1 link2
    | valueConflict link2 link1 = Just $ mkValueConflict link2 link1
    | otherwise                 = valueConflictFirst oConflict pl2s
