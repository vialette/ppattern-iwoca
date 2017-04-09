{-|
Module      : Data.Algorithm.PPattern.Strategy
Structription : Short Structription
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-1017
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer Structription of this module, containing some
commentary with @some markup@.
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

  import qualified Data.Algorithm.PPattern.Geometry.ColorPoint as CP
  import qualified Data.Algorithm.PPattern.State               as State
  import qualified Data.Algorithm.PPattern.Combinatorics       as Combinatorics
  import qualified Data.Algorithm.PPattern.Conflict            as Conflict
  import qualified Data.Algorithm.PPattern.List                as List.Tools
  import qualified Data.Algorithm.PPattern.Strategy.Link       as Strategy.Link
  import qualified Data.Algorithm.PPattern.Strategy.PLink      as Strategy.PLink

  -- strategy
  type Strategy = State.State -> Maybe Conflict.Conflict

  -- All pairs of links.
  collect :: State.State -> [Strategy.PLink.PLink]
  collect = fmap f . flip Combinatorics.choose 2 . State.toList
    where
      f = Tuple.uncurry Strategy.PLink.mk . List.Tools.tuplify2 . fmap (Tuple.uncurry Strategy.Link.mk)

  orderConflict :: Strategy.Link.Link -> Strategy.Link.Link -> Bool
  orderConflict link1 link2 = x1 < x2 && x1' > x2'
    where
      x1  = CP.xCoord (Strategy.Link.fstColorPoint link1)
      x1' = CP.xCoord (Strategy.Link.sndColorPoint link1)

      x2  = CP.xCoord (Strategy.Link.fstColorPoint link2)
      x2' = CP.xCoord (Strategy.Link.sndColorPoint link2)

  reportOrderConflict :: Strategy.Link.Link -> Strategy.Link.Link -> Conflict.Conflict
  reportOrderConflict link1 link2 = conflict
    where
      qcp1 = Strategy.Link.sndColorPoint link1
      pcp2 = Strategy.Link.fstColorPoint link2
      conflict = Conflict.OrderConflict pcp2 (CP.xCoord qcp1)

  valueConflict :: Strategy.Link.Link -> Strategy.Link.Link -> Bool
  valueConflict link1 link2 = y1 < y2 && y1' > y2'
    where
      y1  = CP.yCoord (Strategy.Link.fstColorPoint link1)
      y1' = CP.yCoord (Strategy.Link.sndColorPoint link1)

      y2  = CP.yCoord (Strategy.Link.fstColorPoint link2)
      y2' = CP.yCoord (Strategy.Link.sndColorPoint link2)

  reportValueConflict :: Strategy.Link.Link -> Strategy.Link.Link -> Conflict.Conflict
  reportValueConflict link1 link2 = conflict
    where
      qcp1 = Strategy.Link.sndColorPoint link1
      pcp2 = Strategy.Link.fstColorPoint link2
      conflict = Conflict.ValueConflict pcp2 (CP.yCoord qcp1)

  -- Default strategy for resolving conflicts.
  defaultStrategy :: Strategy
  defaultStrategy = leftmostConflict

  firstConflict :: [Strategy.PLink.PLink] -> Maybe Conflict.Conflict
  firstConflict [] = Nothing
  firstConflict (Strategy.PLink.PLink (link1, link2) : plinks)
    | orderConflict link1 link2 = Just $ reportOrderConflict link1 link2
    | orderConflict link2 link1 = Just $ reportOrderConflict link2 link1
    | valueConflict link1 link2 = Just $ reportValueConflict link1 link2
    | valueConflict link2 link1 = Just $ reportValueConflict link2 link1
    | otherwise                 = firstConflict plinks

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

  orderConflictFirst :: Maybe Conflict.Conflict -> [Strategy.PLink.PLink] -> Maybe Conflict.Conflict
  orderConflictFirst Nothing   []                              = Nothing
  orderConflictFirst vConflict []                              = vConflict
  orderConflictFirst vConflict (Strategy.PLink.PLink (link1, link2) : plinks)
    | orderConflict link1 link2 = Just $ reportOrderConflict link1 link2
    | orderConflict link2 link1 = Just $ reportOrderConflict link2 link1
    | valueConflict link1 link2 =
        case vConflict of
          Nothing -> orderConflictFirst (Just $ reportValueConflict link1 link2) plinks
          _       -> orderConflictFirst vConflict plinks
    | valueConflict link2 link1 =
        case vConflict of
          Nothing -> orderConflictFirst (Just $ reportValueConflict link2 link1) plinks
          _       -> orderConflictFirst vConflict plinks
    | otherwise                 = orderConflictFirst vConflict plinks

  -- Return the leftmost value conflict. If such a conflict does not exists,
  -- return the leftmost order conflict. Return Nothing if there is no conflict.
  leftmostValueConflictFirst :: Strategy
  leftmostValueConflictFirst = valueConflictFirst Nothing . collect

  -- Return the rightmost value conflict. If such a conflict does not exists,
  -- return the rightmost order conflict. Return Nothing if there is no conflict.
  rightmostValueConflictFirst :: Strategy
  rightmostValueConflictFirst = valueConflictFirst Nothing . List.reverse . collect

  valueConflictFirst :: Maybe Conflict.Conflict -> [Strategy.PLink.PLink] -> Maybe Conflict.Conflict
  valueConflictFirst Nothing   []                             = Nothing
  valueConflictFirst oConflict []                             = oConflict
  valueConflictFirst oConflict (Strategy.PLink.PLink (link1, link2) : plinks)
    | orderConflict link1 link2 =
      case oConflict of
        Nothing -> valueConflictFirst (Just $ reportOrderConflict link1 link2) plinks
        _       -> valueConflictFirst oConflict plinks
    | orderConflict link2 link1 =
      case oConflict of
        Nothing -> valueConflictFirst (Just $ reportOrderConflict link2 link1) plinks
        _       -> valueConflictFirst oConflict plinks
    | valueConflict link1 link2 = Just $ reportValueConflict link1 link2
    | valueConflict link2 link1 = Just $ reportValueConflict link2 link1
    | otherwise                 = valueConflictFirst oConflict plinks
