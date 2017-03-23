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
, leftmostOrderConflictFirst
, rightmostOrderConflictFirst
, leftmostValueConflictFirst
, rightmostValueConflictFirst
)
where

  import qualified Data.Tuple as T
  import qualified Data.List  as L

  import qualified Data.Algorithm.PPattern.State    as State
  import qualified Data.Algorithm.PPattern.CPoint   as CPoint
  import qualified Data.Algorithm.PPattern.Combi    as Combi
  import qualified Data.Algorithm.PPattern.Conflict as Conflict
  import qualified Data.Algorithm.PPattern.Tools    as Tools
  import qualified Data.Algorithm.PPattern.Strategy.Link  as Strategy.Link
  import qualified Data.Algorithm.PPattern.Strategy.Strategy.PLink.PLink as Strategy.Strategy.PLink.PLink

  -- strategy
  type Strategy = State.State -> Maybe Conflict.Conflict

  ------------------------------------------------------------------------------
  --
  -- Strategies
  --
  ------------------------------------------------------------------------------

  -- All pairs of links.
  collect :: State.State -> [Strategy.PLink.PLink]
  collect s = fmap f . flip Combi.choose 2 $ State.embeddingToList e
    where
      e = State.embedding s
      f = T.uncurry mkStrategy.PLink.PLink . Tools.tuplify2 . fmap (T.uncurry mkLink)

  orderConflict :: Strategy.Link.Link -> Strategy.Link.Link -> Bool
  orderConflict link1 link2 = x1 < x2 && x1' > x2'
    where
      x1  = CPoint.xCoord (fstCPoint link1)
      x1' = CPoint.xCoord (sndCPoint link1)

      x2  = CPoint.xCoord (fstCPoint link2)
      x2' = CPoint.xCoord (sndCPoint link2)

  reportOrderConflict :: Strategy.Link.Link -> Strategy.Link.Link -> Conflict.Conflict
  reportOrderConflict link1 link2 = conflict
    where
      qcp1 = sndCPoint link1
      pcp2 = fstCPoint link2
      conflict = Conflict.OrderConflict pcp2 (CPoint.xCoord qcp1)

  valueConflict :: Strategy.Link.Link -> Strategy.Link.Link -> Bool
  valueConflict link1 link2 = y1 < y2 && y1' > y2'
    where
      y1  = CPoint.yCoord (fstCPoint link1)
      y1' = CPoint.yCoord (sndCPoint link1)

      y2  = CPoint.yCoord (fstCPoint link2)
      y2' = CPoint.yCoord (sndCPoint link2)

  reportValueConflict :: Strategy.Link.Link -> Strategy.Link.Link -> Conflict.Conflict
  reportValueConflict link1 link2 = conflict
    where
      qcp1 = sndCPoint link1
      pcp2 = fstCPoint link2
      conflict = Conflict.ValueConflict pcp2 (CPoint.yCoord qcp1)

  -- Default strategy for resolving conflicts.
  defaultStrategy :: Strategy
  defaultStrategy = leftmostConflict

  -- Return any leftmost w.r.t. x-ccordinates conflict
  leftmostConflict :: Strategy
  leftmostConflict = aux . collect
    where
      aux [] = Nothing
      aux (Strategy.PLink.PLink (link1, link2) : Strategy.PLink.PLinks)
        | orderConflict link1 link2 = Just $ reportOrderConflict link1 link2
        | orderConflict link2 link1 = Just $ reportOrderConflict link2 link1
        | valueConflict link1 link2 = Just $ reportValueConflict link1 link2
        | valueConflict link2 link1 = Just $ reportValueConflict link2 link1
        | otherwise                 = aux Strategy.PLink.PLinks

  -- Return the leftmost order conflict. If such a conflict does not exists,
  -- return the leftmost value conflict. Return Nothing if there is no conflict.
  leftmostOrderConflictFirst :: Strategy
  leftmostOrderConflictFirst = orderConflictFirst Nothing . collect

  -- Return the rightmost order conflict. If such a conflict does not exists,
  -- return the rightmost value conflict. Return Nothing if there is no conflict.
  rightmostOrderConflictFirst :: Strategy
  rightmostOrderConflictFirst = orderConflictFirst Nothing . L.reverse . collect

  orderConflictFirst :: Maybe Conflict.Conflict -> [Strategy.PLink.PLink] -> Maybe Conflict.Conflict
  orderConflictFirst Nothing   []                              = Nothing
  orderConflictFirst vConflict []                              = vConflict
  orderConflictFirst vConflict (Strategy.PLink.PLink (link1, link2) : Strategy.PLink.PLinks)
    | orderConflict link1 link2 = Just $ reportOrderConflict link1 link2
    | orderConflict link2 link1 = Just $ reportOrderConflict link2 link1
    | valueConflict link1 link2 =
        case vConflict of
          Nothing -> orderConflictFirst (Just $ reportValueConflict link1 link2) Strategy.PLink.PLinks
          _       -> orderConflictFirst vConflict Strategy.PLink.PLinks
    | valueConflict link2 link1 =
        case vConflict of
          Nothing -> orderConflictFirst (Just $ reportValueConflict link2 link1) Strategy.PLink.PLinks
          _       -> orderConflictFirst vConflict Strategy.PLink.PLinks
    | otherwise                 = orderConflictFirst vConflict Strategy.PLink.PLinks

  -- Return the leftmost value conflict. If such a conflict does not exists,
  -- return the leftmost order conflict. Return Nothing if there is no conflict.
  leftmostValueConflictFirst :: Strategy
  leftmostValueConflictFirst = valueConflictFirst Nothing . collect

  -- Return the rightmost value conflict. If such a conflict does not exists,
  -- return the rightmost order conflict. Return Nothing if there is no conflict.
  rightmostValueConflictFirst :: Strategy
  rightmostValueConflictFirst = valueConflictFirst Nothing . L.reverse . collect

  valueConflictFirst :: Maybe Conflict.Conflict -> [Strategy.PLink.PLink] -> Maybe Conflict.Conflict
  valueConflictFirst Nothing   []                             = Nothing
  valueConflictFirst oConflict []                             = oConflict
  valueConflictFirst oConflict (Strategy.PLink.PLink (link1, link2) : Strategy.PLink.PLinks)
    | orderConflict link1 link2 =
      case oConflict of
        Nothing -> valueConflictFirst (Just $ reportOrderConflict link1 link2) Strategy.PLink.PLinks
        _       -> valueConflictFirst oConflict Strategy.PLink.PLinks
    | orderConflict link2 link1 =
      case oConflict of
        Nothing -> valueConflictFirst (Just $ reportOrderConflict link2 link1) Strategy.PLink.PLinks
        _       -> valueConflictFirst oConflict Strategy.PLink.PLinks
    | valueConflict link1 link2 = Just $ reportValueConflict link1 link2
    | valueConflict link2 link1 = Just $ reportValueConflict link2 link1
    | otherwise                 = valueConflictFirst oConflict Strategy.PLink.PLinks
