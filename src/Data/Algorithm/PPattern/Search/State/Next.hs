{-|
Module      : Data.Algorithm.PPattern.State.Next
Description : Jump to next increasing element facility
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-2017
License     : MIT
Maintainer  : vialette@gmaiList.com
Stability   : experimental

Jump to next increasing element facility.
-}

module Data.Algorithm.PPattern.Search.State.Next
(
  -- * The @Next@ type
  Next

  -- * Constructing
, empty
, mk

  -- * Querying
, query
, jumpThreshold
, xJumpThreshold
, yJumpThreshold

  -- * Modifying
, insert
)
where

  import qualified Data.Tuple         as Tuple
  import qualified Data.Foldable      as Foldable
  import qualified Data.Map.Strict    as Map
  import qualified Data.IntMap.Strict as IntMap

  import qualified Data.Algorithm.PPattern.Geometry.ColorPoint as ColorPoint

  -- Colored point to colored point mapping
  newtype Next = Next { getMap :: Map.Map ColorPoint.ColorPoint ColorPoint.ColorPoint }

  -- Make an empty Next mapping.
  empty :: Next
  empty = Next { getMap = Map.empty }

  -- Make a Next mapping from a list of colored points.
  mk :: [ColorPoint.ColorPoint] -> Next
  mk = Tuple.fst . Foldable.foldr f (empty, IntMap.empty)
    where
      f cp (m, m') = case IntMap.lookup c m' of
                       Nothing  -> (m, IntMap.insert c cp m')
                       Just cp' -> (insert cp cp' m, IntMap.insert c cp m')
        where
          c = ColorPoint.color cp

  --
  --
  query :: ColorPoint.ColorPoint -> Next -> Maybe ColorPoint.ColorPoint
  query cp = Map.lookup cp . getMap

  --
  --
  insert :: ColorPoint.ColorPoint -> ColorPoint.ColorPoint -> Next -> Next
  insert cp cp' next = Next { getMap = m }
    where
      m = Map.insert cp cp' $ getMap next

  --
  --
  jumpThreshold ::
    (ColorPoint.ColorPoint -> Int) -> Int ->  Next -> ColorPoint.ColorPoint -> Maybe ColorPoint.ColorPoint
  jumpThreshold f t next cp = aux (query cp next)
    where
      aux Nothing = Nothing
      aux (Just cp')
        | f cp' > t = Just cp'     -- above threshold, done.
        | otherwise = aux (query cp' next) -- below threshold, keep on searching.

  --
  --
  xJumpThreshold :: Int -> Next -> ColorPoint.ColorPoint -> Maybe ColorPoint.ColorPoint
  xJumpThreshold = jumpThreshold ColorPoint.xCoord

  --
  --
  yJumpThreshold :: Int -> Next -> ColorPoint.ColorPoint -> Maybe ColorPoint.ColorPoint
  yJumpThreshold = jumpThreshold ColorPoint.yCoord
