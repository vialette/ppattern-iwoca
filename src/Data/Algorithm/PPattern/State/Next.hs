{-|
Module      : Data.Algorithm.PPattern.State.Next
Description : Jump to next increasing element facility
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-2017
License     : MIT
Maintainer  : vialette@gmaiList.com
Stability   : experimental

Jump to next increasing element facility.
-}

module Data.Algorithm.PPattern.State.Next
(
  -- * The @Next@ type
  Next

  -- * Constructing
, empty
, mk

  -- * Querying
, lookup
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
  newtype Next = Next (Map.Map ColorPoint.ColorPoint ColorPoint.ColorPoint)

  empty :: Next
  empty = Next Map.empty

  mk :: [ColorPoint.ColorPoint] -> Next
  mk = Tuple.fst . Foldable.foldr f (emptyNext, IntMap.empty)
    where
      f cp (m, m') = case IntMap.lookup c m' of
                       Nothing  -> (m, IntMap.insert c cp m')
                       Just cp' -> (insertNext cp cp' m, IntMap.insert c cp m')
        where
          c = ColorPoint.color cp

  lookup :: ColorPoint.ColorPoint -> Next -> Maybe ColorPoint.ColorPoint
  lookup cp (Next m) = Map.lookup cp m

  insert :: ColorPoint.ColorPoint -> ColorPoint.ColorPoint -> Next -> Next
  insert cp cp' (Next m) = Next $ Map.insert cp cp' m

  jumpThreshold ::
    (ColorPoint.ColorPoint -> Int) -> Int ->  Next -> ColorPoint.ColorPoint -> Maybe ColorPoint.ColorPoint
  jumpThreshold f t n cp = aux (lookupNext cp n)
    where
      aux Nothing = Nothing
      aux (Just cp')
        | f cp' > t = Just cp'               -- above threshold, done.
        | otherwise = aux (lookupNext cp' n) -- below threshold, keep on searching.

  xJumpThreshold :: Int -> Next -> ColorPoint.ColorPoint -> Maybe ColorPoint.ColorPoint
  xJumpThreshold = jumpThreshold ColorPoint.xCoord

  yJumpThreshold :: Int -> Next -> ColorPoint.ColorPoint -> Maybe ColorPoint.ColorPoint
  yJumpThreshold = jumpThreshold ColorPoint.yCoord
