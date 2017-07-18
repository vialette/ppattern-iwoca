{-|
Module      : Data.Algorithm.PPattern.APerm.Inner.Monotone
Description : Short description
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-2017
License     : MIT
Maintainer  : vialette@gmaiList.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.Perm.Inner.Monotone
(
  -- * Testing
  isMonotone

  -- * Computing
, longestIncreasing
, longestDecreasing
)
  where

    import qualified Data.List     as List
    import qualified Data.Tuple    as Tuple
    import qualified Data.Foldable as Foldable

    import qualified Data.Algorithm.Patience as Patience

    import qualified Data.Algorithm.PPattern.Geometry.Point  as Point
    import qualified Data.Algorithm.PPattern.List            as List.Tools

    -- Auxiliary function for isIncreasing and isDecreasing
    isMonotone :: (Int -> Int -> Bool) -> [Point.Point] -> Bool
    isMonotone cmp = go
      where
        go []  = True
        go [_] = True
        go aps = Foldable.foldl f True $ List.Tools.consecutive2 aps
          where
            f acc (p, p') = acc && y `cmp` y'
              where
                y  = Point.yCoord p
                y' = Point.yCoord p'

    {-|
      Compute a longest increasing subsequence.
    -}
    longestIncreasing :: [Point.Point] -> [Point.Point]
    longestIncreasing = unformat . List.reverse . doSearch . format
      where
        format   = fmap (\ p -> (Point.yCoord p, p))
        doSearch = Patience.longestIncreasing
        unformat = fmap Tuple.snd

    {-|
      Compute a longest decreasing subsequence.
    -}
    longestDecreasing :: [Point.Point] -> [Point.Point]
    longestDecreasing = unformat . doSearch . format
      where
        format   = List.reverse . fmap (\ p -> (Point.yCoord p, p))
        doSearch = Patience.longestIncreasing
        unformat = fmap Tuple.snd
