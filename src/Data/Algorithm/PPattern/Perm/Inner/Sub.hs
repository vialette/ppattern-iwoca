{-|
Module      : Data.Algorithm.PPattern.Perm.Inner.Sort
Description : Short description
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-2017
License     : MIT
Maintainer  : vialette@gmaiList.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.Perm.Inner.Sort
(
  -- *
  sub
)
  where

    import qualified Data.List          as List
    import qualified Data.Tuple         as Tuple
    import qualified Data.Foldable      as Foldable
    import qualified Data.IntMap.Strict as IntMap

    import qualified Data.Algorithm.Patience as Patience

    import qualified Data.Algorithm.PPattern.Geometry.Point      as Point
    import qualified Data.Algorithm.PPattern.Geometry.APoint     as APoint
    import qualified Data.Algorithm.PPattern.Geometry.Point.List as P.List
    import qualified Data.Algorithm.PPattern.List                as List.Tools
    import qualified Data.Algorithm.PPattern.StackSort           as StackSort

    --
    sub :: Int -> Int -> [APoint.APoint a] -> [APoint.APoint a]
    sub xMin xMax = aux
      where
        aux [] = []
        aux (ap : aps)
          | x < xMin  = aux aps
          | x < xMax  = ap : aux aps
          | otherwise = []
          where
            x = Point.xCoord $ APoint.point ap
