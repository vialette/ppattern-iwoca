{-|
Module      : Data.Algorithm.PPattern.APerm.Inner.Sort
Description : Short description
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-2017
License     : MIT
Maintainer  : vialette@gmaiList.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.APerm.Inner.Sort
(
  stackSort
)
  where

    import qualified Data.Tuple         as Tuple

    import qualified Data.Algorithm.PPattern.APerm.Inner.Annotation as APerm.Inner.Annotation
    import qualified Data.Algorithm.PPattern.Geometry.APoint     as APoint
    import qualified Data.Algorithm.PPattern.Geometry.Point      as Point
    import qualified Data.Algorithm.PPattern.Geometry.Point.List as Point.List
    import qualified Data.Algorithm.PPattern.Sort                as Sort

    {-|
      Stack sort a list of annotated points.
    -}
    stackSort :: [APoint.APoint a] -> [APoint.APoint a]
    stackSort aps = fmap (Tuple.uncurry APoint.mk) . APerm.Inner.Annotation.inject m . Point.List.mkSequential $ Sort.stackSort ys
      where
        ys = fmap (Point.yCoord .APoint.point) aps
        as = fmap APoint.annotation aps
        m  = APerm.Inner.Annotation.mkMap ys as
