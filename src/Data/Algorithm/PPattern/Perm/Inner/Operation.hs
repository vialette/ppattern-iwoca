{-|
Module      : Data.Algorithm.PPattern.Perm.Inner.Operation
Description : Short description
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-2017
License     : MIT
Maintainer  : vialette@gmaiList.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.Perm.Inner.Operation
(
  reversal
, complement
, reversalComplement
, inverse
)
  where

    import qualified Data.List          as List
    import qualified Data.Tuple         as Tuple
    import qualified Data.Foldable      as Foldable

    import qualified Data.Algorithm.PPattern.Perm.Inner.Annotation as Perm.Inner.Annotation
    import qualified Data.Algorithm.PPattern.Geometry.APoint       as APoint
    import qualified Data.Algorithm.PPattern.Geometry.Point        as Point
    import qualified Data.Algorithm.PPattern.Geometry.Point.List   as Point.List

    {-|
    -}
    reversal :: [APoint.APoint a] -> [APoint.APoint a]
    reversal aps = Foldable.foldl f [] aps
      where
        n = List.length aps

        f acc ap = APoint.mk p' a : acc
          where
            p = APoint.point ap
            a = APoint.annotation ap

            x = Point.xCoord p
            x' = n + 1 - x

            p' = Point.updateXCoord x' p


    {-|
    -}
    complement :: [APoint.APoint a] -> [APoint.APoint a]
    complement aps = fmap (Tuple.uncurry APoint.mk) . Perm.Inner.Annotation.inject m $ complementAux ys
      where
        ps = fmap APoint.point aps
        ys = fmap Point.yCoord ps
        as = fmap APoint.annotation aps
        m  = Perm.Inner.Annotation.mkMap ys as

    {-|
    -}
    complementAux :: [Int] -> [Point.Point]
    complementAux ys = Point.List.mkSequential $ fmap f ys
      where
        n   = List.length ys
        f y = n+1-y

    {-|
    -}
    reversalComplement :: [APoint.APoint a] -> [APoint.APoint a]
    reversalComplement aps = fmap (Tuple.uncurry APoint.mk) . Perm.Inner.Annotation.inject m $ reversalComplementAux ys
      where
        ps = fmap APoint.point aps
        ys = fmap Point.yCoord ps
        as = fmap APoint.annotation aps
        m  = Perm.Inner.Annotation.mkMap ys as

    {-|
    -}
    reversalComplementAux :: [Int] -> [Point.Point]
    reversalComplementAux ys = Point.List.mkSequential . fmap f $ List.reverse ys
      where
        n   = List.length ys
        f y = n+1-y

    {-|
    -}
    inverse :: [APoint.APoint a] -> [APoint.APoint a]
    inverse aps = fmap (Tuple.uncurry APoint.mk) . Perm.Inner.Annotation.inject m $ inverseAux ys
      where
        ps = fmap APoint.point aps
        ys = fmap Point.yCoord ps
        as = fmap APoint.annotation aps
        m  = Perm.Inner.Annotation.mkMap ys as

    -- inverse auxialiary function.
    inverseAux :: [Int] -> [Point.Point]
    inverseAux = Point.List.mkSequential . fmap Tuple.snd . List.sort . flip List.zip [1..]
