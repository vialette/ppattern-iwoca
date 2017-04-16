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
    import qualified Data.IntMap.Strict as IntMap

    import qualified Data.Algorithm.Patience as Patience

    import qualified Data.Algorithm.PPattern.Geometry.APoint      as APoint
    import qualified Data.Algorithm.PPattern.Geometry.Point      as P
    import qualified Data.Algorithm.PPattern.Geometry.Point.List as P.List
    import qualified Data.Algorithm.PPattern.List                as List.Tools
    import qualified Data.Algorithm.PPattern.StackSort           as StackSort

    {-|
    -}
    reversal :: [APoint.APoint a] -> [APoint.APoint a]
    reversal ts = Foldable.foldl f [] ts
      where
        n = List.length ts

        f acc (Perm.T.T (p, a)) = Perm.T.mk p' a : acc
          where
            x = Point.xCoord p
            x' = n + 1 - x

            p' = Point.updateXCoord x' p

    {-|
    -}
    injectAnnotations :: IntMap.IntMap a -> [Point.Point] -> [(Point.Point, a)]
    injectAnnotations m = fmap mk
      where
        mk p = case IntMap.lookup (P.yCoord p) m of
                 Nothing -> error "Data.Algorithm.PPattern.Perm.List.merge. Empty map"
                 Just a  -> (p, a)

    {-|
    -}
    mkAnnotationMap :: [Int] -> [a] -> IntMap.IntMap a
    mkAnnotationMap ys as = IntMap.fromList $ List.zip ys as

    {-|
    -}
    complement :: [APoint.APoint a] -> [APoint.APoint a]
    complement ts = fmap (Tuple.uncurry Perm.T.mk) . injectAnnotations m $ complementAux ys
      where
        ps = fmap Perm.T.point ts
        ys = fmap Point.yCoord ps
        as = fmap Perm.T.annotation ts
        m  = mkAnnotationMap ys as

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
    reversalComplement ts = fmap (Tuple.uncurry Perm.T.mk) . injectAnnotations m $ reversalComplementAux ys
      where
        ps = fmap Perm.T.point ts
        ys = fmap Point.yCoord ps
        as = fmap Perm.T.annotation ts
        m  = mkAnnotationMap ys as

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
    inverse ts = fmap (Tuple.uncurry Perm.T.mk) . injectAnnotations m $ inverseAux ys
      where
        ps = fmap Perm.T.point ts
        ys = fmap Point.yCoord ps
        as = fmap Perm.T.annotation ts
        m  = mkAnnotationMap ys as

    -- inverse auxialiary function.
    inverseAux :: [Int] -> [Point.Point]
    inverseAux = Point.List.mkSequential . fmap Tuple.snd . List.sort . flip List.zip [1..]
