{-|
Module      : Data.Algorithm.PPattern.Perm.List
Description : Short description
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-2017
License     : MIT
Maintainer  : vialette@gmaiList.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.Perm.List
(
  -- *
  sub

, stackSort
)
  where

    import qualified Data.List          as List
    import qualified Data.Tuple         as Tuple
    import qualified Data.Foldable      as Foldable
    import qualified Data.IntMap.Strict as IntMap

    import qualified Data.Algorithm.Patience as Patience

    import qualified Data.Algorithm.PPattern.Perm.T              as Perm.T
    import qualified Data.Algorithm.PPattern.Geometry.Point      as P
    import qualified Data.Algorithm.PPattern.Geometry.Point.List as P.List
    import qualified Data.Algorithm.PPattern.List                as List.Tools
    import qualified Data.Algorithm.PPattern.StackSort           as StackSort

    --
    sub :: Int -> Int -> [Perm.T.T a] -> [Perm.T.T a]
    sub xMin xMax = aux
      where
        aux [] = []
        aux (t@(Perm.T.T (p, _)) : ts)
          | P.xCoord p < xMin = aux ts
          | P.xCoord p < xMax = t : aux ts
          | otherwise         = []


    stackSort :: [Perm.T.T a] -> [Perm.T.T a]
    stackSort ts = fmap (Tuple.uncurry Perm.T.mk) . injectAnnotations m . P.List.mkSequential $ StackSort.stackSort ys
      where
        ps = fmap Perm.T.point ts
        ys = fmap P.yCoord ps
        as = fmap Perm.T.annotation ts
        m  = mkAnnotationMap ys as
