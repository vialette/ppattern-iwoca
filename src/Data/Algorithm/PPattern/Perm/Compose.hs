{-|
Module      : Data.Algorithm.PPattern.Perm.Compose
Description : Short description
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-2017
License     : MIT
Maintainer  : vialette@gmaiList.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.Perm.Compose
(
  skewSum
, skewSums
, directSum
, directSums
)
  where

    import qualified Data.List   as List
    import qualified Data.Tuple  as Tuple
    import qualified Data.Monoid as Monoid

    import qualified Data.Algorithm.PPattern.Perm                as Perm
    import qualified Data.Algorithm.PPattern.Geometry.APoint     as APoint
    import qualified Data.Algorithm.PPattern.Geometry.Point.List as Point.List

    {-|

    -}
    skewSum :: Perm.Perm a -> Perm.Perm a -> Perm.Perm a
    skewSum p q = Perm.Perm aps
      where
        m = Perm.size p
        n = Perm.size q
        pps  = Point.List.moveY n $ Perm.toPoints p
        paps = fmap (Tuple.uncurry APoint.mk) . List.zip pps $ Perm.annotations p
        qps  = Point.List.moveX m $ Perm.toPoints q
        qaps = fmap (Tuple.uncurry APoint.mk) . List.zip qps $ Perm.annotations q
        aps  = pts `Monoid.mappend` qts

    {-|

    -}
    skewSums :: [Perm.Perm a] -> Maybe (Perm.Perm a)
    skewSums []       = Nothing
    skewSums (p : ps) = Just $ aux p ps
      where
        aux p []        = p
        aux p (p' : ps) = aux p'' ps
          where
            p'' = skewSum p p'

    {-|

    -}
    directSum :: Perm.Perm a -> Perm.Perm a -> Perm.Perm a
    directSum p q = Perm.Perm ts
      where
        m    = Perm.size p
        paps = Perm.getList p
        qps  = Point.List.move m m $ Perm.toPoints q
        qaps = fmap (Tuple.uncurry APoint.mk) . List.zip qps $ Perm.annotations q
        aps  = paps `Monoid.mappend` qaps

    {-|

    -}
    skewSums :: [Perm.Perm a] -> Maybe (Perm.Perm a)
    skewSums []       = Nothing
    skewSums (p : ps) = Just $ aux p ps
      where
        aux p []        = p
        aux p (p' : ps) = aux p'' ps
          where
            p'' = skewSum p p'
