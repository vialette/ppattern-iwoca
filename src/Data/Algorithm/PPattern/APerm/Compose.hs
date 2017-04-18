{-|
Module      : Data.Algorithm.PPattern.APerm.Compose
Description : Short description
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-2017
License     : MIT
Maintainer  : vialette@gmaiList.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.APerm.Compose
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

    import qualified Data.Algorithm.PPattern.APerm                as APerm
    import qualified Data.Algorithm.PPattern.Geometry.APoint     as APoint
    import qualified Data.Algorithm.PPattern.Geometry.Point.List as Point.List

    {-|

    -}
    skewSum :: APerm.APerm a -> APerm.APerm a -> APerm.APerm a
    skewSum p q = APerm.APerm aps
      where
        m = APerm.size p
        n = APerm.size q
        pps  = Point.List.moveY n $ APerm.toPoints p
        paps = fmap (Tuple.uncurry APoint.mk) . List.zip pps $ APerm.annotations p
        qps  = Point.List.moveX m $ APerm.toPoints q
        qaps = fmap (Tuple.uncurry APoint.mk) . List.zip qps $ APerm.annotations q
        aps  = paps `Monoid.mappend` qaps

    {-|

    -}
    skewSums :: [APerm.APerm a] -> Maybe (APerm.APerm a)
    skewSums []       = Nothing
    skewSums (p : ps) = Just $ skewSumsAux p ps

    skewSumsAux :: APerm.APerm a -> [APerm.APerm a] -> APerm.APerm a
    skewSumsAux p []        = p
    skewSumsAux p (p' : ps) = skewSumsAux (skewSum p p') ps

    {-|

    -}
    directSum :: APerm.APerm a -> APerm.APerm a -> APerm.APerm a
    directSum p q = APerm.APerm aps
      where
        m    = APerm.size p
        paps = APerm.getList p
        qps  = Point.List.move m m $ APerm.toPoints q
        qaps = fmap (Tuple.uncurry APoint.mk) . List.zip qps $ APerm.annotations q
        aps  = paps `Monoid.mappend` qaps

    {-|

    -}
    directSums :: [APerm.APerm a] -> Maybe (APerm.APerm a)
    directSums []       = Nothing
    directSums (p : ps) = Just $ directSumsAux p ps

    directSumsAux :: APerm.APerm a -> [APerm.APerm a] -> APerm.APerm a
    directSumsAux p []        = p
    directSumsAux p (p' : ps) = directSumsAux (directSum p p') ps
