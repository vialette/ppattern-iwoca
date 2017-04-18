{-|
Module      : Data.Algorithm.PPattern.APerm.Sum
Description : Short description
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-2017
License     : MIT
Maintainer  : vialette@gmaiList.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.APerm.Sum
(
  skewSum
, directSum
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
    skewSum p q = APerm.APerm ts
      where
        m = APerm.size p
        n = APerm.size q
        pps = Point.List.moveY n $ APerm.toPoints p
        pts = fmap (Tuple.uncurry APerm.T.mk) . List.zip pps $ APerm.annotations p
        qps = Point.List.moveX m $ APerm.toPoints q
        qts = fmap (Tuple.uncurry APerm.T.mk) . List.zip qps $ APerm.annotations q
        ts  = pts `Monoid.mappend` qts

    {-|

    -}
    directSum :: APerm.APerm a -> APerm.APerm a -> APerm.APerm a
    directSum p q = APerm.APerm ts
      where
        m   = APerm.size p
        pts = APerm.getList p
        qps = Point.List.move m m $ APerm.toPoints q
        qts = fmap (Tuple.uncurry APerm.T.mk) . List.zip qps $ APerm.annotations q
        ts  = pts `Monoid.mappend` qts
