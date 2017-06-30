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
      Compute the skew sum of two APerms.

      >>> let p = APerm.mk [1,3,2]
      >>> let q = APerm.mk [3,2,4,1]
      >>> APerm.Compose.skewSum p q
      [5,7,6,3,2,4,1]
      >>> APerm.Compose.skewSum q p
      [6,5,7,4,1,3,2]
    -}
    skewSum :: APerm.APerm a -> APerm.APerm a -> APerm.APerm a
    skewSum p q = APerm.APerm aps
      where
        m = APerm.size p
        n = APerm.size q
        pps  = Point.List.moveY n $ APerm.points p
        paps = fmap (Tuple.uncurry APoint.mk) . List.zip pps $ APerm.annotations p
        qps  = Point.List.moveX m $ APerm.points q
        qaps = fmap (Tuple.uncurry APoint.mk) . List.zip qps $ APerm.annotations q
        aps  = paps `Monoid.mappend` qaps

    {-|
      Compute the skew sum of a list of APerms.

      >>> APerm.Compose.skewSums []
      Nothing
      >>> let p = APerm.mk [1,3,2]
      >>> let q = APerm.mk [3,2,4,1]
      >>> let r = APerm.mk [1,2]
      >>> APerm.Compose.skewSums [p,q,r]
      Just [7,9,8,5,4,6,3,1,2]
      >>> APerm.Compose.skewSums [p,r,q]
      Just [7,9,8,5,6,3,2,4,1]
      >>> APerm.Compose.skewSums [r,p,q]
      Just [8,9,5,7,6,3,2,4,1]
      >>> APerm.Compose.skewSums [r,q,p]
      Just [8,9,6,5,7,4,1,3,2]
      >>> APerm.Compose.skewSums [q,p,r]
      Just [8,7,9,6,3,5,4,1,2]
      >>> APerm.Compose.skewSums [q,r,p]
      Just [8,7,9,6,4,5,1,3,2]
    -}
    skewSums :: [APerm.APerm a] -> Maybe (APerm.APerm a)
    skewSums []       = Nothing
    skewSums (p : ps) = Just $ skewSumsAux p ps

    skewSumsAux :: APerm.APerm a -> [APerm.APerm a] -> APerm.APerm a
    skewSumsAux p []        = p
    skewSumsAux p (p' : ps) = skewSumsAux (skewSum p p') ps

    {-|
      Compute the direct sum of two APerms.

      >>> let p = APerm.mk [1,3,2]
      >>> let q = APerm.mk [3,2,4,1]
      >>> APerm.Compose.directSum p q
      [1,3,2,6,5,7,4]
      >>> APerm.Compose.directSum q p
      [3,2,4,1,5,7,6]
    -}
    directSum :: APerm.APerm a -> APerm.APerm a -> APerm.APerm a
    directSum p q = APerm.APerm aps
      where
        m    = APerm.size p
        paps = APerm.getList p
        qps  = Point.List.move m m $ APerm.points q
        qaps = fmap (Tuple.uncurry APoint.mk) . List.zip qps $ APerm.annotations q
        aps  = paps `Monoid.mappend` qaps

    {-|
      Compute the direct sum of a list of APerms.

      >>> let p = APerm.mk [1,3,2]
      >>> let q = APerm.mk [3,2,4,1]
      >>> let r = APerm.mk [1,2,3]
      >>> APerm.Compose.directSums [p,q,r]
      Just [1,3,2,6,5,7,4,8,9,10]
      >>> APerm.Compose.directSums [p,r,q]
      Just [1,3,2,4,5,6,9,8,10,7]
      >>> APerm.Compose.directSums [q,p,r]
      Just [3,2,4,1,5,7,6,8,9,10]
      >>> APerm.Compose.directSums [q,r,p]
      Just [3,2,4,1,5,6,7,8,10,9]
      >>> APerm.Compose.directSums [r,p,q]
      Just [1,2,3,4,6,5,9,8,10,7]
      >>> APerm.Compose.directSums [r,q,p]
      Just [1,2,3,6,5,7,4,8,10,9]
    -}
    directSums :: [APerm.APerm a] -> Maybe (APerm.APerm a)
    directSums []       = Nothing
    directSums (p : ps) = Just $ directSumsAux p ps

    directSumsAux :: APerm.APerm a -> [APerm.APerm a] -> APerm.APerm a
    directSumsAux p []        = p
    directSumsAux p (p' : ps) = directSumsAux (directSum p p') ps
