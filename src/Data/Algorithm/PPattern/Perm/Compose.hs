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

    import qualified Data.Monoid as Monoid

    import qualified Data.Algorithm.PPattern.Perm as Perm

    {-|
      Compute the skew sum of two Perms.

      >>> let p = Perm.mk [1,3,2]
      >>> let q = Perm.mk [3,2,4,1]
      >>> Perm.Compose.skewSum p q
      [5,7,6,3,2,4,1]
      >>> Perm.Compose.skewSum q p
      [6,5,7,4,1,3,2]
    -}
    skewSum :: Perm.Perm -> Perm.Perm -> Perm.Perm
    skewSum p q = Perm.mk yCoords
      where
        n = Perm.size q
        pYCoords = (+n) <$> Perm.yCoords p
        qYCoords = Perm.yCoords q
        yCoords = pYCoords `Monoid.mappend` qYCoords

    {-|
      Compute the skew sum of a list of Perms.

      >>> Perm.Compose.skewSums []
      Nothing
      >>> let p = Perm.mk [1,3,2]
      >>> let q = Perm.mk [3,2,4,1]
      >>> let r = Perm.mk [1,2]
      >>> Perm.Compose.skewSums [p,q,r]
      Just [7,9,8,5,4,6,3,1,2]
      >>> Perm.Compose.skewSums [p,r,q]
      Just [7,9,8,5,6,3,2,4,1]
      >>> Perm.Compose.skewSums [r,p,q]
      Just [8,9,5,7,6,3,2,4,1]
      >>> Perm.Compose.skewSums [r,q,p]
      Just [8,9,6,5,7,4,1,3,2]
      >>> Perm.Compose.skewSums [q,p,r]
      Just [8,7,9,6,3,5,4,1,2]
      >>> Perm.Compose.skewSums [q,r,p]
      Just [8,7,9,6,4,5,1,3,2]
    -}
    skewSums :: [Perm.Perm] -> Maybe (Perm.Perm)
    skewSums []       = Nothing
    skewSums (p : ps) = Just $ skewSumsAux p ps

    skewSumsAux :: Perm.Perm -> [Perm.Perm] -> Perm.Perm
    skewSumsAux p []        = p
    skewSumsAux p (p' : ps) = skewSumsAux (skewSum p p') ps

    {-|
      Compute the direct sum of two Perms.

      >>> let p = Perm.mk [1,3,2]
      >>> let q = Perm.mk [3,2,4,1]
      >>> Perm.Compose.directSum p q
      [1,3,2,6,5,7,4]
      >>> Perm.Compose.directSum q p
      [3,2,4,1,5,7,6]
    -}
    directSum :: Perm.Perm -> Perm.Perm -> Perm.Perm
    directSum p q = Perm.mk yCoords
      where
        m = Perm.size p
        pYCoords = Perm.yCoords p
        qYcoords = (+m) <$> Perm.yCoords q
        yCoords  = pYCoords `Monoid.mappend` qYcoords

    {-|
      Compute the direct sum of a list of Perms.

      >>> let p = Perm.mk [1,3,2]
      >>> let q = Perm.mk [3,2,4,1]
      >>> let r = Perm.mk [1,2,3]
      >>> Perm.Compose.directSums [p,q,r]
      Just [1,3,2,6,5,7,4,8,9,10]
      >>> Perm.Compose.directSums [p,r,q]
      Just [1,3,2,4,5,6,9,8,10,7]
      >>> Perm.Compose.directSums [q,p,r]
      Just [3,2,4,1,5,7,6,8,9,10]
      >>> Perm.Compose.directSums [q,r,p]
      Just [3,2,4,1,5,6,7,8,10,9]
      >>> Perm.Compose.directSums [r,p,q]
      Just [1,2,3,4,6,5,9,8,10,7]
      >>> Perm.Compose.directSums [r,q,p]
      Just [1,2,3,6,5,7,4,8,10,9]
    -}
    directSums :: [Perm.Perm] -> Maybe (Perm.Perm)
    directSums []       = Nothing
    directSums (p : ps) = Just $ directSumsAux p ps

    directSumsAux :: Perm.Perm -> [Perm.Perm] -> Perm.Perm
    directSumsAux p []        = p
    directSumsAux p (p' : ps) = directSumsAux (directSum p p') ps
