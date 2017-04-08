{-|
Module      : Data.Algorithm.PPattern.Occurrence
Description : Short description
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-1017
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.Occurrence
(
  -- * The @ColorPoint@ type
  Occurrence(..)

  -- * Constructing
, mk
)
where

  import qualified Data.List  as List
  import qualified Data.Tuple as Tuple

  import qualified Data.Algorithm.PPattern.Geometry.Point      as P
  import qualified Data.Algorithm.PPattern.Geometry.ColorPoint as CP
  import qualified Data.Algorithm.PPattern.Perm                as Perm

  data Element a = Element { getX          :: {-# UNPACK #-} !Int
                           , getY          :: {-# UNPACK #-} !Int
                           , getAnnotation :: a
                           }
                           deriving (Show)

  newtype Occurrence a b = Occurrence [(Element a, Element b)]
                           deriving (Show)

  mkElement :: Int -> Int -> a -> Element a
  mkElement x y annotation = Element { getX = x
                                     , getY = y
                                     , getAnnotation = annotation
                                     }

  mk :: Perm.Perm a -> Perm.Perm b -> [(CP.ColorPoint, CP.ColorPoint)] -> Occurrence a b
  mk p q cps = Occurrence $ List.zip pElements qElements
    where
      pcps = List.sort $ fmap Tuple.fst cps
      ps   = Perm.toAnnotedList p
      pElements = List.reverse $ mkElements ps pcps

      qcps = List.sort $ fmap Tuple.snd cps
      qs   = Perm.toAnnotedList q
      qElements = List.reverse $ mkElements qs qcps

  mkElements :: [(P.Point, a)] -> [CP.ColorPoint] -> [Element a]
  mkElements = aux []
    where
      aux acc _  [] = acc
      aux _   [] _  = error "Occurrence.mkElements. We shouldn't be there" -- make ghc -Werror happy
      aux acc ((p, a) : pas) cps'@(cp : cps)
        | p == CP.point cp = aux (e : acc) pas cps
        | otherwise        = aux acc       pas cps'
        where
          x = CP.xCoord cp
          y = CP.yCoord cp
          e = mkElement x y a
