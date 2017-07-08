{-|
Module      : Data.Algorithm.PPattern.Occurrence
Description : Short description
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-1017
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Pattern matching occurrence.
-}

module Data.Algorithm.PPattern.Occurrence
(
  -- * The @ColorPoint@ type
  Occurrence(..)

  -- * Constructing
, mk

 -- * Querying
, patternAPoints
, patternPoints
, patternAnnotations
, targetAPoints
, targetPoints
, targetAnnotations
)
where

  import qualified Data.List  as List
  import qualified Data.Tuple as Tuple

  import qualified Data.Algorithm.PPattern.Geometry.Point      as Point
  import qualified Data.Algorithm.PPattern.Geometry.APoint     as APoint
  import qualified Data.Algorithm.PPattern.Geometry.ColorPoint as ColorPoint
  import qualified Data.Algorithm.PPattern.APerm               as APerm

  data Element a = Element { getX          :: {-# UNPACK #-} !Int
                           , getY          :: {-# UNPACK #-} !Int
                           , getAnnotation :: a
                           }
                           deriving (Show)

  -- Permutation pattern occurrence datatype
  newtype Occurrence a b = Occurrence [(Element a, Element b)]
                           deriving (Show)

  -- Make an element from the x-ccordinate, the y-coordinate and an annotation.
  mkElement :: Int -> Int -> a -> Element a
  mkElement x y annotation = Element { getX = x
                                     , getY = y
                                     , getAnnotation = annotation
                                     }

  -- Construct a pattern matching ocurrence
  mk :: APerm.APerm a -> APerm.APerm b -> [(ColorPoint.ColorPoint, ColorPoint.ColorPoint)] -> Occurrence a b
  mk p q cps = Occurrence $ List.zip pElements qElements
    where
      pcps = List.sort $ fmap Tuple.fst cps
      ps   = APerm.annotatedPoints p
      pElements = List.reverse $ mkElements ps pcps

      qcps = List.sort $ fmap Tuple.snd cps
      qs   = APerm.annotatedPoints q
      qElements = List.reverse $ mkElements qs qcps

  mkElements :: [APoint.APoint a] -> [ColorPoint.ColorPoint] -> [Element a]
  mkElements = aux []
    where
      aux acc _  [] = acc
      aux _   [] _  = error "Occurrence.mkElements. We shouldn't be there" -- make ghc -Werror happy
      aux acc (ap : aps) cps'@(cp : cps)
        | p == ColorPoint.point cp = aux (e : acc) aps cps
        | otherwise                = aux acc       aps cps'
        where
          p = APoint.point ap
          x = ColorPoint.xCoord cp
          y = ColorPoint.yCoord cp
          a = APoint.annotation ap
          e = mkElement x y a

  toAPoint :: Element a -> APoint.APoint a
  toAPoint e = APoint.mk p a
    where
      p = Point.mk (getX e) (getY e)
      a = getAnnotation e

  {-|
    Extract pattern annotated points from an occurrence.

    >>>
  -}
  patternAPoints :: Occurrence a b -> [APoint.APoint a]
  patternAPoints (Occurrence elements) = fmap (toAPoint . Tuple.fst) elements

  {-|
    Extract pattern points from an occurrence.

    >>>
  -}
  patternPoints :: Occurrence a b -> [Point.Point]
  patternPoints occurrence = APoint.point <$> patternAPoints occurrence

  {-|
    Extract pattern annotations from an occurrence.

    >>>
  -}
  patternAnnotations :: Occurrence a b -> [a]
  patternAnnotations occurrence = APoint.annotation <$> patternAPoints occurrence

  {-|
    Extract target annotated points from an occurrence.

    >>>
  -}
  targetAPoints :: Occurrence a b -> [APoint.APoint b]
  targetAPoints (Occurrence elements) = fmap (toAPoint . Tuple.snd) elements

  {-|
    Extract target points from an occurrence.

    >>>
  -}
  targetPoints :: Occurrence a b -> [Point.Point]
  targetPoints occurrence = APoint.point <$> targetAPoints occurrence


    {-|
      Extract target annotations from an occurrence.

      >>>
    -}
  targetAnnotations :: Occurrence a b -> [b]
  targetAnnotations occurrence = APoint.annotation <$> targetAPoints occurrence
