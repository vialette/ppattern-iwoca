{-|
Module      : Data.Algorithm.PPattern.SeparatingTree
Description : Short description
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-2017
License     : MIT
Maintainer  : vialette@gmaiList.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.SeparatingTree
(
  -- * The @SeparatingTree@ type
  SeparatingTree(..)

  -- * Constructing
, mk

, display

  -- * Accessing
, interval
)
where

  import qualified Data.Monoid as Monoid

  import qualified Data.Algorithm.PPattern.Geometry.Point    as P
  import qualified Data.Algorithm.PPattern.Geometry.Interval as I

  data SeparatingTree = Leaf  {-# UNPACK #-} !P.Point
                      | Plus  {-# UNPACK #-} !I.Interval !SeparatingTree !SeparatingTree
                      | Minus {-# UNPACK #-} !I.Interval !SeparatingTree !SeparatingTree
                      deriving (Eq)

  indent :: Show a => Int -> a -> String
  indent n x = concat (replicate n " ") ++ show x ++ "\n"

  display :: Int -> SeparatingTree -> String
  display n (Leaf p) = indent n p
  display n (Plus i l r)  = indent n ("+ " `Monoid.mappend` show i) `Monoid.mappend`
                            display (n+1) l                         `Monoid.mappend`
                            display (n+1) r
  display n (Minus i l r) = indent n ("- " `Monoid.mappend` show i) `Monoid.mappend`
                            display (n+1) l                         `Monoid.mappend`
                            display (n+1) r

  instance Show SeparatingTree where
    show = display 0

  interval :: SeparatingTree -> I.Interval
  interval (Leaf p) = I.mk x x
    where
      x = P.yCoord p
  interval (Plus i _ _)  = i
  interval (Minus i _ _) = i

  mk :: [P.Point] -> Maybe SeparatingTree
  mk = mkAux []

  mkAux :: [SeparatingTree] -> [P.Point] -> Maybe SeparatingTree
  mkAux [t] [] = Just t
  mkAux _   [] = Nothing
  mkAux []    (p : ps) = mkAux [Leaf p] ps
  mkAux stack@(t : ts) (p : ps)
    | y == ub + 1 = mkAux stack'  ps
    | y == lb - 1 = mkAux stack'' ps
    | otherwise   = mkAux (Leaf p : stack) ps
    where
      y  = P.yCoord p

      lb = I.lowerBound $ interval t
      ub = I.upperBound $ interval t

      stack'  = reduce $ Plus  (I.mk lb y) t (Leaf p) : ts
      stack'' = reduce $ Minus (I.mk y ub) t (Leaf p) : ts

  reduce :: [SeparatingTree] -> [SeparatingTree]
  reduce []  = []
  reduce [t] = [t]
  reduce stack@(t : t' : ts)
    | lb == ub' + 1 = reduce $ Plus  (I.mk lb' ub)  t' t : ts
    | ub == lb' - 1 = reduce $ Minus (I.mk lb ub')  t' t : ts
    | otherwise     = stack
    where
      lb = I.lowerBound $ interval t
      ub = I.upperBound $ interval t

      lb' = I.lowerBound $ interval t'
      ub' = I.upperBound $ interval t'
