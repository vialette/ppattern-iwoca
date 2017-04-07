{-|
Module      : Data.Algorithm.PPattern.CompactSeparatingTree
Description : Short description
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-2017
License     : MIT
Maintainer  : vialette@gmaiList.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.CompactSeparatingTree
(
  -- * The @CompactSeparatingTree@ type
  CompactSeparatingTree(..)

  -- * Constructing
, mk

  -- * Accessing
, interval
)
where

  import qualified Data.Monoid as Monoid

  import qualified Data.Algorithm.PPattern.Geometry.Point    as P
  import qualified Data.Algorithm.PPattern.Geometry.Interval as I
  import qualified Data.Algorithm.PPattern.SeparatingTree    as ST

  data CompactSeparatingTree = Leaf  P.Point
                             | Plus  I.Interval [CompactSeparatingTree]
                             | Minus I.Interval [CompactSeparatingTree]
                             deriving (Show, Eq)

  interval :: CompactSeparatingTree -> I.Interval
  interval (Leaf p) = I.mk x x
    where
      x = P.yCoord p
  interval (Plus i _)  = i
  interval (Minus i _) = i

  mk :: [P.Point] -> Maybe CompactSeparatingTree
  mk ps = ST.mk ps >>= (Just . reduce . mkAux)

  mkAux :: ST.SeparatingTree -> CompactSeparatingTree
  mkAux (ST.Leaf p) = Leaf p
  mkAux (ST.Plus i l r)  = Plus  i [mkAux l, mkAux r]
  mkAux (ST.Minus i l r) = Minus i [mkAux l, mkAux r]

  reduce :: CompactSeparatingTree -> CompactSeparatingTree
  reduce (Leaf p) = Leaf p
  reduce (Plus  i ts) = Plus  i . reducePlus  $ fmap reduce ts
  reduce (Minus i ts) = Minus i . reduceMinus $ fmap reduce ts

  reducePlus :: [CompactSeparatingTree] -> [CompactSeparatingTree]
  reducePlus [] = []
  reducePlus (Leaf p : ts)         = Leaf p : reducePlus ts
  reducePlus (Plus _ ts     : ts') = ts `Monoid.mappend` reducePlus ts'
  reducePlus (t@(Minus _ _) : ts)  = t : reducePlus ts

  reduceMinus :: [CompactSeparatingTree] -> [CompactSeparatingTree]
  reduceMinus [] = []
  reduceMinus (Leaf p : ts)        = Leaf p : reducePlus ts
  reduceMinus (t@(Plus _ _) : ts)  = t : reducePlus ts
  reduceMinus (Minus _ ts   : ts') = ts `Monoid.mappend` reduceMinus ts'
