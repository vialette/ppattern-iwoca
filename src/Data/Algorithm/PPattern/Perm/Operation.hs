{-|
Module      : Data.Algorithm.PPattern.Perm.Operation
Description : Short description
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-2017
License     : MIT
Maintainer  : vialette@gmaiList.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.Perm.Operation
(
  -- *
  reversal
, complement
, reversalComplement
, inverse
)
  where

    import qualified Data.Algorithm.PPattern.Perm                 as Perm
    import qualified Data.Algorithm.PPattern.Geometry.APoint      as APoint
    import qualified Data.Algorithm.PPattern.Perm.Inner.Operation as Perm.Inner.Operation


    {-|
      Reverse a permutation.
    -}
    reversal :: Perm.Perm a -> Perm.Perm a
    reversal = Perm.apply Perm.Inner.Operation.reversal

    {-|
    -}
    complement :: Perm.Perm a -> Perm.Perm a
    complement = Perm.apply Perm.Inner.Operation.complement

    {-|
    -}
    reversalComplement :: Perm.Perm a -> Perm.Perm a
    reversalComplement = Perm.apply Perm.Inner.Operation.reversalComplement

    {-|
    -}
    inverse :: Perm.Perm a  -> Perm.Perm a
    inverse = Perm.apply Perm.Inner.Operation.inverse 
