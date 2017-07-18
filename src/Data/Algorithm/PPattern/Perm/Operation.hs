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
    import qualified Data.Algorithm.PPattern.Perm.Inner.Operation as Perm.Inner.Operation


    {-|
      Reverse an Perm.

      >>> Perm.Operation.reversal $ Perm.mk [1,4,5,2,3,6]
      [6,3,2,5,4,1]
      >>> Perm.Operation.reversal $ Perm.mk "adebcf"
      [6,3,2,5,4,1]
    -}
    reversal :: Perm.Perm -> Perm.Perm
    reversal = Perm.apply Perm.Inner.Operation.reversal

    {-|

      >>> Perm.Operation.complement $ Perm.mk [1,4,5,2,3,6]
      [6,3,2,5,4,1]
      >>> Perm.Operation.complement $ Perm.mk "adebcf"
      [6,3,2,5,4,1]
    -}
    complement :: Perm.Perm -> Perm.Perm
    complement = Perm.apply Perm.Inner.Operation.complement

    {-|

      >>> Perm.Operation.reversalComplement $ Perm.mk [1,4,5,2,3,6]
      [1,4,5,2,3,6]
      >>> Perm.Operation.reversalComplement $ Perm.mk "adebcf"
      [1,4,5,2,3,6]
    -}
    reversalComplement :: Perm.Perm -> Perm.Perm
    reversalComplement = Perm.apply Perm.Inner.Operation.reversalComplement

    {-|


    -}
    inverse :: Perm.Perm  -> Perm.Perm
    inverse = Perm.apply Perm.Inner.Operation.inverse
