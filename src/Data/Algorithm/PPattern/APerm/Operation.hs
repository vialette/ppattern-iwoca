{-|
Module      : Data.Algorithm.PPattern.APerm.Operation
Description : Short description
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-2017
License     : MIT
Maintainer  : vialette@gmaiList.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.APerm.Operation
(
  -- *
  reversal
, complement
, reversalComplement
, inverse
)
  where

    import qualified Data.Algorithm.PPattern.APerm                 as APerm
    import qualified Data.Algorithm.PPattern.APerm.Inner.Operation as APerm.Inner.Operation


    {-|
      Reverse a APermutation.
    -}
    reversal :: APerm.APerm a -> APerm.APerm a
    reversal = APerm.apply APerm.Inner.Operation.reversal

    {-|
    -}
    complement :: APerm.APerm a -> APerm.APerm a
    complement = APerm.apply APerm.Inner.Operation.complement

    {-|
    -}
    reversalComplement :: APerm.APerm a -> APerm.APerm a
    reversalComplement = APerm.apply APerm.Inner.Operation.reversalComplement

    {-|
    -}
    inverse :: APerm.APerm a  -> APerm.APerm a
    inverse = APerm.apply APerm.Inner.Operation.inverse
