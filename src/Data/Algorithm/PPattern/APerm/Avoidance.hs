{-|
Module      : Data.Algorithm.PPattern.APerm.Avoidance
Description : Short description
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-2017
License     : MIT
Maintainer  : vialette@gmaiList.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.APerm.Avoidance
(
  -- * Avoiding a pattern of length 3
  is123Avoiding
, is132Avoiding
, is213Avoiding
, is231Avoiding
, is312Avoiding
, is321Avoiding
)
  where

    import qualified Data.Algorithm.PPattern.APerm           as APerm
    import qualified Data.Algorithm.PPattern.APerm.Operation as APerm.Operation
    import qualified Data.Algorithm.PPattern.APerm.Sort      as APerm.Sort

    {-|
      'is123Avoiding p' returns True if an only if APermutation 'p' avoids 123.
    -}
    is123Avoiding :: APerm.APerm a -> Bool
    is123Avoiding _ = True

    {-|
      'is132Avoiding p' returns True if an only if APermutation 'p' avoids 132.
    -}
    is132Avoiding :: APerm.APerm a -> Bool
    is132Avoiding = is231Avoiding . APerm.Operation.reversal

    {-|
      'is213Avoiding p' returns True if an only if APermutation 'p' avoids 213.
    -}
    is213Avoiding :: APerm.APerm a -> Bool
    is213Avoiding = is132Avoiding . APerm.Operation.reversalComplement

    {-|
      'is231Avoiding p' returns True if an only if APermutation 'p' avoids 231.
    -}
    is231Avoiding :: APerm.APerm a -> Bool
    is231Avoiding = APerm.Sort.isStackSortable

    {-|
      'is312Avoiding p' returns True if an only if APermutation 'p' avoids 312.
    -}
    is312Avoiding :: APerm.APerm a -> Bool
    is312Avoiding = is132Avoiding . APerm.Operation.complement

    {-|
      'is321Avoiding p' returns True if an only if APermutation 'p' avoids 321.
    -}
    is321Avoiding :: APerm.APerm a -> Bool
    is321Avoiding _ = True
