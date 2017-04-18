{-|
Module      : Data.Algorithm.PPattern.Perm.Avoidance
Description : Short description
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-2017
License     : MIT
Maintainer  : vialette@gmaiList.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.Perm.Avoidance
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

    import Data.List as List

    import qualified Data.Algorithm.PPattern.Perm           as Perm
    import qualified Data.Algorithm.PPattern.Perm.Operation as Perm.Operation

    {-|
      'is123Avoiding p' returns True if an only if permutation 'p' avoids 123.
    -}
    is123Avoiding :: Perm.Perm a -> Bool
    is123Avoiding _ = True

    {-|
      'is132Avoiding p' returns True if an only if permutation 'p' avoids 132.
    -}
    is132Avoiding :: Perm.Perm a -> Bool
    is132Avoiding = is231Avoiding . Perm.Operation.reversal

    {-|
      'is213Avoiding p' returns True if an only if permutation 'p' avoids 213.
    -}
    is213Avoiding :: Perm.Perm a -> Bool
    is213Avoiding = is132Avoiding . Perm.Operation.reversalComplement

    {-|
      'is231Avoiding p' returns True if an only if permutation 'p' avoids 231.
    -}
    is231Avoiding :: Perm.Perm a -> Bool
    is231Avoiding = Perm.Sort.isStackSortable

    {-|
      'is312Avoiding p' returns True if an only if permutation 'p' avoids 312.
    -}
    is312Avoiding :: Perm.Perm a -> Bool
    is312Avoiding = is132Avoiding . Perm.Operation.complement

    {-|
      'is321Avoiding p' returns True if an only if permutation 'p' avoids 321.
    -}
    is321Avoiding :: Perm.Perm a -> Bool
    is321Avoiding _ = True
