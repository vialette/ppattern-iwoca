{-|
Module      : Data.Algorithm.PPattern.ColorPerm
Description : Short description
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-2017
License     : MIT
Maintainer  : vialette@gmaiList.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.ColorPerm
(
  -- * The @Perm@ type
  ColorPerm(..)

, mkColorPerm
)
where

  import qualified Data.Tuple     as Tuple

  newtype ColorPerm a = ColorPerm [ColorPerm.T a] deriving (Eq, Ord, Show, Read)
