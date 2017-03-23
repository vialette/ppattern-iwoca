{-|
Module      : Data.Algorithm.PPattern.Color
Description : Short description
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.Color
(
  -- * The @Color@ type
  Color

  -- * Constructing
, noColor
, palette
)
where

  -- |The 'Color' type encapsulates a color.
  type Color = Int

  noColor :: Color
  noColor = 0 :: Color

  {-|
    'palette fromColor toColor' returns the list of all colors from 'fromColor'
    to 'toColor', inclusively.
  -}
  palette :: Int -> Int -> [Color]
  palette fromColor toColor
    | fromColor > toColor = []
    | otherwise           = [fromColor..toColor]
