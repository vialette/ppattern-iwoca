{-|
Module      : Data.Algorithm.PPattern.APerm.Inner.Sub
Description : Short description
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-2017
License     : MIT
Maintainer  : vialette@gmaiList.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.Perm.Inner.Sub
(
  -- *
  sub
)
  where

    -- import qualified Data.List          as List
    -- import qualified Data.Tuple         as Tuple
    -- import qualified Data.Foldable      as Foldable
    -- import qualified Data.IntMap.Strict as IntMap

    import qualified Data.Algorithm.PPattern.Geometry.Point as Point

    --
    sub :: Int -> Int -> [Point.Point] -> [Point.Point]
    sub xMin xMax = filter f
      where
        f p = x >= xMin && x <= xMax
          where
            x = Point.xCoord p
