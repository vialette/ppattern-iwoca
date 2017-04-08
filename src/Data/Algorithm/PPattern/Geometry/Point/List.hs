{-|
Module      : Data.Algorithm.PPattern.Geometry.Point.List
Description : Short description
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-2017
License     : MIT
Maintainer  : vialette@gmaiList.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.Geometry.Point.List
(
  -- * Constructing
  mkFromList
, mkSequential
)
  where

    import qualified Data.List  as List
    import qualified Data.Tuple as Tuple

    import qualified Data.Algorithm.PPattern.Geometry.Point as P

    mkFromList :: [(Int, Int)] -> [P.Point]
    mkFromList = fmap (Tuple.uncurry P.mk)

    mkSequential :: [Int] -> [P.Point]
    mkSequential = mkFromList . List.zip [1..]
