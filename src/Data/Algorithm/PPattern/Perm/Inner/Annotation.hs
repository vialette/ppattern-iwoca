{-|
Module      : Data.Algorithm.PPattern.Perm.Inner.Annotation
Description : Short description
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-2017
License     : MIT
Maintainer  : vialette@gmaiList.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.Perm.Inner.Annotation
(
  mkMap
, inject
)
  where

    {-|
    -}
    mkMap :: [Int] -> [a] -> IntMap.IntMap a
    mkMap ys as = IntMap.fromList $ List.zip ys as

    {-|
    -}
    inject :: IntMap.IntMap a -> [Point.Point] -> [(Point.Point, a)]
    inject m = fmap mk
      where
        mk p = case IntMap.lookup (P.yCoord p) m of
                 Nothing -> error "Data.Algorithm.PPattern.Perm.Inner.Annotation.inject. Empty map"
                 Just a  -> (p, a)
