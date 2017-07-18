{-|
Module      : Data.Algorithm.PPattern.APerm.Inner.Operation
Description : Short description
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-2017
License     : MIT
Maintainer  : vialette@gmaiList.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.Perm.Inner.Operation
(
  reversal
, complement
, reversalComplement
, inverse
)
  where

    import qualified Data.List     as List
    -- import qualified Data.Tuple    as Tuple
    import qualified Data.Foldable as Foldable
    import qualified Data.Function as Function


    import qualified Data.Algorithm.PPattern.Geometry.Point      as Point
    import qualified Data.Algorithm.PPattern.Geometry.Point.List as Point.List

    -- Reverse a permutation given as its list of points
    reversal :: [Point.Point] -> [Point.Point]
    reversal ps = Foldable.foldl f [] ps
      where
        n = List.length ps
        f acc p = Point.updateXCoord x' p : acc
          where
            x  = Point.xCoord p
            x' = n + 1 - x

    -- Complement a permutation given as its list of points
    complement :: [Point.Point] -> [Point.Point]
    complement ps = List.sortBy (compare `Function.on` Point.xCoord) $ Foldable.foldl f [] ps
      where
        n = List.length ps
        f acc p = Point.updateYCoord y' p : acc
          where
            y  = Point.yCoord p
            y' = n + 1 - y

    -- Reverse complement a permutation given as its list of points

    reversalComplement :: [Point.Point] -> [Point.Point]
    reversalComplement = reversal . complement

    --  Inverse a permutation given as its list of points
    inverse :: [Point.Point] -> [Point.Point]
    inverse = Point.List.mkSequential . fmap Point.xCoord . List.sortBy (compare `Function.on` Point.yCoord)
