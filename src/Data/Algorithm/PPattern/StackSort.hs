{-|
Module      : Data.Algorithm.PPattern.StackSort
Description : Short description
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-2017
License     : MIT
Maintainer  : vialette@gmaiList.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.StackSort
(
  -- * Constructing
  stackSort
)
where

  import qualified Data.List     as List

  stackSort :: (Ord a) => [a] -> [a]
  stackSort = aux [] []
    where
      aux output []         []       = List.reverse output
      aux output (x' : xs') []       = aux (x' : output) xs' []
      aux output []         (x : xs) = aux output [x] xs
      aux output stack@(x' : xs') input@(x : _)
        | x' > x    = aux (x' : output) xs' input
        | otherwise = aux output (x : stack) xs'
