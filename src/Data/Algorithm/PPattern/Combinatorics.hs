{-|
Module      : Data.Algorithm.PPattern.Combi
Description : Combinatorial tools
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-1017
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental
-}

module Data.Algorithm.PPattern.Combinatorics
(
  choose
)
where

  {-|
    The 'partitionsByLength' function returns all k-partitions of an integer.
  -}
  choose :: [a] -> Int -> [[a]]
  _      `choose` 0       = [[]]
  []     `choose` _       =  []
  (x:xs) `choose` k       =  (x:) `fmap` (xs `choose` (k-1)) ++ xs `choose` k
