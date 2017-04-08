{-|
Module      : Data.Algorithm.PPattern.Combinatorics
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
    'choose xs k' returns all sublists of size 'k' of 'xs'.
  -}
  choose :: [a] -> Int -> [[a]]
  _      `choose` 0       = [[]]
  []     `choose` _       =  []
  (x:xs) `choose` k       =  (x:) `fmap` (xs `choose` (k-1)) ++ xs `choose` k
