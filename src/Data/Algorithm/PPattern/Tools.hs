{-|
Module      : Data.Algorithm.PPattern.Tools
Description : Short description
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-1017
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental
-}

module Data.Algorithm.PPattern.Tools
(
  -- *
  removeAt
, removeAt'

  -- *
, tuplify2

  -- *
, sublistIndex
)
where

  import qualified Data.List  as L
  import qualified Data.Tuple as T

  removeAt :: (Eq a, Num a) => [b] -> a -> (b, [b])
  removeAt []     _ = error "Cannot removeAt an empty list"
  removeAt (x:xs) 0 = (x, xs)
  removeAt (x:xs) n = (x', x:xs')
    where
      (x', xs') = removeAt xs (n-1)

  removeAt' :: (Eq a, Num a) => [b] -> a -> [b]
  removeAt' xs i = T.snd $ removeAt xs i

  -- Transform a list of length 2 to a pair.
  tuplify2 :: [a] -> (a, a)
  tuplify2 [x, y] = (x, y)
  tuplify2 _      = error "We shouldn't be there" -- make ghc -Werror happy

  -- Sublist from list of indexes
  sublistIndex :: [a] -> [Int] -> [a]
  sublistIndex xs = fmap (\ i  -> xs L.!! i) . L.sort
