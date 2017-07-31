{-|
Module      : Data.Algorithm.PPattern.Perm.Split
Description : Short description
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-2017
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.Perm.Split
(
  -- *
  rand
)
  where

    import qualified Data.List     as List
    import qualified Data.Foldable as Foldable
    import qualified System.Random

    import qualified Data.Algorithm.PPattern.Perm         as Perm
    import qualified Data.Algorithm.PPattern.IntPartition as IntPartition
    import qualified Data.Algorithm.PPattern.Random       as Random

    {-|
      'rand' takes two integers 'n' and 'k' and a random generator 'g'.
      It returns a random (non-uniform) permutation of length 'n' that is
      the union of 'k' increasings sequences, together with a new random
      generator.
    -}
    rand :: System.Random.RandomGen g => Int -> Int -> g -> (Perm.Perm, g)
    rand n k g
      | k > n     = (Perm.empty, g)
      | otherwise = (p, g''')
      where
        -- rand int partition
        (intPartition, g') = IntPartition.randIntPartition n k g
        partitionAsList = IntPartition.toList intPartition
        (partitionAsIncreasingLists, g'') = mkSplits partitionAsList g'

        -- random shuffle
        (xs, g''') = Random.randShuffle partitionAsIncreasingLists g''

        -- make permutation
        p = Perm.mk xs

    -- 'mkSplits xs' constructs increasing lists, where the length of each
    -- list is given by the elements of 'xs'.
    mkSplits :: System.Random.RandomGen g => [Int] -> g -> ([[Int]], g)
    mkSplits ls = mkSplitsAux [] [1..n] ls
      where
        n = Foldable.sum ls

    -- mkSplits auxiliary function.
    mkSplitsAux :: System.Random.RandomGen g =>
      [[Int]] -> [Int] -> [Int] -> g -> ([[Int]], g)
    mkSplitsAux acc _  []       g = (acc, g)
    mkSplitsAux acc xs (l : ls) g = mkSplitsAux (ys : acc) (xs List.\\ ys) ls g'
      where
        (ys, g') = Random.sample l xs g
