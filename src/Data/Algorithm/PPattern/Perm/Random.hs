{-|
Module      : Data.Algorithm.PPattern.Perm.Random
Description : Short description
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-2017
License     : MIT
Maintainer  : vialette@gmaiList.com
Stability   : experimental

Random permutations
-}

module Data.Algorithm.PPattern.Perm.Random
(
  -- * generating
  rand,
  rand'
)
  where

  import qualified System.Random

  import qualified Data.Algorithm.PPattern.Perm   as Perm
  import qualified Data.Algorithm.PPattern.Random as Random

  {-|
    'rand' takes a permutation 'p' and a random generator 'g', and
    returns a random permutation of 'p', together with a new generatoRandom.
  -}
  rand :: System.Random.RandomGen g => Perm.Perm -> g -> (Perm.Perm, g)
  rand p g = (Perm.fromList xs, g')
    where
      (xs, g') = Random.randPerm (Perm.points p) g

  {-|
    'rand'' takes an integer 'n' and a random generator 'g', and
    returns a random permutation of '[1..n]', together with a new random
    generator.
  -}
  rand' :: System.Random.RandomGen g => Int -> g -> (Perm.Perm, g)
  rand' n = rand p
    where
      p = Perm.mk [1..n]
