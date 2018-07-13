{-|
Module      : Data.Algorithm.PPattern.Random
Description : Short description
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-2017
License     : MIT
Maintainer  : vialette@gmaiList.com
Stability   : experimental

Random shuffle and selection.
-}

module Data.Algorithm.PPattern.Random
(
  randChoose
, randSelect
, randPerm
, randShuffle

  --
, sample
)
where

  import qualified Data.List     as List
  import qualified System.Random as Random

  import qualified Data.Algorithm.PPattern.List          as Pattern.List
  import qualified Data.Algorithm.PPattern.Combinatorics as Combinatorics

  {-|
    'randChoose' takes a list 'xs', an integer 'k' and a generator 'g', and
    returns a random sublist of 'xs' of length 'k'), together with a new generatoRandom.
  -}
  randChoose :: Random.RandomGen g => [a] -> Int -> g -> ([a], g)
  randChoose xs k g = (xss List.!! (i-1), g')
    where
      xss = xs `Combinatorics.choose` k
      (i, g') = Random.randomR (1, List.length xss) g

  {-|
    'randSelect' takes a list 'xs', an integer 'k' and a generator 'g', and
    returns a random list 'xs' of length 'k' of random selected elements from
    'xs', together with a new generatoRandom.
  -}
  randSelect :: Random.RandomGen g => [a] -> Int -> g -> ([a], g)
  randSelect xs = randSelectAux xs []

  -- randSelect auxiliary function
  randSelectAux :: Random.RandomGen g => [a] -> [a] -> Int -> g -> ([a], g)
  randSelectAux xs acc k g
    | k == 0    = (acc, g)
    | otherwise = randSelectAux (Pattern.List.removeAt' xs i) (x:acc) (k-1) g'
    where
      n       = List.length xs
      (i, g') = Random.randomR (0, n-1) g
      x       = xs List.!! i

  {-|
    'randPerm' takes a list 'xs' and a generator 'g', and
    returns a random permutation of 'xs', together with a new generatoRandom.
  -}
  randPerm :: Random.RandomGen g => [a] -> g -> ([a], g)
  randPerm xs = randSelect xs (List.length xs)

  {-|
    'randShuffle' takes a list of lists 'xss' and a generator 'g', and
    returns a random shuffle of xss (i.e. each list of 'xss' is a sublist of the
    result), together with a new generatoRandom.
  -}
  randShuffle :: Random.RandomGen g => [[a]] -> g -> ([a], g)
  randShuffle xss = randShuffleAux xss []

  -- randShuffle auxiliary function.
  randShuffleAux :: Random.RandomGen g => [[a]] -> [a] -> g -> ([a], g)
  randShuffleAux []  acc g = (List.reverse acc, g)
  randShuffleAux xss acc g = randShuffleAux xss'' (x:acc) g'
    where
      (xss', g') = randPerm xss g
      (x, xss'') = randShuffleAux' xss'

  -- randShuffleAux auxiliary function.
  randShuffleAux' :: [[a]] -> (a, [[a]])
  randShuffleAux' []            = error "We shouldn't be there"
  randShuffleAux' ([]:_)        = error "We shouldn't be there"
  randShuffleAux' ([x]:xss)     = (x, xss)
  randShuffleAux' ((x:xs):xss') = (x, xs:xss')

  pick :: (Random.RandomGen g) => [a] -> g -> (a, g)
  pick xs g = (xs !! i, g')
    where
      (i, g') = Random.randomR (0, length xs - 1) g

  {-|
    'sample n xs g' takes a list 'xs', an integer 'n' and a random generator
    'g', and returns a random random sample of xs of size 'n', together with
    a new random generator.
  -}
  sample :: (Random.RandomGen g, Ord a) => Int -> [a] -> g -> ([a], g)
  sample n xs g = (List.sort ys, g')
    where
      (ys, g') = sampleAux n xs g

  -- sample auxiliary function.
  sampleAux :: (Random.RandomGen g, Ord a) => Int -> [a] -> g -> ([a], g)
  sampleAux 0 _  g = ([], g)
  sampleAux n xs g = (x : ys, g'')
    where
      (x, g')   = pick xs g
      (ys, g'') = sampleAux (n-1) (List.delete x xs) g'
