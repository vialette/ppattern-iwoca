{-|
Module      : Data.Algorithm.PPattern.IntPartition
Description : Short description
Copyright   : (c) Stéphane Vialette, 2016
License     : MIT
Maintainer  : vialette@gmaiList.com
Stability   : experimental

Integer partitions.
-}

module Data.Algorithm.PPattern.IntPartition
(
  -- * The @IntPartition@ type
  IntPartition(..)
, fromList
, mkIntPartition

  -- * Querying
, nbParts

  -- * Transforming
, toList

  -- * Generating and counting
, partitions
, nbIntPartitions
, partitionsL
, nbpartitionsL

  -- * Random
, randIntPartition
)
where

  import qualified System.Random
  import qualified Data.List  as List
  import qualified Data.Tuple as Tuple

  -- | Partition of integer.
  newtype IntPartition = IntPartition [Int]
                         deriving (Show, Eq, Ord)

  {-|

  -}
  fromList :: [Int] -> IntPartition
  fromList = IntPartition

  {-|

  -}
  mkIntPartition :: [Int] -> IntPartition
  mkIntPartition = fromList

  empty :: IntPartition
  empty = fromList []

  {-|

  -}
  nbParts :: IntPartition -> Int
  nbParts (IntPartition xs) = List.length xs

  {-|

  -}
  toList :: IntPartition -> [Int]
  toList (IntPartition xs) = xs

  {-|
    'intPartitions n ' returns all ordered partitions of integer 'n'.

    λ: intPartitions 6
    [[6],[3,3],[4,2],[5,1],[2,2,2],[3,2,1],[4,1,1],[2,2,1,1],[3,1,1,1],[2,1,1,1,1],[1,1,1,1,1,1]]
  -}
  partitions :: Int -> [IntPartition]
  partitions n = List.concat [partitionsL n k | k <- [1..n]]

  {-|
    'nbIntPartitions n' returns the number of ordered partitions of integer 'n'.
  -}
  nbIntPartitions :: Int -> Int
  nbIntPartitions = List.length . partitions

  {-|
    'partitionsL n k' returns all ordered partitions of integer 'n' into 'k'
    parts.

    λ: partitionsL 6 0
    [[]]
    λ: partitionsL 6 1
    [[6]]
    λ: partitionsL 6 2
    [[3,3],[4,2],[5,1]]
    λ: partitionsL 6 3
    [[2,2,2],[3,2,1],[4,1,1]]
    λ: partitionsL 6 4
    [[2,2,1,1],[3,1,1,1]]
    λ: partitionsL 6 5
    [[2,1,1,1,1]]
    λ: partitionsL 6 6
    [[1,1,1,1,1,1]]
    λ: partitionsL 6 7
    []
  -}
  partitionsL :: Int -> Int -> [IntPartition]
  partitionsL n k = fromList <$> partitionsLAux n k n

  partitionsLAux :: Int -> Int -> Int -> [[Int]]
  partitionsLAux _ 0 _ = [[]]
  partitionsLAux n 1 _ = [[n]]
  partitionsLAux n k b
    | n < k     = []
    | n == k    = [List.replicate k 1]
    | otherwise = List.concat [fmap (k':) (partitionsLAux (n-k') (k-1) k') |
                            k' <- [l..h]]
    where
      l = fromIntegral (ceiling ((fromIntegral n / fromIntegral k) :: Double) :: Int)
      h = min (n-k+1) b

  {-|
    'nbpartitionsL n k' returns the number of ordered partitions of integer
    'n' into 'k' parts.
  -}
  nbpartitionsL :: Int -> Int -> Int
  nbpartitionsL n k = List.length $ partitionsL n k

  {-|
    'randIntPartition' takes two integers 'n' and 'k', and a generator 'g'.
    It returns a random 'k'-partition of '[1..n]', together with a new generator.
  -}
  randIntPartition :: (System.Random.RandomGen g) => Int -> Int -> g -> (IntPartition, g)
  randIntPartition n k g
    | k > n     = (empty, g)
    | otherwise = (fromList xs, g')
    where
      (xs, g') = randIntPartitionAux n k g

  -- randIntPartition auxiliary function
  randIntPartitionAux :: (System.Random.RandomGen g) => Int -> Int -> g -> ([Int], g)
  randIntPartitionAux = go []
    where
      go acc n' 1  g' = (n' : acc, g')
      go acc n' k' g' = go (x : acc) (n'-x) (k'-1) g''
        where
          lo       = 1
          hi       = n'-k'+1
          (x, g'') = System.Random.randomR (lo, hi) . Tuple.snd $ System.Random.randomR (lo, hi) g'
