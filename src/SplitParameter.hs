{-|
Module      : SplitParameter
Description : Short description
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-1017
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Foldable as Foldable
import System.Console.CmdArgs
import System.Random

import qualified Data.Algorithm.PPattern.Perm.Monotone as Perm.Monotone
import qualified Data.Algorithm.PPattern.Perm.Random   as Perm.Random

data Options = Options { size   :: Int
                       , trials :: Int
                       , seed   :: Int
                       } deriving (Data, Typeable)

options :: Options
options = Options { size   = def &= help "The permutation size"
                  , trials = def &= help "The number of trials"
                  , seed   = def &= help "The seed of the random generator"
                  }
                  &= verbosity
                  &= summary "split-parameter v0.1.0.0, (C) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-1017"
                  &= program "split-parameter"

-- Estimate distribution
splitParamter :: RandomGen g => Int -> Int -> g -> [Int]
splitParamter n t = aux [] 1
  where
    aux acc i g
      | i > t     = acc
      | otherwise = aux (k : acc) (i+1) g'
      where
        (p, g') = Perm.Random.rand' n g
        k       = Perm.Monotone.longestDecreasingLength p

go :: RandomGen g => Int -> Int -> g -> IO ()
go n t g = Foldable.mapM_ putStr $ fmap (\k -> show n ++ "," ++ show k ++ "\n") ks
  where
    ks = splitParamter n t g

main :: IO ()
main = do
  opts <- cmdArgs options
  go (size opts) (trials opts)$ mkStdGen (seed opts)
