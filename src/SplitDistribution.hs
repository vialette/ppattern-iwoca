{-|
Module      : SplitDistribution
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

import qualified Data.List   as List
import qualified Data.Monoid as Monoid
import System.Console.CmdArgs
import System.Random

import qualified Data.Algorithm.PPattern.Perm.Monotone as Perm.Monotone
import qualified Data.Algorithm.PPattern.Perm.Split    as Perm.Split

data Options = Options { size           :: Int
                       , splitParameter :: Int
                       , trials         :: Int
                       , seed           :: Int
                       } deriving (Data, Typeable)

options :: Options
options = Options { size           = def &= help "The permutation size"
                  , splitParameter = def &= help "The split parameter"
                  , trials         = def &= help "The number of trials"
                  , seed           = def &= help "The seed of the random generator"
                  }
                  &= verbosity
                  &= summary "split-distribution v0.1.0.0, (C) Stéphane Vialette 2017"
                  &= program "split-distribution"

-- Estimate distribution
distribution :: RandomGen g => Int -> Int -> Int -> g -> [Int]
distribution n k t = aux [] 1
  where
    aux acc i g
      | i > t = acc
      | otherwise = aux (k' : acc) (i+1) g'
      where
        (p, g') = Perm.Split.rand n k g
        k'      = Perm.Monotone.longestDecreasingLength p

go :: RandomGen g => Int -> Int -> Int -> g -> IO ()
go n k t g = putStr $ show k `Monoid.mappend`
                      ",\""  `Monoid.mappend`
                      ks     `Monoid.mappend`
                      "\n"
  where
    ks = List.intercalate "," . fmap show $ distribution n k t g

main :: IO ()
main = do
  opts <- cmdArgs options
  go (size opts) (splitParameter opts) (trials opts) $ mkStdGen (seed opts)
