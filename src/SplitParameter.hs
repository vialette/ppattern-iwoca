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

import qualified Data.List   as List
import qualified Data.Monoid as Monoid
import System.Console.CmdArgs
import System.Random

import qualified Data.Algorithm.PPattern.Perm.Monotone as Perm.Monotone
import qualified Data.Algorithm.PPattern.Perm.Split    as Perm.Split

data Options = Options { size           :: Int
                       , seed           :: Int
                       } deriving (Data, Typeable)

options :: Options
options = Options { size           = def &= help "The permutation size"
                  , seed           = def &= help "The seed of the random generator"
                  }
                  &= verbosity
                  &= summary "split-parameter v0.1.0.0, (C) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-1017"
                  &= program "split-parameter"

-- Estimate distribution
splitParamter :: RandomGen g => Int -> g -> [Int]
splitParamter n g = Perm.Monotone.longestDecreasingLength p
  where
    (p, _) = Perm.Rand.rand' n g

go :: RandomGen g => Int -> g -> IO ()
go n g = putStr $ show k `Monoid.mappend`
                  ",\""  `Monoid.mappend`
                  show k `Monoid.mappend`
                  "\n"
  where
    k = splitParamter n g

main :: IO ()
main = do
  opts <- cmdArgs options
  go (size opts) $ mkStdGen (seed opts)
