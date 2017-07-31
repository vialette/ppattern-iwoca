{-|
Module      : ExactDistribution
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

import qualified Data.List          as List
import qualified Data.Foldable      as Foldable
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Monoid        as Monoid
import System.Console.CmdArgs

import qualified Data.Algorithm.PPattern.Perm           as Perm
import qualified Data.Algorithm.PPattern.Perm.Monotone  as Perm.Monotone
import qualified Data.Algorithm.PPattern.Perm.Enumerate as Perm.Enumerate

data Options = Options { size   :: Int
                       } deriving (Data, Typeable)

options :: Options
options = Options { size = def &= help "The permutation size"
                  }
                  &= verbosity
                  &= summary "exact-distribution v0.1.0.0, (C) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-1017"
                  &= program "exact-distribution"

-- Exact distribution
exactDistribution :: Int -> IntMap.IntMap [Int]
exactDistribution = exactDistributionAux IntMap.empty . Perm.Enumerate.perms

-- exactDistributionAux auxiliary function
exactDistributionAux :: IntMap.IntMap Int -> [Perm.Perm] -> IntMap.IntMap [Int]
exactDistributionAux m []       = exactDistributionAux' [] IntMap.empty . List.sort $ IntMap.toList m
exactDistributionAux m (p : ps) = case IntMap.lookup k m of
                                    Nothing -> exactDistributionAux (IntMap.insert k 1 m)     ps
                                    Just  n -> exactDistributionAux (IntMap.insert k (n+1) m) ps
  where
    k  = Perm.Monotone.longestDecreasingLength p

-- exactDistributionAux final function
exactDistributionAux' :: [Int] -> IntMap.IntMap [Int] -> [(Int, Int)] -> IntMap.IntMap [Int]
exactDistributionAux' _   m []             = m
exactDistributionAux' acc m ((i, n) : ins) = exactDistributionAux' acc' m' ins
  where
    acc' = acc `Monoid.mappend` List.replicate n i
    m'   = IntMap.insert i acc' m

f :: (Int, [Int]) -> String
f (i, xs) = show i  `Monoid.mappend`
            ",\""   `Monoid.mappend`
            xs'     `Monoid.mappend`
            "\"\n"
  where
    xs' = List.intercalate "," . fmap show $ xs

go :: Int -> IO ()
go n = do
  putStr . Foldable.concat . fmap f . List.sort . IntMap.toList $ exactDistribution n

main :: IO ()
main = do
  opts <- cmdArgs options
  go (size opts)
