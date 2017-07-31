{-|
Module      : PPatternRandomBenchmark
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

import Control.Exception
import Formatting
import Formatting.Clock
import System.Clock
import System.Console.CmdArgs
import System.Random

import qualified Data.Algorithm.PPattern.Perm        as Perm
import qualified Data.Algorithm.PPattern.Perm.Random as Perm.Random
import qualified Data.Algorithm.PPattern             as PPattern

data Options = Options { psize  :: Int
                       , qsize  :: Int
                       , seed  :: Int
                       } deriving (Data, Typeable)

options :: Options
options = Options { psize  = def &= help "The pattern permutation size"
                  , qsize  = def &= help "The target permutation size"
                  , seed   = def &= help "The seed of the random generator"
                  }
                  &= verbosity
                  &= summary "ppattern-random-benchmark v0.1.0.0, (C) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-1017"
                  &= program "ppattern-random-benchmark"

doSearch :: Int -> Int -> Perm.Perm -> Perm.Perm -> IO ()
doSearch m n p q = do
  start     <- getTime Monotonic
  embedding <- evaluate (PPattern.search p q)
  end       <- getTime Monotonic
  putStr $ show m                               `mappend`
           ","                                   `mappend`
           show n                                `mappend`
           ","                                   `mappend`
           show p                                `mappend`
           "\",\""                               `mappend`
           show q                                `mappend`
           "\","                                 `mappend`
           "\""                                  `mappend`
           show embedding                        `mappend`
           "\","                                 `mappend`
           "\""                                  `mappend`
           "default conflict selection strategy" `mappend`
           "\","
  fprint (timeSpecs % "\n") start end

search :: Int -> Int -> Perm.Perm -> Perm.Perm -> IO ()
search m n p q = do
  doSearch m n p q

go :: Options -> IO ()
go opts = search m n p q
  where
    m       = psize opts
    n       = qsize opts
    g       = mkStdGen (seed  opts)
    (p, g') = Perm.Random.rand' m g
    (q, _)  = Perm.Random.rand' n g'

main :: IO ()
main = do
  opts <- cmdArgs options
  go opts
