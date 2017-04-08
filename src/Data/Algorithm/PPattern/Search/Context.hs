{-|
Module      : Data.Algorithm.PPattern.Search.Context
Structription : Context for constructing increasing colorings
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-1017
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer Structription of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.Search.Context
(
  -- * The @Resolve@ type
  Context(..)

  -- * Constructing
, mk

  -- * Querying
, agree
, allowedColor

  -- * Updating
, update
)
where

  import qualified Data.List          as List
  import qualified Data.IntMap.Strict as IntMap

  import qualified Data.Algorithm.PPattern.Color               as Color

  -- Context type for constructing increasing colorings of permutation p.
  data Context = Context  { precede                 :: IntMap.IntMap Int
                          , follow                  :: IntMap.IntMap Int
                          , rightLongestDecreasings :: IntMap.IntMap Int
                          }

  mk :: IntMap.IntMap Int -> IntMap.IntMap Int -> IntMap.IntMap Int -> Context
  mk precedeMap followMap rightLongestDecreasingsMap =
    Context { precede = precedeMap
            , follow   = followMap
            , rightLongestDecreasings = rightLongestDecreasingsMap
            }

  allowedColor :: Color.Color -> Int -> [Color.Color] -> Context -> Bool
  allowedColor c y cs context =
    case IntMap.lookup y m of
      Nothing -> error "allowedColor. Point should be inside the map"
      Just i  -> i >= k
    where
      m = rightLongestDecreasings context
      k = List.length $ List.filter (> c) cs

  agree :: Color.Color -> Int -> Context -> Bool
  agree c y context = agreeForPrecede && agreeForFollow
    where
      agreeForPrecede = agreePrecede c y (precede context)
      agreeForFollow  = agreeFollow  c y (follow  context)

  agreePrecede :: Color.Color -> Int -> IntMap.IntMap Int -> Bool
  agreePrecede c y m =
    case IntMap.lookup c m of
      Nothing -> True
      Just y' -> y' < y

  agreeFollow :: Color.Color -> Int -> IntMap.IntMap Int -> Bool
  agreeFollow c y m =
    case IntMap.lookup c m of
      Nothing -> True
      Just y' -> y < y'

  update :: Color.Color -> Int -> Context -> Context
  update c y context = mk precede' follow' rightLongestDecreasings'
    where
      precede' = updatePrecede c y (precede context)
      follow'  = updateFollow  c y (follow  context)
      rightLongestDecreasings' = rightLongestDecreasings context

  updatePrecede :: Color.Color -> Int-> IntMap.IntMap Int -> IntMap.IntMap Int
  updatePrecede = aux (1 :: Color.Color)
    where
      aux c refC y m
        | c > refC  = m
        | otherwise = aux (c+1) refC y m'
          where
            m' = case IntMap.lookup c m of
                   Nothing  -> IntMap.insert c y          m
                   Just y'  -> IntMap.insert c (max y y') m

  updateFollow :: Color.Color -> Int -> IntMap.IntMap Int -> IntMap.IntMap Int
  updateFollow c y m =
    case IntMap.lookup c m of
      Nothing       -> m
      Just y'
        | y < y'    -> m
        | y == y'   -> IntMap.delete c m
        | otherwise -> error "updateFollow. We shouldn't be there" -- make ghc -Werror happy
