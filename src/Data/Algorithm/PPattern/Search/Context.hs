{-|
Module      : Data.Algorithm.PPattern.Search.Context
Structription : Context for constructing increasing colorings
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-1017
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Pattern matching context.
-}

module Data.Algorithm.PPattern.Search.Context
(
  -- * The @Context@ type
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

  -- Context type for constructing increasing colorings of a permutation.
  -- left y m
  -- right y m
  data Context = Context  { left      :: IntMap.IntMap Int
                          , right     :: IntMap.IntMap Int
                          , rightDecs :: IntMap.IntMap Int
                          }

  -- Make a new context from a left map, a right map and a rightDecs
  -- map.
  mk :: IntMap.IntMap Int -> IntMap.IntMap Int -> IntMap.IntMap Int -> Context
  mk leftMap rightMap rightDecsMap =
    Context { left      = leftMap
            , right     = rightMap
            , rightDecs = rightDecsMap
            }

  -- Returns True iff if color c is allowed for the entry y.
  -- The color is not allowed if there are not enough colors for coloring
  -- the right decresings entry on the right part of entry y.
  allowedColor :: Color.Color -> Int -> [Color.Color] -> Context -> Bool
  allowedColor c y cs context =
    case IntMap.lookup y m of
      Nothing -> error "Context.allowedColor. Point should be inside the map"
      -- Just l  -> l >= k
      Just l  -> k >= l
    where
      m = rightDecs context
      k = List.length $ List.filter (> c) cs

  -- Return True iff color c for entry y agrees with the current (left and right)
  -- context.
  agree :: Color.Color -> Int -> Context -> Bool
  agree c y context = testLeft && testRight
    where
      testLeft = agreeLeft c y (left context)
      testRight  = agreeRight  c y (right  context)

  -- Return True iff color c for entry y agrees with the current left context,
  -- i.e., the preceding entry y' with color c is smaller.
  agreeLeft :: Color.Color -> Int -> IntMap.IntMap Int -> Bool
  agreeLeft c y m =
    case IntMap.lookup c m of
      Nothing -> True    -- first entry with color c
      Just y' -> y' < y  -- the preceding entry has to be smaller

  -- Return True iff color c for entry y agrees with the current right context,
  -- i.e., there does not exist a right entry y' with color c which is strictly
  -- smaller.
  agreeRight :: Color.Color -> Int -> IntMap.IntMap Int -> Bool
  agreeRight c y m =
    case IntMap.lookup c m of
      Nothing -> True
      Just y' -> y < y'

  -- Update the context.
  -- rightDecs is left unmodified.
  update :: Color.Color -> Int -> Context -> Context
  update c y context = context { left = left', right = right' }
    where
      left'  = updateLeft  c y (left  context)
      right' = updateRight c y (right context)

  -- Update the left part of the context.
  updateLeft :: Color.Color -> Int-> IntMap.IntMap Int -> IntMap.IntMap Int
  updateLeft = aux (1 :: Color.Color)
    where
      aux c refC y m
        | c > refC  = m
        | otherwise = aux (c+1) refC y m'
          where
            m' = case IntMap.lookup c m of
                   Nothing  -> IntMap.insert c y          m
                   Just y'  -> IntMap.insert c (max y y') m

  -- Update the right part of the context.
  -- There is no update, the constraint is just possibly deleted.
  updateRight :: Color.Color -> Int -> IntMap.IntMap Int -> IntMap.IntMap Int
  updateRight c y m =
    case IntMap.lookup c m of
      Nothing       -> m
      Just y'
        | y < y'    -> m
        | y == y'   -> IntMap.delete c m
        | otherwise -> error "Context.updateRight. We shouldn't be there" -- make ghc -Werror happy
