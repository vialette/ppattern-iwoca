{-|
Module      : Data.Algorithm.PPattern.State.Embedding
Structription : Short Structription
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-2017
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Mapping from color points to color points.
-}

module Data.Algorithm.PPattern.State.Embedding
(
  -- * The @Next@ type
  Embedding

  -- * Exporting
, toList

  -- * Constructing
, empty

  -- * Querying
, query

  -- * Modifying
, insert

  -- * Displaying
, showEmbedding
)
where

  import qualified Data.List       as List
  import qualified Data.Tuple      as Tuple
  import qualified Data.Monoid     as Monoid
  import qualified Data.Map.Strict as Map
  import qualified Data.Function   as Function

  import qualified Data.Algorithm.PPattern.Geometry.ColorPoint as ColorPoint

  -- Mapping from colored points to colored points.
  newtype Embedding = Embedding { getMap :: Map.Map ColorPoint.ColorPoint ColorPoint.ColorPoint }

  -- Make an embty embedding.
  empty :: Embedding
  empty = Embedding Map.empty

  -- Transform an embedding to a list of correcponding colored points.
  toList :: Embedding -> [(ColorPoint.ColorPoint, ColorPoint.ColorPoint)]
  toList = List.sortBy (compare `Function.on` Tuple.fst) . Map.toList . getMap

  -- Insert a pair of corresponding colored points into an embedding.
  insert :: ColorPoint.ColorPoint -> ColorPoint.ColorPoint -> Embedding -> Embedding
  insert cp cp' e = Embedding { getMap = m' }
    where
      m  = getMap e
      m' = Map.insert cp cp' m

  -- Get the mapping of a given colored point.
  query :: ColorPoint.ColorPoint -> Embedding -> Maybe ColorPoint.ColorPoint
  query cp = Map.lookup cp . getMap

  -- Show the embedding.
  showEmbedding :: Embedding -> String
  showEmbedding = aux "" . List.sort . toList
    where
      aux acc []                  = acc
      aux acc ((cp, cp') : cpcps) = aux (acc `Monoid.mappend` str) cpcps
        where
          str = show cp  `Monoid.mappend`
                " -> "   `Monoid.mappend`
                show cp' `Monoid.mappend`
                "\n"
