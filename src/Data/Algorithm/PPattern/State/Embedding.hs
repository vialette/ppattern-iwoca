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
  import qualified Data.Monoid     as Monoid
  import qualified Data.Map.Strict as Map

  import qualified Data.Algorithm.PPattern.Geometry.ColorPoint as ColorPoint

  newtype Embedding = Embedding (Map.Map ColorPoint.ColorPoint ColorPoint.ColorPoint)

  empty :: Embedding
  empty = Embedding Map.empty

  toList :: Embedding -> [(ColorPoint.ColorPoint, ColorPoint.ColorPoint)]
  toList (Embedding m) = Map.toList m

  insert :: ColorPoint.ColorPoint -> ColorPoint.ColorPoint -> Embedding -> Embedding
  insert cp cp' (Embedding m) = Embedding m'
    where
      m' = Map.insert cp cp' m

  query :: ColorPoint.ColorPoint -> Embedding -> Maybe ColorPoint.ColorPoint
  query cp (Embedding m) = Map.lookup cp m

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
