{-|
Module      : Data.Algorithm.PPattern.Strategy.Link2
Description : Link2 strategy element
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-2017
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental
-}

module Data.Algorithm.PPattern.Search.ConflictSelection.Link2
(
  -- * The @Link2@ type
  Link2(..)

  -- * Constructing
, mk
)
where

  import qualified Data.Algorithm.PPattern.Search.ConflictSelection.Link as Strategy.Link

  -- pair of links
  newtype Link2 = Link2 (Strategy.Link.Link, Strategy.Link.Link)
                  deriving (Show, Eq, Ord)

  -- Construct a simple Link2.
  mk :: Strategy.Link.Link -> Strategy.Link.Link -> Link2
  mk link1 link2 = Link2 (link1, link2)
