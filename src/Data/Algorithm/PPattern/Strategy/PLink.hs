{-|
Module      : Data.Algorithm.PPattern.Strategy.PLink
Description : PLink strategy element
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-2017
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental
-}

module Data.Algorithm.PPattern.Strategy.PLink
(
  -- * The @PLink@ type
  PLink(..)

  -- * Constructing
, mk
)
where

  import qualified Data.Algorithm.PPattern.Strategy.Link as Strategy.Link

  -- pair of links
  newtype PLink = PLink (Strategy.Link.Link, Strategy.Link.Link)
                  deriving (Show, Eq, Ord)

  -- Construct a simple PLink.
  mk :: Strategy.Link.Link -> Strategy.Link.Link -> PLink
  mk link1 link2 = PLink (link1, link2)
