{-|
Module      : Data.Algorithm.PPattern.Search
Description : Short description
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-1017
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.Search
(
  -- * Searching
  search
)
where

  import qualified Data.List as List
  import qualified Data.Foldable as Foldable
  import qualified Data.IntMap.Strict as IntMap

  import qualified Data.Algorithm.PPattern.Combinatorics       as Combinatorics
  import qualified Data.Algorithm.PPattern.Perm                as Perm
  import qualified Data.Algorithm.PPattern.Color               as Color
  import qualified Data.Algorithm.PPattern.Geometry.ColorPoint as ColorPoint
  import qualified Data.Algorithm.PPattern.Strategy            as Strategy
  import qualified Data.Algorithm.PPattern.State               as State
  import qualified Data.Algorithm.PPattern.Context             as Context
  import qualified Data.Algorithm.PPattern.Conflict            as Conflict

  -- Make an initial list of colored points. Each element from the longest
  -- decreasing subsequence is given a distinct colors. All other elements
  -- are given the 'not determined yet' color 0.
  mkColorPoints :: [(Int, Int)] -> [Int] -> [Color.Color] -> [ColorPoint.ColorPoint]
  mkColorPoints []             _  _         = []
  mkColorPoints ((i, y) : iys) [] refColors = cp : mkColorPoints iys [] refColors
    where
      cp = ColorPoint.mk i y 0
  mkColorPoints ((_, _) : _)   _  []        =
    error "mkColorPoints. We shouldn't be there" -- make ghc -Werror happy
  mkColorPoints ((i, y) : iys) ds'@(d : ds) refColors'@(c : refColors)
    | y == d    = ColorPoint.mk i y c : mkColorPoints iys ds  refColors
    | otherwise = ColorPoint.mk i y Color.blankColor : mkColorPoints iys ds' refColors'

  -- Render the embedding as a list of pairs of color points.
  showEmbedding :: State.State -> Maybe [(ColorPoint.ColorPoint, ColorPoint.ColorPoint)]
  showEmbedding = Just . State.toList

  -- Search an order-isomorphic occurrence of permutation p into permutation q.
  -- Resolve conflicts according to a given strategy.
  search :: Perm.Perm -> Perm.Perm -> Strategy.Strategy -> [(ColorPoint.ColorPoint, ColorPoint.ColorPoint)]
  search p q strategy = searchAux p q strategy >>= presentEmbedding

  searchAux :: Perm.Perm -> Perm.Perm -> Strategy.Strategy -> Maybe State.State
  searchAux p q strategy
    | m > n     = Nothing
    | l > k     = Nothing
    | otherwise = res
    where
      -- compare by size
      m = Perm.size p
      n = Perm.size q

      -- comapre by longest decreasing subsequence length
      decreasing = Perm.longestDecreasing p
      l = Foldable.length decreasing
      k = Perm.longestDecreasingLength q

      -- make initial state
      s = State.mk q
      cs = Color.palette 1 k
      pIndexed = Perm.index p

      -- Embed p and perform search
      res = Foldable.asum [doSearch pcps cs context strategy s
                            | refColors   <- cs `Combinatorics.choose` l
                            , let pcps    = mkColorPoints pIndexed decreasing refColors
                            , let precede = IntMap.empty
                            , let follow  = IntMap.fromList $ List.zip refColors decreasing
                            , let context = Context.mk precede follow
                          ]

  doSearch ::
    [ColorPoint.ColorPoint] -> [Color.Color] -> Context.Context -> Strategy.Strategy -> State.State ->
      Maybe State.State
  doSearch []             _  _       _        s  = Just s
  doSearch pcps@(pcp : _) cs context strategy s
    | ColorPoint.color pcp == 0 = doSearchFreePoint pcps cs context strategy s
    | otherwise             = doSearchFixedColorPoint pcps cs context strategy s

  -- pcp is a 0-color point.
  doSearchFreePoint ::
    [ColorPoint.ColorPoint] -> [Color.Color] -> Context.Context -> Strategy.Strategy -> State.State ->
      Maybe State.State
  doSearchFreePoint []           _  _       _        _ =
    error "doSearchFreePoint. We shouldn't be there" -- make ghc -Werror happy
  doSearchFreePoint (pcp : pcps) cs context strategy s =
    Foldable.asum [State.pAppend (ColorPoint.updateColor c pcp) s >>= -- append new point
                   resolveConflicts strategy                      >>= -- resolve for match
                   doSearch pcps cs context' strategy
                     | c <- cs
                     , Context.agree c y context
                     , let context' = Context.update c y context
                  ]
    where
      y = ColorPoint.yCoord pcp

  -- pcp is not a 0-color point.
  doSearchFixedColorPoint ::
    [ColorPoint.ColorPoint] -> [Color.Color] -> Context.Context -> Strategy.Strategy -> State.State ->
      Maybe State.State
  doSearchFixedColorPoint []           _  _                  _        _ =
    error "doSearchFixedColorPoint. We shouldn't be there" -- make ghc -Werror happy
  doSearchFixedColorPoint (pcp : pcps) cs context strategy s =
      State.pAppend pcp s       >>= -- append new point
      resolveConflicts strategy >>= -- resolve for match
      doSearch pcps cs context' strategy
    where
      y = ColorPoint.yCoord pcp
      c = ColorPoint.color  pcp
      context' = Context.update c y context

  resolveConflicts :: Strategy.Strategy -> State.State -> Maybe State.State
  resolveConflicts strategy s =
    case strategy s of
      Nothing                             -> Just s
      Just (Conflict.OrderConflict pcp t) -> State.xResolve pcp t s >>=
                                             resolveConflicts strategy
      Just (Conflict.ValueConflict pcp t) -> State.yResolve pcp t s >>=
                                             resolveConflicts strategy
