{-|
Module      : Data.Algorithm.PPattern.Perm
Description : Short description
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-1017
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.Searchable
(
  -- * The @Perm@ type
  Search
)
where

-- Make an initial list of colored points. Each element from the longest
-- decreasing subsequence is given a distinct colors. All other elements
-- are given the 'not determined yet' color 0.
mkCPoints :: [(Int, Int)] -> [Int] -> [Color.Color] -> [CPoint.CPoint]
mkCPoints []             _  _         = []
mkCPoints ((i, y) : iys) [] refColors = cp : mkCPoints iys [] refColors
  where
    cp = CPoint.mkCPoint i y 0
mkCPoints ((_, _) : _)   _  []        =
  error "mkCPoints. We shouldn't be there" -- make ghc -Werror happy
mkCPoints ((i, y) : iys) ds'@(d : ds) refColors'@(c : refColors)
  | y == d    = CPoint.mkCPoint i y c : mkCPoints iys ds  refColors
  | otherwise = CPoint.mkCPoint i y Color.noColor : mkCPoints iys ds' refColors'

-- Render the embedding as a list of pairs of color points.
showEmbedding :: State.State -> Maybe [(ColorPoint.ColorPoint a b, ColorPoint.ColorPoint a b)]
showEmbedding = Just . State.embeddingToList

-- Search an order-isomorphic occurrence of permutation p into permutation q.
-- Resolve conflicts according to a given strategy.
search :: Perm.Perm -> Perm.Perm -> Strategy.Strategy -> [(ColorPoint.ColorPoint a b, ColorPoint.ColorPoint a b)]
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
    s = State.mkState q
    cs = Color.colors 1 k
    pIndexed = Perm.index p

    -- Embed p and perform search
    res = Foldable.asum [doSearch pcps cs context strategy s
                          | refColors   <- cs `Combi.choose` l
                          , let pcps    = mkCPoints pIndexed decreasing refColors
                          , let precede = IntMap.empty
                          , let follow  = IntMap.fromList $ L.zip refColors decreasing
                          , let context = Context.mk precede follow
                        ]

doSearch ::
  [CPoint.CPoint] -> [Color.Color] -> Context.Context -> Strategy.Strategy -> State.State ->
    Maybe State.State
doSearch []             _  _       _        s  = Just s
doSearch pcps@(pcp : _) cs context strategy s
  | CPoint.color pcp == 0 = doSearchFreePoint pcps cs context strategy s
  | otherwise             = doSearchFixedColorPoint pcps cs context strategy s

-- pcp is a 0-color point.
doSearchFreePoint ::
  [CPoint.CPoint] -> [Color.Color] -> Context.Context -> Strategy.Strategy -> State.State ->
    Maybe State.State
doSearchFreePoint []           _  _       _        _ =
  error "doSearchFreePoint. We shouldn't be there" -- make ghc -Werror happy
doSearchFreePoint (pcp : pcps) cs context strategy s =
  Foldable.asum [State.pAppend (CPoint.updateColor c pcp) s >>= -- append new point
                 resolveConflicts strategy                  >>= -- resolve for match
                 doSearch pcps cs context' strategy
                   | c <- cs
                   , Context.agree c y context
                   , let context' = Context.update c y context
                ]
  where
    y = CPoint.yCoord pcp

-- pcp is not a 0-color point.
doSearchFixedColorPoint ::
  [CPoint.CPoint] -> [Color.Color] -> Context.Context -> Strategy.Strategy -> State.State ->
    Maybe State.State
doSearchFixedColorPoint []           _  _                  _        _ =
  error "doSearchFixedColorPoint. We shouldn't be there" -- make ghc -Werror happy
doSearchFixedColorPoint (pcp : pcps) cs context strategy s =
    State.pAppend pcp s       >>= -- append new point
    resolveConflicts strategy >>= -- resolve for match
    doSearch pcps cs context' strategy
  where
    y = CPoint.yCoord pcp
    c = CPoint.color  pcp
    context' = Context.update c y context

resolveConflicts :: Strategy.Strategy -> State.State -> Maybe State.State
resolveConflicts strategy s =
  case strategy s of
    Nothing                             -> Just s
    Just (Conflict.OrderConflict pcp t) -> State.xResolve pcp t s >>=
                                           resolveConflicts strategy
    Just (Conflict.ValueConflict pcp t) -> State.yResolve pcp t s >>=
                                           resolveConflicts strategy
