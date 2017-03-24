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

  import qualified Data.Algorithm.PPattern.Search.Context      as Search.Context
  import qualified Data.Algorithm.PPattern.Combinatorics       as Combinatorics
  import qualified Data.Algorithm.PPattern.Perm                as Perm
  import qualified Data.Algorithm.PPattern.Color               as Color
  import qualified Data.Algorithm.PPattern.Geometry.ColorPoint as ColorPoint
  import qualified Data.Algorithm.PPattern.Strategy            as Strategy
  import qualified Data.Algorithm.PPattern.State               as State
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

  -- Extract embedding in case of a direct search
  present :: State.State -> Maybe [(ColorPoint.ColorPoint, ColorPoint.ColorPoint)]
  present = Just . State.toList

  -- Extract embedding in case of a reverse search
  presentReverse :: State.State -> Maybe [(ColorPoint.ColorPoint, ColorPoint.ColorPoint)]
  presentReverse pSize qSize  = Just . Foldable.foldr f [] . State.toList
    where
      f acc (cp1, cp2) = (cp1', cp2') : acc
        where
          cp1' = ColorPoint.updateXCoord (pSize + 1 - ColorPoint.xCoord cp1) cp1
          cp2' = ColorPoint.updateXCoord (qSize + 1 - ColorPoint.xCoord cp2) cp2

  -- Search for an order-isomorphic occurrence of permutation p into permutation q.
  -- Resolve conflicts according to a given strategy.
  search :: Perm.Perm -> Perm.Perm -> Strategy.Strategy -> Maybe [(ColorPoint.ColorPoint, ColorPoint.ColorPoint)]
  search p q strategy
    | pSize > qSize = Nothing
    | qLongestDecreasingLength < qLongestIncreasingLength =
        searchAux p q qLongestDecreasingLength strategy >>=
        present
    | otherwise =
        searchAux pReverse qReverse qReverseLongestDecreasingLength strategy >>=
        presentReverse pSize qSize
    where
      pSize = Perm.size p
      qSize = Perm.size q

      qLongestDecreasing = Perm.longestDecreasing q
      qLongestDecreasingLength = Perm.size qLongestDecreasing

      qLongestIncreasing = Perm.longestIncreasing q
      qLongestIncreasingLength = Perm.size qLongestIncreasing

      pReverse = Perm.reversal p
      qReverse = Perm.reversal q

      qReverseLongestDecreasing = Perm.longestDecreasing qReverse
      qReverseLongestDecreasingLength = Perm.size qReverseLongestDecreasing

  searchAux :: Perm.Perm a -> Perm.Perm a -> Int -> Strategy.Strategy -> Maybe State.State
  searchAux p q qLongestDecreasingLength strategy
    | pLongestDecreasingLength > qLongestDecreasingLength = Nothing
    | otherwise                                           = computation
    where

      -- comapre by longest decreasing subsequence length
      pLongestDecreasing = Perm.longestDecreasing p
      pLongestDecreasingLength = Foldable.length decreasing

      -- make initial state
      s = State.mk q
      cs = Color.palette 1 qLongestDecreasingLength

      -- Permutation p as blank points
      pPoints = Perm.points p
      pBlankPoints = fmap (\ p -> ColorPoint.mk p Color.blankColor) pPoints

      -- p longest decressing by suffixes
      pRightLongestDecreasing = RightLongestDecreasing.mk pPoints

      -- Embed p and perform search
      res = Foldable.asum [doSearch pcps pDecreasingPanorama cs context strategy s
                            | refColors   <- cs `Combinatorics.choose` l
                            , let pcps    = pInitialColorPoints p pLongestDecreasing refColors
                            , let precede = IntMap.empty
                            , let follow  = IntMap.fromList $ List.zip refColors decreasing
                            , let context = Search.Context.mk pcps precede follow
                          ]

  doSearch ::
    [ColorPoint.ColorPoint] -> [Color.Color] -> Search.Context.Context -> Strategy.Strategy -> State.State ->
      Maybe State.State
  doSearch []             _  _       _        s  = Just s
  doSearch pcps@(pcp : _) cs context strategy s
    | c == Color.blankColor = doSearchBlankColorPoint pcps cs context strategy s
    | otherwise             = doSearchFixedColorPoint pcps cs context strategy s
    where
      c = ColorPoint.color pcp

  -- pcp is a 0-color point.
  doSearchFreePoint ::
    [ColorPoint.ColorPoint] -> [Color.Color] -> Search.Context.Context -> Strategy.Strategy -> State.State ->
      Maybe State.State
  doSearchFreePoint []           _  _       _        _ =
    error "doSearchFreePoint. We shouldn't be there" -- make ghc -Werror happy
  doSearchFreePoint (pcp : pcps) cs context strategy s =
    Foldable.asum [State.pAppend (ColorPoint.updateColor c pcp) s >>= -- append new point
                   resolveConflicts strategy                      >>= -- resolve for match
                   doSearch pcps cs context' strategy
                     | c <- cs
                     , Search.Context.agree c y context
                     , let context' = Search.Context.update c y context
                  ]
    where
      y = ColorPoint.yCoord pcp

  -- pcp is not a 0-color point.
  doSearchFixedColorPoint ::
    [ColorPoint.ColorPoint] -> [Color.Color] -> Search.Context.Context -> Strategy.Strategy -> State.State ->
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
      context' = Search.Context.update c y context

  resolveConflicts :: Strategy.Strategy -> State.State -> Maybe State.State
  resolveConflicts strategy s =
    case strategy s of
      Nothing                             -> Just s
      Just (Conflict.OrderConflict pcp t) -> State.xResolve pcp t s >>=
                                             resolveConflicts strategy
      Just (Conflict.ValueConflict pcp t) -> State.yResolve pcp t s >>=
                                             resolveConflicts strategy
