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

  import qualified Data.Algorithm.Patience as Patience

  import qualified Data.Algorithm.PPattern.Color               as Color
  import qualified Data.Algorithm.PPattern.Geometry.Point      as Point
  import qualified Data.Algorithm.PPattern.Geometry.ColorPoint as ColorPoint
  import qualified Data.Algorithm.PPattern.Strategy            as Strategy
  import qualified Data.Algorithm.PPattern.State               as State
  import qualified Data.Algorithm.PPattern.Search.Context      as Context
  import qualified Data.Algorithm.PPattern.Combinatorics       as Combinatorics
  import qualified Data.Algorithm.PPattern.Perm                as Perm
  import qualified Data.Algorithm.PPattern.Conflict            as Conflict

  -- Make an initial list of colored points. Each element from the longest
  -- decreasing subsequence is given a distinct colors. All other elements
  -- are given the 'not determined yet' color 0.
  initialColorPoints :: [Point.Point] -> [Point.Point] -> [Color.Color] -> [ColorPoint.ColorPoint]
  initialColorPoints []       _  _  = []
  initialColorPoints (p : ps) [] cs =  ColorPoint.mk2Blank p : initialColorPoints ps [] cs
  initialColorPoints (_ : []) _  _  = error "Search.initialColorPoints" -- make ghc -Werror happy
  initialColorPoints (p : ps) ps''@(p' : ps') cs'@(c : cs)
    | Point.yCoord p == Point.yCoord p' =
      ColorPoint.mk2 p c : initialColorPoints ps ps' cs
    | otherwise                         =
      ColorPoint.mk2Blank p : initialColorPoints ps ps'' cs'

  -- Extract embedding in case of a direct search
  present :: State.State -> Maybe [(ColorPoint.ColorPoint, ColorPoint.ColorPoint)]
  present = Just . State.toList

  -- Extract embedding in case of a reverse search
  presentReverse :: Int -> Int -> State.State -> Maybe [(ColorPoint.ColorPoint, ColorPoint.ColorPoint)]
  presentReverse pSize qSize  = Just . Foldable.foldr f [] . State.toList
    where
      f (cp1, cp2) acc = (cp1', cp2') : acc
        where
          cp1' = ColorPoint.updateXCoord (pSize + 1 - ColorPoint.xCoord cp1) cp1
          cp2' = ColorPoint.updateXCoord (qSize + 1 - ColorPoint.xCoord cp2) cp2

  mkRightLongestDecreasing :: [Point.Point] -> IntMap.IntMap Int
  mkRightLongestDecreasing ps = mkRightLongestDecreasingAux IntMap.empty ps ys
    where
      ys = fmap (\ p -> (Point.yCoord p, undefined)) ps

  mkRightLongestDecreasingAux :: IntMap.IntMap Int -> [Point.Point] -> [(Int, t)] -> IntMap.IntMap Int
  mkRightLongestDecreasingAux m []       _        = m
  mkRightLongestDecreasingAux m (p : []) []       = IntMap.insert (Point.yCoord p) 0 m
  mkRightLongestDecreasingAux m (p : ps) (_ : ys) = mkRightLongestDecreasingAux m' ps ys'
    where
      longestIncreasing       = Patience.longestIncreasing $ List.reverse ys
      longestDecreasingLength = List.length longestIncreasing
      m'                      = IntMap.insert (Point.yCoord p) longestDecreasingLength m
      ys'                     = List.filter (\ (y, _) -> Point.yCoord p > y) ys

  -- Search for an order-isomorphic occurrence of permutation p into permutation q.
  -- Resolve conflicts according to a given strategy.
  search :: Perm.Perm a -> Perm.Perm a -> Strategy.Strategy -> Maybe [(ColorPoint.ColorPoint, ColorPoint.ColorPoint)]
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
      -- Permutation p as points
      pPoints = Perm.points p

      -- p longest decressing by suffixes
      pRightLongestDecreasing = mkRightLongestDecreasing pPoints

      -- permutation p longest decreasing data
      pLongestDecreasing = Perm.longestDecreasing p
      pLongestDecreasingLength = Perm.size pLongestDecreasing
      pLongestDecreasingPoints = Perm.points pLongestDecreasing

      -- initial state
      s = State.mk q
      cs = Color.palette 1 qLongestDecreasingLength

      -- Embed p and perform search
      computation =
        Foldable.asum [doSearch pcps cs context strategy s
                        | refColors   <- cs `Combinatorics.choose` pLongestDecreasingLength
                        , let pcps    = initialColorPoints pPoints pLongestDecreasingPoints refColors
                        , let precede = IntMap.empty
                        , let ys      = fmap Point.yCoord pLongestDecreasingPoints
                        , let pairs   = List.zip refColors ys
                        , let follow  = IntMap.fromList pairs
                        , let context = Context.mk precede follow pRightLongestDecreasing
                      ]

  doSearch ::
    [ColorPoint.ColorPoint] -> [Color.Color] ->
    Context.Context -> Strategy.Strategy -> State.State ->
      Maybe State.State
  doSearch [] _ _ _ s  = Just s
  doSearch pcps@(pcp : _) cs context strategy s
    | c == Color.blankColor = doSearchBlankPoint pcps cs context strategy s
    | otherwise             = doSearchColorPoint pcps cs context strategy s
    where
      c = ColorPoint.color pcp

  -- pcp is a blank point.
  doSearchBlankPoint ::
    [ColorPoint.ColorPoint] -> [Color.Color] -> Context.Context -> Strategy.Strategy -> State.State ->
    Maybe State.State
  doSearchBlankPoint [] _ _ _ _ =
    error "doSearchFreePoint. We shouldn't be there" -- make ghc -Werror happy
  doSearchBlankPoint (pcp : pcps) cs context strategy s =
    Foldable.asum [State.pAppend (ColorPoint.updateColor c pcp) s >>= -- append new point
                   resolveConflicts strategy                      >>= -- resolve for match
                   doSearch pcps cs  context' strategy
                     | c <- cs
                     , Context.agree c y context
                     , Context.allowedColor c y cs context
                     , let context' = Context.update c y context
                  ]
    where
      y = ColorPoint.yCoord pcp

  -- pcp is not a 0-color point.
  doSearchColorPoint ::
    [ColorPoint.ColorPoint] -> [Color.Color] -> Context.Context -> Strategy.Strategy -> State.State ->
    Maybe State.State
  doSearchColorPoint [] _ _ _ _ =
    error "doSearchFixedColorPoint. We shouldn't be there" -- make ghc -Werror happy
  doSearchColorPoint (pcp : pcps) cs context strategy s =
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
