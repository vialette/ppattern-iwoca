{-|
Module      : Data.Algorithm.PPattern.Search
Description : Short description
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-1017
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Pattern matching.
-}

module Data.Algorithm.PPattern.Search
(
  -- * Searching
  search

  -- temp
, initialColorPoints
, searchAux
, doSearch
, doSearchBlankPoint
, doSearchColorPoint
, mkRightDecs
)
where

  import qualified Data.List          as List
  import qualified Data.Foldable      as Foldable
  import qualified Data.IntMap.Strict as IntMap

  import qualified Data.Algorithm.Patience as Patience

  import qualified Data.Algorithm.PPattern.Combinatorics            as Combinatorics
  import qualified Data.Algorithm.PPattern.Perm                     as Perm
  import qualified Data.Algorithm.PPattern.Perm.Monotone            as Perm.Monotone
  import qualified Data.Algorithm.PPattern.Perm.Operation           as Perm.Operation
  import qualified Data.Algorithm.PPattern.Color                    as Color
  import qualified Data.Algorithm.PPattern.Geometry.Point           as Point
  import qualified Data.Algorithm.PPattern.Geometry.ColorPoint      as ColorPoint
  import qualified Data.Algorithm.PPattern.Search.ConflictSelection as ConflictSelection
  import qualified Data.Algorithm.PPattern.Search.State             as State
  import qualified Data.Algorithm.PPattern.Search.Context           as Context
  import qualified Data.Algorithm.PPattern.Search.Conflict          as Conflict
  import qualified Data.Algorithm.PPattern.Search.Occurrence        as Occurrence

  -- Make an initial list of colored point. Each element from the longest
  -- decreasing subsequence is given a distinct color. All other elements
  -- are given the 'not determined yet' color 0.
  initialColorPoints :: [Point.Point] -> [Point.Point] -> [Color.Color] -> [ColorPoint.ColorPoint]
  initialColorPoints _        (_ : _) []      = error "Search.initialColorPoints _ (_ : _) []" -- make ghc -Werror happy
  initialColorPoints _        []      (_ : _) = error "Search.initialColorPoints _ [] (_ : _)" -- make ghc -Werror happy
  initialColorPoints []       _   _   = []
  initialColorPoints (p : ps) []  cs  = ColorPoint.mkBlank' p : initialColorPoints ps [] cs
  initialColorPoints (p : ps) ps''@(p' : ps') cs'@(c : cs)
    | Point.yCoord p == Point.yCoord p' = ColorPoint.mk' p c : initialColorPoints ps ps' cs
    | otherwise                         = ColorPoint.mkBlank' p : initialColorPoints ps ps'' cs'

  -- Extract embedding in case of a direct search
  mkOccurrence :: State.State -> Maybe Occurrence.Occurrence
  mkOccurrence = Just . Occurrence.mk . State.toList

  -- Extract embedding in case of a reverse search
  mkReverseOccurrence :: Int -> Int -> State.State -> Maybe Occurrence.Occurrence
  mkReverseOccurrence pSize qSize  = Just . Occurrence.mk . List.reverse . Foldable.foldr f [] . State.toList
    where
      f (cp1, cp2) acc = (cp1', cp2') : acc
        where
          cp1' = ColorPoint.updateXCoord (pSize + 1 - ColorPoint.xCoord cp1) cp1
          cp2' = ColorPoint.updateXCoord (qSize + 1 - ColorPoint.xCoord cp2) cp2

  -- 'mkRightDecs ps' returns a map m defined as follows.
  -- m y is the longest decreasing subsequence (on y coordinates) in the suffix
  -- of ps starting at the entry with y-coordinate y.
  mkRightDecs :: [Point.Point] -> IntMap.IntMap Int
  mkRightDecs = mkRightDecsAux IntMap.empty . fmap f
    where
      -- Patience.longestIncreasing asks for a list of pairs with data snd element.
      -- As we do not use this data snd element, we rely on undefined.
      f p = (Point.yCoord p, undefined)

  -- Max longest decreasing length by suffix auxiliary function.
  mkRightDecsAux :: IntMap.IntMap Int -> [(Int, t)] -> IntMap.IntMap Int
  mkRightDecsAux m []            = m
  mkRightDecsAux m [(y, _)]      = IntMap.insert y 0 m
  mkRightDecsAux m ((y, _) : ys) = mkRightDecsAux m' ys
    where
      ys'                     = List.filter (\ (y', _) -> y > y') ys
      longestIncreasing       = Patience.longestIncreasing $ List.reverse ys'
      longestDecreasingLength = List.length longestIncreasing
      m'                      = IntMap.insert y longestDecreasingLength m

  -- Search for an order-isomorphic occurrence of a permutation p into a permutation q.
  -- Resolve conflicts according to a given conflictSelection.
  search :: Perm.Perm -> Perm.Perm -> ConflictSelection.ConflictSelection -> Maybe Occurrence.Occurrence
  search p q conflictSelection
    | m > n                    = Nothing
    | qDecLength <= qIncLength = searchAux p        q        qDecLength  conflictSelection       >>= mkOccurrence
    | otherwise                = searchAux pReverse qReverse qReverseDecLength conflictSelection >>= mkReverseOccurrence m n
    where
      m = Perm.size p
      n = Perm.size q

      qDecLength = Perm.Monotone.longestDecreasingLength q
      qIncLength = Perm.Monotone.longestIncreasingLength q

      pReverse = Perm.Operation.reversal p
      qReverse = Perm.Operation.reversal q

      qReverseDecLength = Perm.Monotone.longestDecreasingLength qReverse

  -- Search auxiliary function.
  -- `seachAux p q k s` take a pattern 'p', a target permutation 'q',
  -- the longest decreasing subsequence in the target permutation 'k' and
  -- a conflict ressolving conflictSelection 's'.
  -- It returns 'Just state' if 'p' is contains in 'q', Nothing otherwise.
  searchAux :: Perm.Perm -> Perm.Perm -> Int -> ConflictSelection.ConflictSelection -> Maybe State.State
  searchAux p q qDecLength conflictSelection
    | pDecLength > qDecLength = Nothing
    | otherwise               = computation
    where
      -- Permutation p as points
      pps = Perm.points p

      -- p longest decreassing by suffixes
      pRightDecs = mkRightDecs pps

      -- Permutation p longest decreasing data
      pDec       = Perm.Monotone.longestDecreasing p
      pDecLength = Perm.size pDec
      pDecPoints = Perm.points pDec

      -- initial state
      s = State.mk q
      cs = Color.palette 1 qDecLength

      -- Embed p and perform search
      computation = Foldable.asum [doSearch pcps cs context conflictSelection s
                                    | refColors   <- cs `Combinatorics.choose` pDecLength
                                    , let pcps    = initialColorPoints pps pDecPoints refColors
                                    , let precede = IntMap.empty
                                    , let ys      = fmap Point.yCoord pDecPoints
                                    , let pairs   = List.zip refColors ys
                                    , let follow  = IntMap.fromList pairs
                                    , let context = Context.mk precede follow pRightDecs
                                  ]

  --
  --
  doSearch :: [ColorPoint.ColorPoint] -> [Color.Color] -> Context.Context -> ConflictSelection.ConflictSelection -> State.State -> Maybe State.State
  doSearch [] _ _ _ s  = Just s
  doSearch pcps cs context conflictSelection s
    | Color.isBlankColor c = doSearchBlankPoint pcps cs context conflictSelection s
    | otherwise            = doSearchColorPoint pcps cs context conflictSelection s
    where
      pcp = List.head pcps
      c   = ColorPoint.color pcp

  -- pcp is a blank point.
  doSearchBlankPoint :: [ColorPoint.ColorPoint] -> [Color.Color] -> Context.Context -> ConflictSelection.ConflictSelection -> State.State -> Maybe State.State
  doSearchBlankPoint []           _  _       _        _ = error "doSearchFreePoint. We shouldn't be there" -- make ghc -Werror happy
  doSearchBlankPoint (pcp : pcps) cs context conflictSelection s =
    Foldable.asum [State.pAppend (ColorPoint.updateColor c pcp) s >>= -- append new point
                   resolveConflicts conflictSelection                      >>= -- resolve for match
                   doSearch pcps cs context' conflictSelection
                     | c <- cs
                     , Context.agree c y context
                     , Context.allowedColor c y cs context
                     , let context' = Context.update c y context
                  ]
    where
      y = ColorPoint.yCoord pcp

  -- pcp is not a blank point.s
  doSearchColorPoint :: [ColorPoint.ColorPoint] -> [Color.Color] -> Context.Context -> ConflictSelection.ConflictSelection -> State.State -> Maybe State.State
  doSearchColorPoint []           _  _       _        _ = error "doSearchFixedColorPoint. We shouldn't be there" -- make ghc -Werror happy
  doSearchColorPoint (pcp : pcps) cs context conflictSelection s = State.pAppend pcp s       >>=
                                                                   resolveConflicts conflictSelection >>=
                                                                   doSearch pcps cs context' conflictSelection
    where
      y = ColorPoint.yCoord pcp
      c = ColorPoint.color  pcp
      context' = Context.update c y context

  -- Resolve all conflicts (according to the given conflictSelection).
  resolveConflicts :: ConflictSelection.ConflictSelection -> State.State -> Maybe State.State
  resolveConflicts conflictSelection s =
    case conflictSelection s of
      Nothing                                  -> Just s
      Just (Conflict.HorizontalConflict pcp t) -> State.xResolve pcp t s >>= resolveConflicts conflictSelection
      Just (Conflict.VerticalConflict pcp t)   -> State.yResolve pcp t s >>= resolveConflicts conflictSelection
