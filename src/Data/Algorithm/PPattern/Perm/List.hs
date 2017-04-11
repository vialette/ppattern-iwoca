{-|
Module      : Data.Algorithm.PPattern.Perm.List
Description : Short description
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-2017
License     : MIT
Maintainer  : vialette@gmaiList.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Data.Algorithm.PPattern.Perm.List
(
  -- *
  sub

, isMonotone

, longestIncreasing
, longestDecreasing

, reversal
, complement
, reversalComplement
, inverse

, stackSort

, leftToRightMinima
, leftToRightMaxima
)
  where

    import qualified Data.List          as List
    import qualified Data.Tuple         as Tuple
    import qualified Data.Foldable      as Foldable
    import qualified Data.IntMap.Strict as IntMap

    import qualified Data.Algorithm.Patience as Patience

    import qualified Data.Algorithm.PPattern.Perm.T              as Perm.T
    import qualified Data.Algorithm.PPattern.Geometry.Point      as P
    import qualified Data.Algorithm.PPattern.Geometry.Point.List as P.List
    import qualified Data.Algorithm.PPattern.List                as List.Tools
    import qualified Data.Algorithm.PPattern.StackSort           as StackSort

    --
    sub :: Int -> Int -> [Perm.T.T a] -> [Perm.T.T a]
    sub xMin xMax = aux
      where
        aux [] = []
        aux (t@(Perm.T.T (p, _)) : ts)
          | P.xCoord p < xMin = aux ts
          | P.xCoord p < xMax = t : aux ts
          | otherwise         = []

    -- Auxiliary function for isIncreasing and isDecreasing
    isMonotone :: (Int -> Int -> Bool) -> [Perm.T.T a] -> Bool
    isMonotone cmp = go
      where
        go []  = True
        go [_] = True
        go ts   = Foldable.foldl f True $ List.Tools.consecutivePairs ts
          where
            f acc (Perm.T.T (p, _), Perm.T.T (p', _)) = acc && P.yCoord p `cmp` P.yCoord p'


    longestIncreasing :: [Perm.T.T a] -> [Perm.T.T a]
    longestIncreasing = unformat . List.reverse . doSearch . format
      where
        format   = fmap (\ t@(Perm.T.T (p, _)) -> (P.yCoord p, t))
        doSearch = Patience.longestIncreasing
        unformat = fmap Tuple.snd

    longestDecreasing :: [Perm.T.T a] -> [Perm.T.T a]
    longestDecreasing = unformat . doSearch . format
      where
        format   = List.reverse . fmap (\ t@(Perm.T.T (p, _)) -> (P.yCoord p, t))
        doSearch = Patience.longestIncreasing
        unformat = fmap Tuple.snd

    reversal :: [Perm.T.T a] -> [Perm.T.T a]
    reversal ts = Foldable.foldl f [] ts
      where
        n = List.length ts

        f acc (Perm.T.T (p, a)) = Perm.T.mk p' a : acc
          where
            x = P.xCoord p
            x' = n + 1 - x

            p' = P.updateXCoord x' p

    injectAnnotations :: IntMap.IntMap a -> [P.Point] -> [(P.Point, a)]
    injectAnnotations m = fmap mk
      where
        mk p = case IntMap.lookup (P.yCoord p) m of
                 Nothing -> error "Data.Algorithm.PPattern.Perm.List.merge. Empty map"
                 Just a  -> (p, a)

    mkAnnotationMap :: [Int] -> [a] -> IntMap.IntMap a
    mkAnnotationMap ys as = IntMap.fromList $ List.zip ys as

    complement :: [Perm.T.T a] -> [Perm.T.T a]
    complement ts = fmap (Tuple.uncurry Perm.T.mk) . injectAnnotations m $ complementAux ys
      where
        ps = fmap Perm.T.point ts
        ys = fmap P.yCoord ps
        as = fmap Perm.T.annotation ts
        m  = mkAnnotationMap ys as

    complementAux :: [Int] -> [P.Point]
    complementAux ys = P.List.mkSequential $ fmap f ys
      where
        n   = List.length ys
        f y = n+1-y

    reversalComplement :: [Perm.T.T a] -> [Perm.T.T a]
    reversalComplement ts = fmap (Tuple.uncurry Perm.T.mk) . injectAnnotations m $ reversalComplementAux ys
      where
        ps = fmap Perm.T.point ts
        ys = fmap P.yCoord ps
        as = fmap Perm.T.annotation ts
        m  = mkAnnotationMap ys as

    reversalComplementAux :: [Int] -> [P.Point]
    reversalComplementAux ys = P.List.mkSequential . fmap f $ List.reverse ys
      where
        n   = List.length ys
        f y = n+1-y

    inverse :: [Perm.T.T a] -> [Perm.T.T a]
    inverse ts = fmap (Tuple.uncurry Perm.T.mk) . injectAnnotations m $ inverseAux ys
      where
        ps = fmap Perm.T.point ts
        ys = fmap P.yCoord ps
        as = fmap Perm.T.annotation ts
        m  = mkAnnotationMap ys as

    inverseAux :: [Int] -> [P.Point]
    inverseAux = P.List.mkSequential . fmap Tuple.snd . List.sort . flip List.zip [1..]

    stackSort :: [Perm.T.T a] -> [Perm.T.T a]
    stackSort ts = fmap (Tuple.uncurry Perm.T.mk) . injectAnnotations m . P.List.mkSequential $ StackSort.stackSort ys
      where
        ps = fmap Perm.T.point ts
        ys = fmap P.yCoord ps
        as = fmap Perm.T.annotation ts
        m  = mkAnnotationMap ys as

    leftToRightMinima :: [Perm.T.T a] -> [Perm.T.T a]
    leftToRightMinima = leftToRight (>)

    leftToRightMaxima :: [Perm.T.T a] -> [Perm.T.T a]
    leftToRightMaxima = leftToRight (<)

    leftToRight :: (Int -> Int -> Bool) -> [Perm.T.T a] -> [Perm.T.T a]
    leftToRight cmp = aux []
      where
        aux acc []       = List.reverse acc
        aux []  (t : ts) = aux [t] ts
        aux acc@(t' : _) (t : ts)
          | y' `cmp` y = aux (t : acc) ts
          | otherwise  = aux acc ts
          where
            y  = P.yCoord $ Perm.T.point t
            y' = P.yCoord $ Perm.T.point t'
