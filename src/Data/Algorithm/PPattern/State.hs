{-|
Module      : Data.Algorithm.PPattern
Structription : Short Structription
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-2017
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Pattern matching for permutations.
-}

module Data.Algorithm.PPattern.State
(
  -- * The @State@ type
  State(..)

  -- * Exporting
,  toList

  -- * Constructing
, mk

  -- * Modifying
, pAppend
, xResolve
, yResolve
)
where

  import qualified Data.Monoid as Monoid

  import qualified Data.Algorithm.PPattern.Geometry.ColorPoint           as CP
  import qualified Data.Algorithm.PPattern.APerm                         as APerm
  import qualified Data.Algorithm.PPattern.State.Next                    as State.Next
  import qualified Data.Algorithm.PPattern.State.Access                  as State.Access
  import qualified Data.Algorithm.PPattern.State.Embedding               as State.Embedding
  import qualified Data.Algorithm.PPattern.State.IncreasingFactorization as State.IncreasingFactorization

  -- The state of a search
  data State =
    State { pColorPoints            :: [CP.ColorPoint]
          , qColorPoints            :: [CP.ColorPoint]
          , embedding               :: State.Embedding.Embedding
          , pRightmostMappedByColor :: State.Access.Access
          , qLeftmostByColor        :: State.Access.Access
          , qRightmostMappedByColor :: State.Access.Access
          , qRightmost              :: Maybe CP.ColorPoint
          , pNext                   :: State.Next.Next
          , qNext                   :: State.Next.Next
          }

  -- Show class
  instance Show State where
    show State { embedding = e } = State.Embedding.showEmbedding e

  {-|
    Make a new state. Permutation q is required.
  -}
  mk :: APerm.APerm a -> State
  mk q  = State { pColorPoints            = []
                , qColorPoints            = qcps
                , embedding               = State.Embedding.empty
                , pRightmostMappedByColor = State.Access.empty
                , qLeftmostByColor        = State.Access.mkLeftmostByColor qcps
                , qRightmostMappedByColor = State.Access.empty
                , qRightmost              = Nothing
                , pNext                   = State.Next.empty
                , qNext                   = n
                }
    where
      qcps = State.IncreasingFactorization.increasingFactorization q
      n    = State.Next.mk qcps

  toList :: State -> [(CP.ColorPoint, CP.ColorPoint)]
  toList = State.Embedding.toList . embedding

  {-|
    Add a new colored point to the list of colored points associated
    to permutation p. Update the state accordingly.
  -}
  pAppend :: CP.ColorPoint -> State -> Maybe State
  pAppend pcp s =
    case State.Access.query (CP.color pcp) (pRightmostMappedByColor s) of
      Nothing   -> pAppendAux1 pcp  pcp s
      Just pcp' -> pAppendAux1 pcp' pcp s

  pAppendAux1 :: CP.ColorPoint -> CP.ColorPoint -> State -> Maybe State
  pAppendAux1 pcp pcp' s =
    case State.Embedding.query pcp (embedding s) of
      Nothing  -> State.Access.query (CP.color pcp) (qLeftmostByColor s) >>=
                  pAppendAux2 pcp pcp' s
      Just qcp -> State.Next.query qcp (qNext s) >>=
                  pAppendAux2 pcp pcp' s

  pAppendAux2 ::
    CP.ColorPoint -> CP.ColorPoint  -> State -> CP.ColorPoint ->
    Maybe State
  pAppendAux2 pcp pcp' s qcp =
    case qRightmost s of
      Nothing   -> pAppendAux3 pcp pcp' qcp  s qcp
      Just qcp' -> pAppendAux3 pcp pcp' qcp' s qcp

  pAppendAux3 ::
    CP.ColorPoint -> CP.ColorPoint -> CP.ColorPoint -> State -> CP.ColorPoint ->
    Maybe State
  pAppendAux3 pcp pcp' qcp s qcp'
    | x' < x    = State.Next.query qcp' (qNext s) >>= pAppendAux3 pcp pcp' qcp s
    | otherwise = pAppendFinalize pcp pcp' qcp qcp' s
    where
      x  = CP.xCoord qcp
      x' = CP.xCoord qcp'

  pAppendFinalize ::
    CP.ColorPoint -> CP.ColorPoint -> CP.ColorPoint -> CP.ColorPoint -> State ->
    Maybe State
  pAppendFinalize pcp pcp' _ qcp' s = Just s'
    where
      c                        = CP.color pcp
      pColorPoints'            = pColorPoints s `Monoid.mappend` [pcp']
      embedding'               = State.Embedding.insert pcp' qcp' (embedding s)
      pRightmostMappedByColor' = State.Access.insert c pcp' (pRightmostMappedByColor s)
      qRightmostMappedByColor' = State.Access.insert c qcp' (qRightmostMappedByColor s)
      qRightmost'              = qcp'
      pNext'                   = if pcp /= pcp'
                                 then State.Next.insert pcp pcp' (pNext s)
                                 else pNext s

      s'   = s { pColorPoints            = pColorPoints'
               , embedding               = embedding'
               , pRightmostMappedByColor = pRightmostMappedByColor'
               , qRightmostMappedByColor = qRightmostMappedByColor'
               , qRightmost              = Just qRightmost'
               , pNext                   = pNext'
               }

  xResolve :: CP.ColorPoint -> Int -> State -> Maybe State
  xResolve pcp t s = State.Embedding.query pcp (embedding s) >>=
                     State.Next.xJumpThreshold t (qNext s)    >>=
                     resolve pcp s

  yResolve :: CP.ColorPoint -> Int -> State -> Maybe State
  yResolve pcp t s = State.Embedding.query pcp (embedding s) >>=
                     State.Next.yJumpThreshold t (qNext s)    >>=
                     resolve pcp s

  resolve :: CP.ColorPoint -> State -> CP.ColorPoint -> Maybe State
  resolve pcp s qcp =
    resolveAux (State.Next.query pcp (pNext s)) (State.Next.query qcp (qNext s)) s'
    where
      embedding' = State.Embedding.insert pcp qcp (embedding s)

      qRightmost' =
        case qRightmost s of
          Nothing   -> Just qcp
          Just qcp' -> if CP.xCoord qcp > CP.xCoord qcp'
                       then Just qcp
                       else Just qcp'

      qRightmostMappedByColor' =
        case State.Access.query (CP.color qcp) (qRightmostMappedByColor s) of
          Nothing   -> State.Access.insert (CP.color qcp) qcp (qRightmostMappedByColor s)
          Just qcp' -> if CP.xCoord qcp > CP.xCoord qcp'
                       then State.Access.insert (CP.color qcp) qcp (qRightmostMappedByColor s)
                       else qRightmostMappedByColor s

      s' = s { embedding               = embedding'
             , qRightmost              = qRightmost'
             , qRightmostMappedByColor = qRightmostMappedByColor'
             }

  resolveAux :: Maybe CP.ColorPoint -> Maybe CP.ColorPoint -> State -> Maybe State
  resolveAux Nothing    _          s = Just s
  resolveAux _          Nothing    _ = Nothing
  resolveAux (Just pcp) (Just qcp) s = State.Embedding.query pcp (embedding s) >>= resolveAux' pcp qcp s

  resolveAux' :: CP.ColorPoint -> CP.ColorPoint -> State -> CP.ColorPoint -> Maybe State
  resolveAux' pcp qcp s qcp'
    | CP.xCoord qcp <= CP.xCoord qcp' = Just s
    | otherwise                       = resolve pcp s qcp
