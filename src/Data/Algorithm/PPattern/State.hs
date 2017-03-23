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
  State

  -- * Constructing
, mk

  -- * Modifying
, pAppend
, xResolve
, yResolve
)
where

  import qualified Data.Monoid as Monoid

  import qualified Data.Algorithm.PPattern.Perm            as Perm
  import qualified Data.Algorithm.PPattern.ColorPoint      as ColorPoint
  import qualified Data.Algorithm.PPattern.Color           as Color
  import qualified Data.Algorithm.PPattern.State.Next      as State.Next
  import qualified Data.Algorithm.PPattern.State.Access    as State.Access
  import qualified Data.Algorithm.PPattern.State.Embedding as State.Embedding

  -- The state of a search
  data State =
    State { pColorPoints            :: [ColorPoint.ColorPoint]
          , qColorPoints            :: [ColorPoint.ColorPoint]
          , embedding               :: State.Embedding.Embedding
          , pRightmostMappedByColor :: State.Access.Access
          , qLeftmostByColor        :: State.Access.Access
          , qRightmostMappedByColor :: State.Access.Access
          , qRightmost              :: Maybe ColorPoint.ColorPoint
          , pNext                   :: State.Next.Next
          , qNext                   :: State.Next.Next
          }

  -- Show class
  instance Show State where
    show State { embedding = e } = State.Embedding.showEmbedding e

  {-|
    Make a new state. Permutation q is required.
  -}
  mk :: Perm.Perm -> State
  mk q  = State { pColorPoints            = []
                , qColorPoints            = qcps
                , embedding               = emptyEmbedding
                , pRightmostMappedByColor = emptyAccess
                , qLeftmostByColor        = mkLeftmostByColor qcps
                , qRightmostMappedByColor = emptyAccess
                , qRightmost              = Nothing
                , pNext                   = emptyNext
                , qNext                   = n
                }
    where
      qcps = Perm.ColorPoint.increasingFactorization q
      n    = State.Next.mk qcps

  {-|
    Add a new colored point to the list of colored points associated
    to permutation p. Update the state accordingly.
  -}
  pAppend :: ColorPoint.ColorPoint -> State -> Maybe State
  pAppend pcp s =
    case lookupAccess (ColorPoint.color pcp) (pRightmostMappedByColor s) of
      Nothing   -> pAppendAux1 pcp  pcp s
      Just pcp' -> pAppendAux1 pcp' pcp s

  pAppendAux1 :: ColorPoint.ColorPoint -> ColorPoint.ColorPoint -> State -> Maybe State
  pAppendAux1 pcp pcp' s =
    case lookupEmbedding pcp (embedding s) of
      Nothing  -> State.Access.lookup (ColorPoint.color pcp) (qLeftmostByColor s) >>=
                  pAppendAux2 pcp pcp' s
      Just qcp -> State.Next.lookup qcp (qNext s) >>=
                  pAppendAux2 pcp pcp' s

  pAppendAux2 ::
    ColorPoint.ColorPoint -> ColorPoint.ColorPoint  -> State -> ColorPoint.ColorPoint ->
    Maybe State
  pAppendAux2 pcp pcp' s qcp =
    case qRightmost s of
      Nothing   -> pAppendAux3 pcp pcp' qcp  s qcp
      Just qcp' -> pAppendAux3 pcp pcp' qcp' s qcp

  pAppendAux3 ::
    ColorPoint.ColorPoint -> ColorPoint.ColorPoint -> ColorPoint.ColorPoint -> State -> ColorPoint.ColorPoint ->
    Maybe State
  pAppendAux3 pcp pcp' qcp s qcp'
    | x' < x    = lookupNext qcp' (qNext s) >>= pAppendAux3 pcp pcp' qcp s
    | otherwise = pAppendFinalize pcp pcp' qcp qcp' s
    where
      x  = ColorPoint.xCoord qcp
      x' = ColorPoint.xCoord qcp'

  pAppendFinalize ::
    ColorPoint.ColorPoint -> ColorPoint.ColorPoint -> ColorPoint.ColorPoint -> ColorPoint.ColorPoint -> State ->
    Maybe State
  pAppendFinalize pcp pcp' _ qcp' s = Just s'
    where
      c                        = ColorPoint.color pcp
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

  xResolve :: ColorPoint.ColorPoint -> Int -> State -> Maybe State
  xResolve pcp t s = State.Embedding.lookup pcp (embedding s) >>=
                     State.Next.xJumpThreshold t (qNext s)    >>=
                     resolve pcp s

  yResolve :: ColorPoint.ColorPoint -> Int -> State -> Maybe State
  yResolve pcp t s = State.Embedding.lookup pcp (embedding s) >>=
                     State.Next.yJumpThreshold t (qNext s)    >>=
                     resolve pcp s

  resolve :: ColorPoint.ColorPoint -> State -> ColorPoint.ColorPoint -> Maybe State
  resolve pcp s qcp =
    resolveAux (State.Next.lookup pcp (pNext s)) (lookupNext qcp (qNext s)) s'
    where
      embedding' = State.Embedding.insert pcp qcp (embedding s)

      qRightmost' =
        case qRightmost s of
          Nothing   -> Just qcp
          Just qcp' -> if ColorPoint.xCoord qcp > ColorPoint.xCoord qcp'
                       then Just qcp
                       else Just qcp'

      qRightmostMappedByColor' =
        case State.Access.lookup(ColorPoint.color qcp) (qRightmostMappedByColor s) of
          Nothing   -> State.Access.insert (ColorPoint.color qcp) qcp (qRightmostMappedByColor s)
          Just qcp' -> if ColorPoint.xCoord qcp > ColorPoint.xCoord qcp'
                       then State.Access.insert (ColorPoint.color qcp) qcp (qRightmostMappedByColor s)
                       else qRightmostMappedByColor s

      s' = s { embedding               = embedding'
             , qRightmost              = qRightmost'
             , qRightmostMappedByColor = qRightmostMappedByColor'
             }

  resolveAux :: Maybe ColorPoint.ColorPoint -> Maybe ColorPoint.ColorPoint -> State -> Maybe State
  resolveAux Nothing    _          s = Just s
  resolveAux _          Nothing    _ = Nothing
  resolveAux (Just pcp) (Just qcp) s = State.Embedding.lookup pcp (embedding s) >>=
                                       resolveAux' pcp qcp s

  resolveAux' :: ColorPoint.ColorPoint -> ColorPoint.ColorPoint -> State -> ColorPoint.ColorPoint -> Maybe State
  resolveAux' pcp qcp s qcp'
    | ColorPoint.xCoord qcp <= ColorPoint.xCoord qcp' = Just s
    | otherwise                                       = resolve pcp s qcp
