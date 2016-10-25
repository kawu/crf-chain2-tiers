{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Rank2Types #-}

module Data.CRF.Chain2.Tiers.DAG.Inference
(
--   tag
-- -- , probs
-- , marginals
-- , expectedFeatures
-- , accuracy
-- , zx
-- , zx'
) where


import           Control.Applicative ((<$>))

import           Data.Number.LogFloat as L
import qualified Data.Vector as V
import qualified Data.Array as A
import           Data.Maybe (fromJust)
import qualified Data.MemoCombinators as Memo

import           Data.DAG (EdgeID, DAG)
import qualified Data.DAG as DAG

import           Data.CRF.Chain2.Tiers.Core
import qualified Data.CRF.Chain2.Tiers.Model as Md
import           Data.CRF.Chain2.Tiers.DAG.Feature (EdgeIx(..))
import qualified Data.CRF.Chain2.Tiers.DAG.Feature as Ft


---------------------------------------------
-- Util Types
---------------------------------------------


-- | Accumulation function.
type AccF = [L.LogFloat] -> L.LogFloat


-- | Position in the sentence.
data Pos
  = Beg
  | Mid EdgeIx
  | End
  deriving (Show, Eq, Ord)


-- | Simplify the position by conflating `Beg` and `End` to `Nothing`.
simplify :: Pos -> Maybe EdgeIx
simplify (Mid x) = Just x
simplify Beg = Nothing
simplify End = Nothing


-- | Inverse operation of `simplify`, with the default position value.
complicate :: Pos -> Maybe EdgeIx -> Pos
complicate df Nothing = df
complicate _ (Just x) = Mid x


-- | First argument represents the current EdgeIx (Nothing if out of bounds),
-- the next argument represents the previous EdgeIx.
type ProbArray = Pos -> Pos -> L.LogFloat


---------------------------------------------
-- Memoization
---------------------------------------------


memoProbArray :: DAG a X -> ProbArray -> ProbArray
memoProbArray dag =
  Memo.memo2 memoPos memoPos
  where
    memoPos = undefined -- Memo.maybe (memoEdgeIx dag)


memoEdgeIx :: DAG a X -> Memo.Memo EdgeIx
memoEdgeIx dag =
  Memo.wrap fromPair toPair memoPair
  where
    memoPair = Memo.pair memoEdgeID Memo.integral
    memoEdgeID = Memo.unsafeArrayRange (DAG.minEdge dag, DAG.maxEdge dag)
    fromPair (x, y) = EdgeIx x y
    toPair (EdgeIx x y) = (x, y)


----------------------------------------------------
-- Potential
----------------------------------------------------


-- | Observation potential on a given position and a
-- given label (identified by index).
onWord :: Md.Model -> DAG a X -> EdgeIx -> L.LogFloat
onWord crf dag
  = product
  . map (Md.phi crf)
  . Ft.obFeatsOn dag
{-# INLINE onWord #-}


-- | Transition potential on a given position and a
-- given labels (identified by indexes).
onTransition
  :: Md.Model
  -> DAG a X
  -> Maybe EdgeIx
  -> Maybe EdgeIx
  -> Maybe EdgeIx
  -> L.LogFloat
onTransition crf dag u w v
  = product
  . map (Md.phi crf)
  $ Ft.trFeatsOn dag u w v
{-# INLINE onTransition #-}


---------------------------------------------
-- A bit more complex stuff
---------------------------------------------


-- | Forward table computation.
forward :: AccF -> Md.Model -> DAG a X -> ProbArray
forward acc crf dag =
  alpha
  where
    alpha = memoProbArray dag alpha'
    alpha' Beg Beg = 1.0
    alpha' End End = acc
      [ alpha End (Mid w)
        -- below, onTransition equals currently to 1; in general, there could be
        -- some related transition features, though.
        * onTransition crf dag Nothing Nothing (Just w)
      | w <- Ft.finalEdgeIxs dag ]
    alpha' u v = acc
      [ alpha v w * psi' u
        * onTransition crf dag (simplify u) (simplify v) (simplify w)
      | w <- complicate Beg <$> Ft.prevEdgeIxs dag (edgeID <$> simplify v) ]
    psi' u = case u of
      Mid x -> psi x
      _ -> 1.0
    psi = memoEdgeIx dag $ onWord crf dag


-- | Backward table computation.
backward :: AccF -> Md.Model -> DAG a X -> ProbArray
backward acc crf dag =
  beta
  where
    beta = memoProbArray dag beta'
    beta' End End = 1.0
    beta' Beg Beg = acc
      [ beta (Mid u) Beg * psi u
        * onTransition crf dag  (Just u) Nothing Nothing
      | u <- Ft.initialEdgeIxs dag ]
    beta' v w = acc
      [ beta u v * psi' u
        * onTransition crf dag (simplify u) (simplify v) (simplify w)
      | u <- complicate End <$> Ft.nextEdgeIxs dag (edgeID <$> simplify v) ]
    psi' u = case u of
      Mid x -> psi x
      _ -> 1.0
    psi = memoEdgeIx dag $ onWord crf dag


-- | Normalization factor computed for the sentence using the forward
-- computation.
zx :: Md.Model -> DAG a X -> L.LogFloat
zx crf = zxAlpha . forward sum crf

-- | Normalization factor based on the forward table.
zxAlpha :: ProbArray -> L.LogFloat
zxAlpha pa = pa End End


-- | Normalization factor computed for the sentence using the backward
-- computation.
zx' :: Md.Model -> DAG a X -> L.LogFloat
zx' crf = zxBeta . backward sum crf

-- | Normalization factor based on the backward table.
zxBeta :: ProbArray -> L.LogFloat
zxBeta pa = pa Beg Beg


-- | Probability of chosing the given pair of edges and the corresponding labels.
edgeProb2
  :: ProbArray
  -- ^ Forward probability table
  -> ProbArray
  -- ^ Backward probability table
  -> EdgeIx
  -- ^ Current edge and the corresponding label
  -> Maybe EdgeIx
  -- ^ Previous edge and the corresponding label
  -> L.LogFloat
edgeProb2 alpha beta u0 v0 =
  alpha u v * beta u v / zxAlpha alpha
  where
    u = Mid u0
    v = complicate Beg v0


-- | Probability of chosing the given edge and the corresponding label.
edgeProb1
  :: DAG a X
  -- ^ The underlying sentence DAG
  -> ProbArray
  -- ^ Forward probability table
  -> ProbArray
  -- ^ Backward probability table
  -> EdgeIx
  -- ^ Edge and the corresponding label
  -> L.LogFloat
edgeProb1 dag alpha beta u = sum
  [ edgeProb2 alpha beta u v
  | v <- Ft.prevEdgeIxs dag (Just $ edgeID u) ]


-- | Tag potential labels with marginal probabilities.
-- marginals :: Md.Model -> DAG a X -> DAG a [(Lb, L.LogFloat)]
marginals :: Md.Model -> DAG a X -> DAG a [L.LogFloat]
marginals crf dag =
  DAG.mapE label dag
  where
    label edgeID _ =
      [ prob1 edgeIx
      | edgeIx <- Ft.edgeIxs dag edgeID ]
    prob1 = edgeProb1 dag alpha beta
    alpha = forward sum crf dag
    beta = backward sum crf dag
