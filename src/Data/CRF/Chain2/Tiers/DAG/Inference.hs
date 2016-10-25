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


-- -- | TODO:
-- -- HYPO: The first CbIx is actually an EdgeID.
-- type ProbArray = CbIx -> CbIx -> CbIx -> L.LogFloat
-- 
-- 
-- type ProbArray' = EdgeID -> CbIx -> EdgeID -> CbIx -> L.LogFloat
-- type ProbArray'' = (EdgeID, CbIx) -> (EdgeID, CbIx) -> L.LogFloat
-- type ProbArray''' = EdgeIx -> EdgeIx -> L.LogFloat


-- | First argument represents the current EdgeIx (Nothing if out of bounds),
-- the next argument represents the previous EdgeIx.
type ProbArray = Maybe EdgeIx -> Maybe EdgeIx -> L.LogFloat


-- -- | First two arguments relate to the edge IDs of the current and
-- -- the prevous edge, the subsequent two CbIxs correspond to the
-- -- first EdgeID and the second EdgeID, respectively.
-- type ProbArrayA = (EdgeID, EdgeID) -> (CbIx, CbIx) -> L.LogFloat


---------------------------------------------
-- Memoization
---------------------------------------------


memoProbArray :: DAG a X -> ProbArray -> ProbArray
memoProbArray dag =
  Memo.memo2 mayMemoEdgeIx mayMemoEdgeIx
  where
    mayMemoEdgeIx = Memo.maybe (memoEdgeIx dag)


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
    alpha' Nothing Nothing = 1.0
    alpha' u v = acc
      [ alpha v w * psi (fromJust u)
        * onTransition crf dag u v w
      | w <- Ft.prevEdgeIxs dag (edgeID <$> v) ]
    psi = memoEdgeIx dag $ onWord crf dag


-- | Backward table computation.
backward :: AccF -> Md.Model -> DAG a X -> ProbArray
backward acc crf dag =
  beta
  where
    beta = memoProbArray dag beta'
    beta' Nothing Nothing = 1.0
    beta' v w = acc
      [ beta u v * psi' u
        * onTransition crf dag u v w
      | u <- Ft.nextEdgeIxs dag (edgeID <$> v) ]
    psi' Nothing = 1.0
    psi' (Just u) = psi u
    psi = memoEdgeIx dag $ onWord crf dag


-- | Normalization factor based on probability array.
zxProbArray :: ProbArray -> L.LogFloat
zxProbArray pa = pa Nothing Nothing


-- | Normalization factor computed for the sentence using the backward
-- computation.
zx :: Md.Model -> DAG a X -> L.LogFloat
zx crf = zxProbArray . backward sum crf


-- | Normalization factor computed for the sentence using the forward
-- computation.
zx' :: Md.Model -> DAG a X -> L.LogFloat
zx' crf = zxProbArray . forward sum crf


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
edgeProb2 alpha beta u v =
  alpha (Just u) v * beta (Just u) v / zxProbArray alpha


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
