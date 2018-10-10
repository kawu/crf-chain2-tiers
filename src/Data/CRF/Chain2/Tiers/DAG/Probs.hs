{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Rank2Types #-}


module Data.CRF.Chain2.Tiers.DAG.Probs
( probability
, likelihood
, parLikelihood
) where


import           GHC.Conc (numCapabilities)

import           Control.Applicative ((<$>))
import qualified Control.Arrow as Arr
-- import qualified Control.Parallel as Par
import qualified Control.Parallel.Strategies as Par

import qualified Data.Number.LogFloat as L
import qualified Data.Vector as V
-- import qualified Data.Vector.Unboxed as U
-- import qualified Data.Array as A
-- import qualified Data.Set as S
import           Data.Maybe (maybeToList)
-- import qualified Data.MemoCombinators as Memo
-- import qualified Data.List as List
-- import           Data.Function (on)
-- import qualified Data.Foldable as F

import           Data.DAG (EdgeID, DAG)
import qualified Data.DAG as DAG

import qualified Data.CRF.Chain2.Tiers.Core as C
import           Data.CRF.Chain2.Tiers.Core (X, Y, Ob, Cb, CbIx)
import qualified Data.CRF.Chain2.Tiers.Model as Md
import           Data.CRF.Chain2.Tiers.Util (partition)
import           Data.CRF.Chain2.Tiers.DAG.Feature (EdgeIx(..))
-- import qualified Data.CRF.Chain2.Tiers.DAG.Feature as Ft

import           Data.CRF.Chain2.Tiers.DAG.Inference
                 (AccF, Pos(..), simplify, complicate, ProbArray)
import qualified Data.CRF.Chain2.Tiers.DAG.Inference as I

-- import Debug.Trace (trace)


--------------------------------------------
-- Indexing
---------------------------------------------


-- | List of observations on the given edge of the sentence.
obList :: DAG a (X, Y) -> EdgeID -> [Ob]
obList dag i = C.unX . fst $ DAG.edgeLabel i dag
{-# INLINE obList #-}


-- | Vector of labels and the corresponding probabilities on the given edge of
-- the sentence.
lbVec :: DAG a (X, Y) -> EdgeID -> V.Vector (Cb, Double)
lbVec dag edgeID =
  case DAG.edgeLabel edgeID dag of
    (_, y) -> C._unY y
{-# INLINE lbVec #-}


-- | Number of potential labels on the given edge of the sentence.
lbNum :: DAG a (X, Y) -> EdgeID -> Int
lbNum dag = V.length . lbVec dag
{-# INLINE lbNum #-}


-- | Label on the given edge and on the given position.
lbOn :: DAG a (X, Y) -> EdgeID -> CbIx -> Maybe (Cb, L.LogFloat)
lbOn dag i = fmap (Arr.second L.logToLogFloat) . (lbVec dag i V.!?)
{-# INLINE lbOn #-}


-- | List of label indices at the given edge.
lbIxs :: DAG a (X, Y) -> EdgeID -> [CbIx]
lbIxs dag i = [0 .. lbNum dag i - 1]
{-# INLINE lbIxs #-}


-- | The list of EdgeIx's corresponding to the given edge.
edgeIxs :: DAG a (X, Y) -> EdgeID -> [EdgeIx]
edgeIxs dag i =
  [ EdgeIx {edgeID=i, lbIx=u}
  | u <- lbIxs dag i ]


--------------------------------------------
-- Indexing advanced
---------------------------------------------


-- | The list of EdgeIx's corresponding to the previous edges. If the argument
-- edgeID is `Nothing` or if the list of previous edges is empty, the result
-- will be a singleton list containing `Nothing` (which represents a special
-- out-of-bounds EdgeIx).
prevEdgeIxs :: DAG a (X, Y) -> Maybe EdgeID -> [Maybe EdgeIx]
prevEdgeIxs _ Nothing = [Nothing]
prevEdgeIxs dag (Just i)
  | null js = [Nothing]
  | otherwise = Just <$>
    [ EdgeIx {edgeID=j, lbIx=u}
    | j <- js, u <- lbIxs dag j ]
  where js = DAG.prevEdges i dag


-- | Obtain the list of final EdgeIxs.
finalEdgeIxs :: DAG a (X, Y) -> [EdgeIx]
finalEdgeIxs dag = concat
  [ edgeIxs dag i
  | i <- DAG.dagEdges dag
  , DAG.isFinalEdge i dag ]


---------------------------------------------
-- Feature alternatives
---------------------------------------------


-- | Observation features on a given position and with respect
-- to a given label (determined by index).
obFeatsOn :: DAG a (X, Y) -> EdgeIx -> [C.Feat]
obFeatsOn dag EdgeIx{..} = concat
  [ C.obFeats o e
  | (e, _prob) <- maybeToList $ lbOn dag edgeID lbIx
  , o <- obList dag edgeID ]
{-# INLINE obFeatsOn #-}


-- | Probability of the given EdgeIx. WARNING: returns 0 if the Ix is not in the
-- DAG (and we rely on this behavior)!
probOn :: DAG a (X, Y) -> EdgeIx -> L.LogFloat
probOn dag EdgeIx{..} =
  maybe 0 id $ snd <$> lbOn dag edgeID lbIx
{-# INLINE probOn #-}


-- | Transition features on a given position and with respect to given labels
-- (determined by indexes).
--
-- TODO: almost the same as `Feature.trFeatsOn`.
trFeatsOn
  :: DAG a (X, Y)
  -> Maybe EdgeIx -- ^ Current EdgeIx
  -> Maybe EdgeIx -- ^ Previous EdgeIx
  -> Maybe EdgeIx -- ^ One before the previous EdgeIx
  -> [C.Feat]
trFeatsOn dag u' v' w' = doit
  (lbOn' =<< u')
  (lbOn' =<< v')
  (lbOn' =<< w')
  where
    lbOn' EdgeIx{..} = fst <$> lbOn dag edgeID lbIx
    doit (Just u) (Just v) (Just w) = C.trFeats3 u v w
    doit (Just u) (Just v) _        = C.trFeats2 u v
    doit (Just u) _ _               = C.trFeats1 u
    doit _ _ _                      = []
{-# INLINE trFeatsOn #-}


----------------------------------------------------
-- Potential
----------------------------------------------------


-- | Observation potential on a given position and a given label (identified by
-- index), multiplied by the label's probability.
onWord :: Md.Model -> DAG a (X, Y) -> EdgeIx -> L.LogFloat
onWord crf dag ix
  = (probOn dag ix *)
  . L.product
  . map (Md.phi crf)
  . obFeatsOn dag
  $ ix
{-# INLINE onWord #-}


-- | Transition potential on a given position and a
-- given labels (identified by indexes).
onTransition
  :: Md.Model
  -> DAG a (X, Y)
  -> Maybe EdgeIx
  -> Maybe EdgeIx
  -> Maybe EdgeIx
  -> L.LogFloat
onTransition crf dag u w v
  = L.product
  . map (Md.phi crf)
  $ trFeatsOn dag u w v
{-# INLINE onTransition #-}


---------------------------------------------
-- A bit more complex stuff
---------------------------------------------


-- | Forward table computation.
forward :: AccF -> Md.Model -> DAG a (X, Y) -> ProbArray
forward acc crf dag =
  alpha
  where
    alpha = I.memoProbArray dag alpha'
    alpha' Beg Beg = 1.0
    alpha' End End = acc
      [ alpha End (Mid w)
        -- below, onTransition equals currently to 1; in general, there could be
        -- some related transition features, though.
        * onTransition crf dag Nothing Nothing (Just w)
      | w <- finalEdgeIxs dag ]
    alpha' u v = acc
      [ alpha v w * psi' u
        * onTransition crf dag (simplify u) (simplify v) (simplify w)
      | w <- complicate Beg <$> prevEdgeIxs dag (edgeID <$> simplify v) ]
    psi' u = case u of
      Mid x -> psi x
      _ -> 1.0
    psi = I.memoEdgeIx dag $ onWord crf dag


-- | Probability of the given DAG in the given model.
probability :: Md.Model -> DAG a (X, Y) -> L.LogFloat
probability crf dag =
  zxAlpha (forward L.sum crf dag) / normFactor
  where
    zxAlpha pa = pa End End
    normFactor = I.zx crf (fmap fst dag)


-- | Log-likelihood of the given dataset (no parallelization).
likelihood :: Md.Model -> [DAG a (X, Y)] -> L.LogFloat
likelihood crf = L.product . map (probability crf)


-- | Log-likelihood of the given dataset (parallelized version).
parLikelihood :: Md.Model -> [DAG a (X, Y)] -> L.LogFloat
parLikelihood crf dataset =
  let k = numCapabilities
      parts = partition k dataset
      probs = Par.parMap Par.rseq (likelihood crf) parts
  in  L.product probs
