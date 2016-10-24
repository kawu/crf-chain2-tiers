-- | Feature extraction module for DAG-aware CRFs.


module Data.CRF.Chain2.Tiers.DAG.Feature
(
-- * Feature
  Feat (..)

-- * Featre extraction
, presentFeats
) where


import qualified Data.Number.LogFloat as L

import           Data.DAG (DAG, EdgeID)
import qualified Data.DAG as DAG

import           Data.CRF.Chain2.Tiers.Core (X, Y, Feat)
import qualified Data.CRF.Chain2.Tiers.Core as C


----------------------------------------------------
-- Feature extraction (DAG)
----------------------------------------------------


-- | Observation features with probabilities for a given edge.
obFeats :: EdgeID -> DAG a (X, Y) -> [(Feat, L.LogFloat)]
obFeats edgeID dag =
  [ (ft, px)
  | let edgeLabel = DAG.edgeLabel edgeID dag
  , (x, px) <- C.unY (snd edgeLabel)
  , o       <- C.unX (fst edgeLabel)
  , ft      <- C.obFeats o x ]



-- | Zero-order transition features with probabilities for a given edge.
trFeats1 :: EdgeID -> DAG a (X, Y) -> [(Feat, L.LogFloat)]
trFeats1 i dag =
  [ (ft, px)
  | null (prevEdges i) -- TODO: see ticket on Trello
  , (x, px) <- edgeLabel i
  , ft <- C.trFeats1 x ]
  where
    edgeLabel = C.unY . snd . flip DAG.edgeLabel dag
    prevEdges = flip DAG.prevEdges dag


-- | First-order transition features with probabilities for a given edge.
trFeats2 :: EdgeID -> DAG a (X, Y) -> [(Feat, L.LogFloat)]
trFeats2 i dag =
  [ (ft, px * py)
  | (x, px) <- edgeLabel i
  , j <- prevEdges i
  , null (prevEdges j) -- TODO: see ticket on Trello
  , (y, py) <- edgeLabel j
  , ft <- C.trFeats2 x y ]
  where
    edgeLabel = C.unY . snd . flip DAG.edgeLabel dag
    prevEdges = flip DAG.prevEdges dag


-- | Second-order transition features with probabilities for a given edge.
trFeats3 :: EdgeID -> DAG a (X, Y) -> [(Feat, L.LogFloat)]
trFeats3 i dag =
  [ (ft, px * py * pz)
  | (x, px) <- edgeLabel i
  , j <- prevEdges i
  , (y, py) <- edgeLabel j
  , k <- prevEdges j
  , (z, pz) <- edgeLabel k
  , ft <- C.trFeats3 x y z ]
  where
    edgeLabel = C.unY . snd . flip DAG.edgeLabel dag
    prevEdges = flip DAG.prevEdges dag


-- | 'Feature's of all kinds which occur in the given dataset.
allFeats :: EdgeID -> DAG a (X, Y) -> [(Feat, L.LogFloat)]
allFeats edgeID dag
  =  obFeats  edgeID dag
  ++ trFeats1 edgeID dag
  ++ trFeats2 edgeID dag
  ++ trFeats3 edgeID dag


-- | 'Feature's of all kinds which occur in the given dataset.
presentFeats :: DAG a (X, Y) -> [(Feat, L.LogFloat)]
presentFeats dag = concat
  [ allFeats edgeID dag
  | edgeID <- DAG.dagEdges dag ]
