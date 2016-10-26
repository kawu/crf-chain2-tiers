{-# LANGUAGE RecordWildCards #-}


-- | Feature extraction module for DAG-aware CRFs.


module Data.CRF.Chain2.Tiers.DAG.Feature
(
-- * Feature
  Feat (..)

-- * Featre extraction
-- ** Present features
, presentFeats
-- ** Hidden features
, EdgeIx (..)
, hiddenFeats
, obFeatsOn
, trFeatsOn

-- * Feature selection
, FeatSel
, selectPresent
, selectHidden

-- * Indexing
, lbNum
, lbIxs
, edgeIxs
, prevEdgeIxs
, nextEdgeIxs
, initialEdgeIxs
, finalEdgeIxs
) where


import           Control.Applicative ((<$>))

import qualified Data.Number.LogFloat as L
import qualified Data.Vector as V
import           Data.Maybe (maybeToList)

import           Data.DAG (DAG, EdgeID)
import qualified Data.DAG as DAG

import           Data.CRF.Chain2.Tiers.Core (X, Y, Ob, Cb, CbIx, Feat)
import qualified Data.CRF.Chain2.Tiers.Core as C


----------------------------------------------------
-- Present features
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


-- | Present 'Feat'ures of all kinds occurring w.r.t. to the given edge.
presentFeatsOn :: EdgeID -> DAG a (X, Y) -> [(Feat, L.LogFloat)]
presentFeatsOn edgeID dag
  =  obFeats  edgeID dag
  ++ trFeats1 edgeID dag
  ++ trFeats2 edgeID dag
  ++ trFeats3 edgeID dag


-- | Present 'Feat'ures of all kinds occurring in the given DAG.
presentFeats :: DAG a (X, Y) -> [(Feat, L.LogFloat)]
presentFeats dag = concat
  [ presentFeatsOn edgeID dag
  | edgeID <- DAG.dagEdges dag ]


---------------------------------------------
-- Indexing
---------------------------------------------


-- | List of observations on the given edge of the sentence.
obList :: DAG a X -> EdgeID -> [Ob]
obList dag i = C.unX $ DAG.edgeLabel i dag
{-# INLINE obList #-}


-- | Vector of potential labels on the given edge of the sentence.
lbVec :: DAG a X -> EdgeID -> V.Vector Cb
lbVec dag i = C._unR $ DAG.edgeLabel i dag
{-# INLINE lbVec #-}


-- | Number of potential labels at the given position of the sentence.
lbNum :: DAG a X -> EdgeID -> Int
lbNum dag = V.length . lbVec dag
{-# INLINE lbNum #-}


-- | Potential label at the given position and at the given index.
lbOn :: DAG a X -> EdgeID -> CbIx -> Maybe Cb
lbOn dag = (V.!?) . lbVec dag
{-# INLINE lbOn #-}


-- | List of label indices at the given edge.
lbIxs :: DAG a X -> EdgeID -> [CbIx]
lbIxs dag i = [0 .. lbNum dag i - 1]
{-# INLINE lbIxs #-}


-- | The list of EdgeIx's corresponding to the given edge.
edgeIxs :: DAG a X -> EdgeID -> [EdgeIx]
edgeIxs dag i =
  [ EdgeIx {edgeID=i, lbIx=u}
  | u <- lbIxs dag i ]


-- | The list of EdgeIx's corresponding to the previous edges.
-- If the argument edgeID is `Nothing` or if the list of previous
-- edges is empty, the result will be a singleton list containing
-- `Nothing` (which represents a special out-of-bounds EdgeIx).
prevEdgeIxs :: DAG a X -> Maybe EdgeID -> [Maybe EdgeIx]
prevEdgeIxs _ Nothing = [Nothing]
prevEdgeIxs dag (Just i)
  | null js = [Nothing]
  | otherwise = Just <$>
    [ EdgeIx {edgeID=j, lbIx=u}
    | j <- js, u <- lbIxs dag j ]
  where js = DAG.prevEdges i dag


-- | Similar to `prevEdgeIxs` but returns the succeeding edges.
nextEdgeIxs :: DAG a X -> Maybe EdgeID -> [Maybe EdgeIx]
nextEdgeIxs _ Nothing = [Nothing]
nextEdgeIxs dag (Just i)
  | null js = [Nothing]
  | otherwise = Just <$>
    [ EdgeIx {edgeID=j, lbIx=u}
    | j <- js, u <- lbIxs dag j ]
  where js = DAG.nextEdges i dag


-- | Obtain the list of initial EdgeIxs.
initialEdgeIxs :: DAG a X -> [EdgeIx]
initialEdgeIxs dag = concat
  [ edgeIxs dag i
  | i <- DAG.dagEdges dag
  , DAG.isInitialEdge i dag ]


-- | Obtain the list of final EdgeIxs.
finalEdgeIxs :: DAG a X -> [EdgeIx]
finalEdgeIxs dag = concat
  [ edgeIxs dag i
  | i <- DAG.dagEdges dag
  , DAG.isFinalEdge i dag ]


----------------------------------------------------
-- Hidden features
----------------------------------------------------


-- | Edge with the corresponding label index.
data EdgeIx = EdgeIx
  { edgeID :: {-# UNPACK #-} !EdgeID
    -- ^ ID of an edge
  , lbIx   :: {-# UNPACK #-} !CbIx
    -- ^ Index of the corresponding complex label
  }
  deriving (Show, Eq, Ord)


-- | Observation features on a given position and with respect
-- to a given label (determined by index).
obFeatsOn :: DAG a X -> EdgeIx -> [Feat]
obFeatsOn dag EdgeIx{..} = concat
  [ C.obFeats o e
  | e <- maybeToList $ lbOn dag edgeID lbIx
  , o <- obList dag edgeID ]
-- obFeatsOn :: DAG a X -> EdgeID -> CbIx -> [Feat]
-- obFeatsOn dag edgeID lbIx = concat
--   [ C.obFeats o e
--   | e <- maybeToList $ lbOn dag edgeID lbIx
--   , o <- obList dag edgeID ]
{-# INLINE obFeatsOn #-}


-- | Transition features on a given position and with respect
-- to given labels (determined by indexes).
trFeatsOn
  :: DAG a X
  -> Maybe EdgeIx -- ^ Current EdgeIx
  -> Maybe EdgeIx -- ^ Previous EdgeIx
  -> Maybe EdgeIx -- ^ One before the previous EdgeIx
  -> [Feat]
trFeatsOn dag u' v' w' = doit
  (lbOn' =<< u')
  (lbOn' =<< v')
  (lbOn' =<< w')
  where
    lbOn' EdgeIx{..} = lbOn dag edgeID lbIx
    doit (Just u) (Just v) (Just w) = C.trFeats3 u v w
    doit (Just u) (Just v) _        = C.trFeats2 u v
    doit (Just u) _ _               = C.trFeats1 u
    doit _ _ _                      = []
{-# INLINE trFeatsOn #-}


-- | Features hidden in the dataset element.
hiddenFeats :: DAG a X -> [Feat]
hiddenFeats dag =
  obFs ++ trFs
  where
    obFs = concat
      [ obFeatsOn dag u
      | i <- DAG.dagEdges dag
      , u <- edgeIxs dag i ]
    trFs = concat
      [ trFeatsOn dag u v w
      | i <- DAG.dagEdges dag
      , u <- Just <$> edgeIxs dag i
      , v <- prevEdgeIxs dag (edgeID <$> u)
      , w <- prevEdgeIxs dag (edgeID <$> v) ]


----------------------------------------------------
-- Feature selection
----------------------------------------------------


-- | A feature selection function type.
type FeatSel a = DAG a (X, Y) -> [Feat]


-- | The 'presentFeats' adapted to fit feature selection specs.
selectPresent :: FeatSel a
selectPresent = map fst . presentFeats


-- | The 'hiddenFeats' adapted to fit feature selection specs.
selectHidden :: FeatSel a
selectHidden = hiddenFeats . fmap fst
