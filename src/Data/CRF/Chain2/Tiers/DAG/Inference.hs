{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Rank2Types #-}


module Data.CRF.Chain2.Tiers.DAG.Inference
( tag
, tag'
, tagK
, marginals
, marginals'
, ProbType (..)
, probs
, probs'
, accuracy
, expectedFeaturesIn
, zx
, zx'

-- * Internals (used by `Probs`) (TODO: move elsewhere)
, AccF
, ProbArray
, Pos (..)
, simplify
, complicate
-- ** Memoization
, memoProbArray
, memoEdgeIx
) where


import           GHC.Conc (numCapabilities)

import           Control.Applicative ((<$>))
import qualified Control.Parallel as Par
import qualified Control.Parallel.Strategies as Par

import qualified Data.Number.LogFloat as L
import qualified Data.Vector as V
import qualified Data.Array as A
import qualified Data.Set as S
import           Data.Maybe (fromJust)
import qualified Data.MemoCombinators as Memo
import qualified Data.List as List
import           Data.Function (on)
import qualified Data.Foldable as F

import           Data.DAG (EdgeID, DAG)
import qualified Data.DAG as DAG

import qualified Data.CRF.Chain2.Tiers.Core as C
import           Data.CRF.Chain2.Tiers.Core (X, Y, Cb, CbIx)
import qualified Data.CRF.Chain2.Tiers.Model as Md
import           Data.CRF.Chain2.Tiers.Util (partition)
import           Data.CRF.Chain2.Tiers.DAG.Feature (EdgeIx(..))
import qualified Data.CRF.Chain2.Tiers.DAG.Feature as Ft


---------------------------------------------
-- Util Types
---------------------------------------------


-- | Accumulation function.
type AccF = [L.LogFloat] -> L.LogFloat


-- | Position in the sentence.
data Pos
  = Beg        -- ^ Before the beginning of the sentence
  | Mid EdgeIx -- ^ Actual edge
  | End        -- ^ After the end of the sentence
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


memoProbArray :: DAG a b -> ProbArray -> ProbArray
memoProbArray dag =
  let memo = memoPos dag
  in  Memo.memo2 memo memo


memoPos :: DAG a b -> Memo.Memo Pos
memoPos dag f =
  table (f Beg) (memo (f . Mid)) (f End)
  where
    memo = memoEdgeIx dag
    table b m e Beg = b
    table b m e (Mid x) = m x
    table b m e End = e


memoEdgeIx :: DAG a b -> Memo.Memo EdgeIx
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
  = L.product
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
  = L.product
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
zx crf = zxAlpha . forward L.sum crf

-- | Normalization factor based on the forward table.
zxAlpha :: ProbArray -> L.LogFloat
zxAlpha pa = pa End End


-- | Normalization factor computed for the sentence using the backward
-- computation.
zx' :: Md.Model -> DAG a X -> L.LogFloat
zx' crf = zxBeta . backward L.sum crf

-- | Normalization factor based on the backward table.
zxBeta :: ProbArray -> L.LogFloat
zxBeta pa = pa Beg Beg


-- -- | Probability of chosing the given three edges and the corresponding labels.
-- edgeProb3
--   :: Md.Model
--   -- ^ The underlying model
--   -> DAG a X
--   -- ^ The underlying DAG
--   -> ProbArray
--   -- ^ Forward probability table
--   -> ProbArray
--   -- ^ Backward probability table
--   -> Int
--   -- ^ Sentence position
--   -> (CbIx -> L.LogFloat)
--   -- ^ Memoized psi (onWord)
--   -> CbIx
--   -- ^ Corresponding to the current position `i`
--   -> CbIx
--   -- ^ Corresponding to the position `i - 1`
--   -> CbIx
--   -- ^ Corresponding to the position `i - 2`
--   -> L.LogFloat
-- edgeProb3 crf sent alpha beta k psiMem x y z =
--     alpha (k - 1) y z * beta (k + 1) x y * psiMem x
--     * onTransition crf sent k x y z / zxBeta beta


-- | Probability of chosing the given three edges and the corresponding labels.
edgeProb3
  :: Md.Model
  -- ^ The underlying model
  -> DAG a X
  -- ^ The underlying DAG
  -> (EdgeIx -> L.LogFloat)
  -- ^ Memoized psi (onWord)
  -> ProbArray
  -- ^ Forward probability table
  -> ProbArray
  -- ^ Backward probability table
  -> EdgeIx
  -- ^ Current edge and the corresponding label
  -> Maybe EdgeIx
  -- ^ Previous edge and the corresponding label
  -> Maybe EdgeIx
  -- ^ One before the previous edge and the corresponding label
  -> L.LogFloat
-- edgeProb3 crf dag psi alpha beta x y z =
edgeProb3 crf dag psi alpha beta u0 v0 w0
  = alpha v w
  * beta u v
  * psi u0
  * onTransition crf dag (Just u0) v0 w0
  / zxBeta beta
  where
   u = Mid u0
   v = complicate Beg v0
   w = complicate Beg w0


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
  :: AccF
  -- ^ Accumulating function (should be the same as the one used to
  -- compute forward and backward tables)
  -> DAG a X
  -- ^ The underlying sentence DAG
  -> ProbArray
  -- ^ Forward probability table
  -> ProbArray
  -- ^ Backward probability table
  -> EdgeIx
  -- ^ Edge and the corresponding label
  -> L.LogFloat
edgeProb1 acc dag alpha beta u = acc -- sum
  [ edgeProb2 alpha beta u v
  | v <- Ft.prevEdgeIxs dag (Just $ edgeID u) ]


-- | Tag potential labels with marginal probabilities.
marginals :: Md.Model -> DAG a X -> DAG a [(CbIx, L.LogFloat)]
marginals crf dag =
  DAG.mapE label dag
  where
    label edgeID _ =
      [ (Ft.lbIx edgeIx, prob1 edgeIx)
      | edgeIx <- Ft.edgeIxs dag edgeID ]
    prob1 = edgeProb1 L.sum dag alpha beta
    alpha = forward L.sum crf dag
    beta = backward L.sum crf dag


-- | Tag potential labels with marginal probabilities.
marginals' :: Md.Model -> DAG a X -> DAG a [(Cb, L.LogFloat)]
marginals' crf dag = mergeProbs dag (marginals crf dag)


-- -- | Tag potential labels with alternative probabilities.
-- -- TODO: explain what is that exactly.
-- probs :: Md.Model -> DAG a X -> DAG a [(CbIx, L.LogFloat)]
-- probs crf dag =
--   DAG.mapE label dag
--   where
--     label edgeID _ =
--       [ (Ft.lbIx edgeIx, prob1 edgeIx)
--       | edgeIx <- Ft.edgeIxs dag edgeID ]
--     prob1 = edgeProb1 maximum dag alpha beta
--     alpha = forward maximum crf dag
--     beta = backward maximum crf dag


-- | Type of resulting probabilities.
data ProbType
  = Marginals
  -- ^ Marginal probabilities
  | MaxProbs
  -- ^ TODO


-- | Tag potential labels with alternative probabilities.
-- TODO: explain what is that exactly.
probs :: ProbType -> Md.Model -> DAG a X -> DAG a [(CbIx, L.LogFloat)]
probs probTyp crf dag =
  DAG.mapE label dag
  where
    label edgeID _ =
      [ (Ft.lbIx edgeIx, prob1 edgeIx)
      | edgeIx <- Ft.edgeIxs dag edgeID ]
    prob1 = edgeProb1 acc dag alpha beta
    alpha = forward acc crf dag
    beta = backward acc crf dag
    acc = case probTyp of
      Marginals -> L.sum
      MaxProbs  -> maximum


-- | Tag potential labels with alternative probabilities.
-- TODO: explain what is that exactly.
probs' :: ProbType -> Md.Model -> DAG a X -> DAG a [(Cb, L.LogFloat)]
probs' typ crf dag = mergeProbs dag (probs typ crf dag)


-- | Utility function useful for `margilans'` and `probs'`.
mergeProbs :: DAG a X -> DAG a [(CbIx, L.LogFloat)] -> DAG a [(Cb, L.LogFloat)]
mergeProbs dag
  = fmap lbAt
  . DAG.zipE dag
  where
    lbAt (x, ys) =
      [ (C.lbAt x cbIx, pr)
      | (cbIx, pr) <- ys ]


-- | Get (at most) k best tags for each word and return them in
-- descending order.  TODO: Tagging with respect to marginal
-- distributions might not be the best idea.  Think of some
-- more elegant method.
tagK :: Int -> Md.Model -> DAG a X -> DAG a [(CbIx, L.LogFloat)]
tagK k crf dag = fmap
    ( take k
    . reverse
    . List.sortBy (compare `on` snd)
    ) (marginals crf dag)


-- | Find the most probable label sequence (with probabilities of individual
-- lables determined with respect to marginal distributions) satisfying the
-- constraints imposed over label values.
tag :: Md.Model -> DAG a X -> DAG a CbIx
tag crf = fmap (fst . head) . tagK 1 crf


-- | Similar to `tag` but directly returns complex labels and not just their
-- `CbIx` indexes.
tag' :: Md.Model -> DAG a X -> DAG a Cb
tag' crf dag
  = fmap (uncurry C.lbAt)
  $ DAG.zipE dag (tag crf dag)


expectedFeaturesOn
  :: Md.Model
  -- ^ CRF model
  -> DAG a X
  -- ^ The underlying sentence DAG
  -> ProbArray
  -- ^ Forward computation table
  -> ProbArray
  -- ^ Backward computation table
  -> EdgeID
  -- ^ ID of an edge of the underlying DAG
  -> [(C.Feat, L.LogFloat)]
expectedFeaturesOn crf dag alpha beta edgeID =
  fs1 ++ fs3
  where
    psi = memoEdgeIx dag $ onWord crf dag
    prob1 = edgeProb1 L.sum dag alpha beta
    prob3 = edgeProb3 crf dag psi alpha beta

    fs1 =
      [ (ft, prob)
      | edgeIx <- Ft.edgeIxs dag edgeID
      , let prob = prob1 edgeIx
      , ft <- Ft.obFeatsOn dag edgeIx ]

    fs3 =
      [ (ft, prob)
      | u <- Just <$> Ft.edgeIxs dag edgeID
      , v <- Ft.prevEdgeIxs dag (Ft.edgeID <$> u)
      , w <- Ft.prevEdgeIxs dag (Ft.edgeID <$> v)
      , let prob = prob3 (fromJust u) v w
      , ft <- Ft.trFeatsOn dag u v w ]


-- | A list of features defined within the context of the sentence accompanied
-- by expected probabilities determined on the basis of the model.
--
-- One feature can occur multiple times in the output list.
expectedFeaturesIn
  :: Md.Model
  -> DAG a X
  -> [(C.Feat, L.LogFloat)]
expectedFeaturesIn crf dag = zxF `Par.par` zxB `Par.pseq` zxF `Par.pseq`
    concat [expectedOn edgeID | edgeID <- DAG.dagEdges dag]
  where
    expectedOn = expectedFeaturesOn crf dag alpha beta
    alpha = forward L.sum crf dag
    beta = backward L.sum crf dag
    zxF = zxAlpha alpha
    zxB = zxBeta beta


goodAndBad :: Md.Model -> DAG a (X, Y) -> (Int, Int)
goodAndBad crf dag =

    F.foldl' gather (0, 0) $ DAG.zipE labels labels'

  where

    gather (good, bad) results =
      if consistent results
      then (good + 1, bad)
      else (good, bad + 1)

    consistent results = case results of
      (Just xs, Just ys) -> (not . S.null) (S.intersection xs ys)
      (Nothing, Nothing) -> True
      _ -> False

    labels' = fmap best $ probs' MaxProbs crf (fmap fst dag)
    labels  = fmap (best . C.unY)    (fmap snd dag)

    best zs
      | null zs   = Nothing
      | otherwise =
          let maxProb = maximum (map snd zs)
          in  if maxProb < eps
              then Nothing
              else Just
                   . S.fromList . map fst
                   . filter ((>= maxProb - eps) . snd)
                   $ zs
    eps = 1.0e-9



goodAndBad' :: Md.Model -> [DAG a (X, Y)] -> (Int, Int)
goodAndBad' crf dataset =
    let add (g, b) (g', b') = (g + g', b + b')
    in  F.foldl' add (0, 0) [goodAndBad crf x | x <- dataset]


-- | Compute the accuracy of the model with respect to the labeled dataset.
accuracy :: Md.Model -> [DAG a (X, Y)] -> Double
accuracy crf dataset =
    let k = numCapabilities
    	parts = partition k dataset
        xs = Par.parMap Par.rseq (goodAndBad' crf) parts
        (good, bad) = F.foldl' add (0, 0) xs
        add (g, b) (g', b') = (g + g', b + b')
    in  fromIntegral good / fromIntegral (good + bad)
