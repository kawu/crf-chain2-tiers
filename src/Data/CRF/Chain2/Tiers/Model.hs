{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}


module Data.CRF.Chain2.Tiers.Model
( Model (..)
, mkModel
, phi
, index
, onWord
, onTransition
) where


import           Control.Applicative ((<$>), (<*>))
import           Control.Monad (guard)
import           Data.Maybe (catMaybes)
import           Data.List (foldl1')
import           Data.Binary (Binary, get, put)
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import           Data.Vector.Binary ()
import qualified Data.Array.Unboxed as A
import           Data.Ix (Ix, inRange, range)
import qualified Data.Number.LogFloat as L

import           Data.CRF.Chain2.Tiers.Dataset.Internal
import           Data.CRF.Chain2.Tiers.Feature


-- | Dummy feature index.
dummy :: FeatIx
dummy = FeatIx (-1)
{-# INLINE dummy #-}


----------------------------------------------------
-- (Feature -> FeatIx) map
----------------------------------------------------


-- | Transition map restricted to a particular tagging layer.
type TransMap = A.UArray (Lb, Lb, Lb) FeatIx


-- | CRF feature map.
data FeatMap = FeatMap
    { transMaps	:: V.Vector TransMap
    , otherMap 	:: M.Map Feat FeatIx }


instance Binary FeatMap where
    put FeatMap{..} = put transMaps >> put otherMap
    get = FeatMap <$> get <*> get


-- | Get index of a feature.
featIndex :: Feat -> FeatMap -> Maybe FeatIx
featIndex (TFeat3 x y z k) (FeatMap v _) = do
    m  <- v V.!? k
    ix <- m !? (x, y, z)
    guard (ix /= dummy)
    return ix
featIndex x (FeatMap _ m) = M.lookup x m


-- | Make feature map from a *set* of (feature, index) pairs.
-- TODO: Take number of layers as argument -- it will
-- make computations faster.
mkFeatMap :: [(Feat, FeatIx)] -> FeatMap
mkFeatMap xs = FeatMap
    ( V.fromList
        [ mkArray . catMaybes $
            map (getTFeat3 k) xs
        | k <- [0 .. maxLayerNum xs] ] )
    (M.fromList (filter (isOther . fst) xs))
  where
    maxLayerNum = maximum . map (ln.fst)
    getTFeat3 i (TFeat3 x y z j, v)
        | i == j                = Just ((x, y, z), v)
        | otherwise             = Nothing
    getTFeat3 _ _               = Nothing
    isOther (TFeat3 _ _ _ _)    = False
    isOther _                   = True
    mkArray ys =
        let p = foldl1' updateMin (map fst ys)
            q = foldl1' updateMax (map fst ys)
            updateMin (!x, !y, !z) (x', y', z') =
                (min x x', min y y', min z z')
            updateMax (!x, !y, !z) (x', y', z') =
                (max x x', max y y', max z z')
            zeroed pq = A.array pq [(k, dummy) | k <- range pq]
        in  zeroed (p, q) A.// ys


(!?) :: (Ix i, A.IArray a b) => a i b -> i -> Maybe b
m !? x = if inRange (A.bounds m) x
    then Just (m A.! x)
    else Nothing
{-# INLINE (!?) #-}


----------------------------------------------------
-- Internal model
----------------------------------------------------


-- | Internal model data.
data Model = Model
    { values        :: U.Vector Double
    , ixMap         :: FeatMap }


instance Binary Model where
    put Model{..} = put values >> put ixMap
    get = Model <$> get <*> get


-- | Construct model from a dataset.
mkModel :: FeatSel -> [(Xs, Ys)] -> Model
mkModel ftSel dataset = Model
    { values    = U.replicate (S.size fs) 0.0 
    , ixMap     =
        let featIxs = map FeatIx [0..]
            featLst = S.toList fs
        in  mkFeatMap (zip featLst featIxs) }
  where
    fs = S.fromList $ concatMap (uncurry ftSel) dataset


-- | Potential assigned to the feature -- exponential of the
-- corresonding parameter.
phi :: Model -> Feat -> L.LogFloat
phi Model{..} ft = case featIndex ft ixMap of
    Just ix -> L.logToLogFloat (values U.! unFeatIx ix)
    Nothing -> L.logToLogFloat (0 :: Float)
{-# INLINE phi #-}


-- | Index of a feature.
index :: Model -> Feat -> Maybe FeatIx
index Model{..} ft = featIndex ft ixMap
{-# INLINE index #-}


-- | Observation potential on a given position and a
-- given label (identified by index).
onWord :: Model -> Xs -> Int -> CbIx -> L.LogFloat
onWord crf xs i u =
    product . map (phi crf) $ obFeatsOn xs i u
{-# INLINE onWord #-}


-- | Transition potential on a given position and a
-- given labels (identified by indexes).
onTransition :: Model -> Xs -> Int -> CbIx -> CbIx -> CbIx -> L.LogFloat
onTransition crf xs i u w v =
    product . map (phi crf) $ trFeatsOn xs i u w v
{-# INLINE onTransition #-}
