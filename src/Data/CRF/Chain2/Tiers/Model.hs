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
import           Data.Binary (Binary, get, put)
import           Data.Int (Int32)
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import           Data.Vector.Binary ()
import qualified Data.Number.LogFloat as L

import           Data.CRF.Chain2.Tiers.Dataset.Internal
import           Data.CRF.Chain2.Tiers.Feature
import qualified Data.CRF.Chain2.Tiers.Array as A


-- | Dummy feature index.
dummy :: FeatIx
dummy = FeatIx (-1)
{-# INLINE dummy #-}


----------------------------------------------------
-- (Feature -> FeatIx) map
----------------------------------------------------


-- | Transition map restricted to a particular tagging layer.
-- Transitions of x form.
type T1Map = A.Array Lb FeatIx


-- | Transition map restricted to a particular tagging layer.
-- Transitions of (x, y) form.
type T2Map = A.Array (Lb, Lb) FeatIx


-- | Transition map restricted to a particular tagging layer.
-- Transitions of (x, y, z) form.
type T3Map = A.Array (Lb, Lb, Lb) FeatIx


mkT3Map :: [(Feat, FeatIx)] -> T3Map
mkT3Map xs =
    let ys = [((x, y, z), ix) | (TFeat3 x y z _, ix) <- xs]
    in  A.mkArray dummy ys


mkT2Map :: [(Feat, FeatIx)] -> T2Map
mkT2Map xs =
    let ys = [((x, y), ix) | (TFeat2 x y _, ix) <- xs]
    in  A.mkArray dummy ys


mkT1Map :: [(Feat, FeatIx)] -> T1Map
mkT1Map xs =
    let ys = [(x, ix) | (TFeat1 x _, ix) <- xs]
    in  A.mkArray dummy ys


-- | Observation map restricted to a particular tagging layer.
data OMap = OMap {
    -- | Where memory ranges related to
    -- individual observations begin?
      oBeg  :: U.Vector Int32
    -- | Labels related to individual observations.
    , oLb   :: U.Vector Lb
    -- | Feature indices related to individual (Ob, Lb) pairs.
    , oIx   :: U.Vector FeatIx }


instance Binary OMap where
    put OMap{..} = put oBeg >> put oLb >> put oIx
    get = OMap <$> get <*> get <*> get


mkOMap :: [(Feat, FeatIx)] -> OMap
mkOMap xs = OMap

    { oBeg = U.fromList $ scanl (+) 0
        [ fromIntegral (M.size lbMap)
        | ((ob, lbMap), i) <- zip (M.toAscList ftMap) [0..]
        -- We check, if the set of keys (observations) is
        -- equal to {0, 1, .., obNum-1}.
        -- TODO: We don't really have to care if the condition is satisfied.
        -- We can use dummy FeatIx values.
        , if ob == Ob i
            then True
            else error "mkOMap: ob /= i" ]

    , oLb = U.fromList . concat $
        [ M.keys lbMap
        | lbMap <- M.elems ftMap ]

    , oIx = U.fromList . concat $
        [ M.elems lbMap
        | lbMap <- M.elems ftMap ] }

  where

    -- A feature map of type Map Ob (Map Lb FeatIx).
    ftMap = fmap M.fromList $ M.fromListWith (++)
        [ (ob, [(x, ix)])
        | (OFeat ob x _, ix) <- xs ]


-- | Feature map restricted to a particular layer.
data LayerMap = LayerMap
    { t1Map     :: !T1Map
    , t2Map     :: !T2Map
    , t3Map     :: !T3Map
    , obMap     :: !OMap }


instance Binary LayerMap where
    put LayerMap{..} = put t1Map >> put t2Map >> put t3Map >> put obMap
    get = LayerMap <$> get <*> get <*> get <*> get


-- | Feature map is a vectro of layer maps.
type FeatMap = V.Vector LayerMap


-- | Get index of a feature.
featIndex :: Feat -> FeatMap -> Maybe FeatIx
featIndex (TFeat3 x y z k) v = do
    m  <- t3Map <$> (v V.!? k)
    ix <- m A.!? (x, y, z)
    guard (ix /= dummy)
    return ix
featIndex (TFeat2 x y k) v = do
    m  <- t2Map <$> (v V.!? k)
    ix <- m A.!? (x, y)
    guard (ix /= dummy)
    return ix
featIndex (TFeat1 x k) v = do
    m  <- t1Map <$> (v V.!? k)
    ix <- m A.!? x
    guard (ix /= dummy)
    return ix
featIndex (OFeat ob x k) v = do
    OMap{..} <- obMap <$> (v V.!? k)
    p  <- fromIntegral <$> oBeg U.!? (unOb ob)
    q  <- fromIntegral <$> oBeg U.!? (unOb ob + 1)
    i  <- U.findIndex (==x) (U.slice p (q - p) oLb)
    ix <- oIx U.!? (p + i)
    -- guard (ix /= dummy)
    return ix


-- | Make feature map from a *set* of (feature, index) pairs.
mkFeatMap :: [(Feat, FeatIx)] -> FeatMap
mkFeatMap xs = V.fromList

    -- TODO: We can first divide features between individual layers.
    [ mkLayerMap $ filter (inLayer k . fst) xs
    | k <- [0 .. maxLayerNum] ]

  where

    -- Make layer map.
    mkLayerMap = LayerMap
        <$> mkT1Map
        <*> mkT2Map
        <*> mkT3Map
        <*> mkOMap

    -- Number of layers (TODO: should be taken as mkFeatMap argument).
    maxLayerNum = maximum $ map (ln.fst) xs

    -- Check if feature is in a given layer.
    inLayer k x | ln x == k     = True
                | otherwise     = False


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
