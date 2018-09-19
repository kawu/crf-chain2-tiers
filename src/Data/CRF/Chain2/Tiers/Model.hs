{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}


module Data.CRF.Chain2.Tiers.Model
( 
-- * Model
  Model (..)
, mkModel
, fromSet
, fromMap
, toMap

-- * Potential
, phi
, index
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


unT3Map :: Int -> T3Map -> [(Feat, FeatIx)]
unT3Map k t3 =
    [ (TFeat3 x y z k, ix)
    | ((x, y, z), ix) <- A.unArray t3
    , ix /= dummy ]


unT2Map :: Int -> T2Map -> [(Feat, FeatIx)]
unT2Map k t2 =
    [ (TFeat2 x y k, ix)
    | ((x, y), ix) <- A.unArray t2
    , ix /= dummy ]


unT1Map :: Int -> T1Map -> [(Feat, FeatIx)]
unT1Map k t1 =
    [ (TFeat1 x k, ix)
    | (x, ix) <- A.unArray t1
    , ix /= dummy ]


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
        | ob <- map mkOb [0 .. maxOb]
        , let lbMap = maybe M.empty id $ M.lookup ob ftMap ]

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

    -- Max observation
    maxOb = unOb . fst $ M.findMax ftMap


-- | Deconstruct observation feature map given the layer identifier.
unOMap :: Int -> OMap -> [(Feat, FeatIx)]
unOMap k OMap{..} =
    [ (OFeat o x k, i)
    | (o, (p, q)) <- zip
        (map mkOb [0..])
        (pairs . map fromIntegral $ U.toList oBeg)
    , (x, i)  <- zip
        (U.toList $ U.slice p (q - p) oLb)
        (U.toList $ U.slice p (q - p) oIx) ]
  where
    pairs xs = zip xs (tail xs)


-- | Feature map restricted to a particular layer.
data LayerMap = LayerMap
    { t1Map     :: !T1Map
    , t2Map     :: !T2Map
    , t3Map     :: !T3Map
    , obMap     :: !OMap }


instance Binary LayerMap where
    put LayerMap{..} = put t1Map >> put t2Map >> put t3Map >> put obMap
    get = LayerMap <$> get <*> get <*> get <*> get


-- | Deconstruct the layer map given the layer identifier.
unLayerMap :: Int -> LayerMap -> [(Feat, FeatIx)]
unLayerMap k LayerMap{..} = concat
    [ unT1Map k t1Map
    , unT2Map k t2Map
    , unT3Map k t3Map
    , unOMap  k obMap ]


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

    -- Number of layers (TODO: could be taken as mkFeatMap argument).
    maxLayerNum = maximum $ map (ln.fst) xs

    -- Check if feature is in a given layer.
    inLayer k x | ln x == k     = True
                | otherwise     = False


-- | Deconstruct the feature map.
unFeatMap :: FeatMap -> [(Feat, FeatIx)]
unFeatMap fm = concat
    [ unLayerMap i layer
    | (i, layer) <- zip [0..] (V.toList fm) ]


----------------------------------------------------
-- Internal model
----------------------------------------------------


-- | Internal model data.
data Model = Model
    { values        :: U.Vector Double
    , featMap       :: FeatMap }


instance Binary Model where
    put Model{..} = put values >> put featMap
    get = Model <$> get <*> get


-- | Construct model from a feature set.
-- All values will be set to 1 in log domain.
fromSet :: S.Set Feat -> Model
fromSet ftSet = Model
    { values    = U.replicate (S.size ftSet) 0.0
    , featMap   =
        let featIxs = map FeatIx [0..]
            featLst = S.toList ftSet
        in  mkFeatMap (zip featLst featIxs) }


-- | Construct model from a dataset given a feature selection function.
mkModel :: FeatSel -> [(Xs, Ys)] -> Model
mkModel ftSel = fromSet . S.fromList . concatMap (uncurry ftSel)


-- | Construct model from a (feature -> value) map.
fromMap :: M.Map Feat L.LogFloat -> Model
fromMap ftMap = Model
    { values    = U.fromList . map L.logFromLogFloat $ M.elems ftMap
    , featMap   =
        let featIxs = map FeatIx [0..]
            featLst = M.keys ftMap
        in  mkFeatMap (zip featLst featIxs) }


-- | Convert model to a (feature -> value) map.
toMap :: Model -> M.Map Feat L.LogFloat
toMap Model{..} = M.fromList
    [ (ft, L.logToLogFloat (values U.! unFeatIx ix))
    | (ft, ix) <- unFeatMap featMap ]


----------------------------------------------------
-- Potential
----------------------------------------------------


-- | Potential assigned to the feature -- exponential of the
-- corresonding parameter.
phi :: Model -> Feat -> L.LogFloat
phi Model{..} ft = case featIndex ft featMap of
    Just ix -> L.logToLogFloat (values U.! unFeatIx ix)
    Nothing -> L.logToLogFloat (0 :: Double)
{-# INLINE phi #-}


-- | Index of a feature.
index :: Model -> Feat -> Maybe FeatIx
index Model{..} ft = featIndex ft featMap
{-# INLINE index #-}
