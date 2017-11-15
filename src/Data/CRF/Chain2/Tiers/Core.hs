{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}


-- | Internal core data types.


module Data.CRF.Chain2.Tiers.Core
(
-- * Basic types
  Ob (..)
, mkOb, unOb
, Lb (..)
, mkLb, unLb
, FeatIx (..)
, mkFeatIx, unFeatIx
, CbIx

-- * Complex label
, Cb (..)
, mkCb
, unCb

-- * Input element (word)
, X (_unX, _unR)
, mkX
, unX
, unR
-- ** Indexing
, lbAt

-- * Output element (choice)
, Y (_unY)
, mkY
, unY

-- * Feature
, Feat (..)
-- ** Feature generation
, obFeats
, trFeats1
, trFeats2
, trFeats3
) where


import           Control.Applicative ((<$>), (<*>))
import           Control.Arrow (second)

import           Data.Binary (Binary, put, get, putWord8, getWord8)
import           Data.Ix (Ix)
import           Data.Int (Int16, Int32)
import           Data.List (zip4)
import qualified Data.Array.Unboxed as A
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import           Data.Vector.Unboxed.Deriving
import qualified Data.Vector.Generic.Base as G
import qualified Data.Vector.Generic.Mutable as G
import qualified Data.Number.LogFloat as L
-- import qualified Data.Primitive.ByteArray as BA

import           Data.CRF.Chain2.Tiers.Array (Bounds)

----------------------------------------------------------------
-- Basic types
----------------------------------------------------------------


-- | An observation.
newtype Ob = Ob { _unOb :: Int32 }
    deriving (Show, Eq, Ord, Binary)
--           GeneralizedNewtypeDeriving doesn't work for this in 7.8.2:
--           , G.Vector U.Vector, G.MVector U.MVector, U.Unbox )
derivingUnbox "Ob" [t| Ob -> Int32 |] [| _unOb |] [| Ob |]

-- | Smart observation constructor.
mkOb :: Int -> Ob
mkOb = Ob . fromIntegral
{-# INLINE mkOb #-}


-- | Deconstract observation.
unOb :: Ob -> Int
unOb = fromIntegral . _unOb
{-# INLINE unOb #-}


-- | An atomic label.
newtype Lb = Lb { _unLb :: Int16 }
    deriving (Show, Eq, Ord, Binary , Num, Ix, Bounds)
derivingUnbox "Lb" [t| Lb -> Int16 |] [| _unLb |] [| Lb |]


-- | Smart label constructor.
mkLb :: Int -> Lb
mkLb = Lb . fromIntegral
{-# INLINE mkLb #-}


-- | Deconstruct label.
unLb :: Lb -> Int
unLb = fromIntegral . _unLb
{-# INLINE unLb #-}


-- | An index of the label.
type CbIx = Int


-- | A feature index.  To every model feature a unique index is assigned.
newtype FeatIx = FeatIx { _unFeatIx :: Int32 }
    deriving (Show, Eq, Ord, Binary)
derivingUnbox "FeatIx" [t| FeatIx -> Int32 |] [| _unFeatIx |] [| FeatIx |]

-- | Smart feature index constructor.
mkFeatIx :: Int -> FeatIx
mkFeatIx = FeatIx . fromIntegral
{-# INLINE mkFeatIx #-}


-- | Deconstract feature index.
unFeatIx :: FeatIx -> Int
unFeatIx = fromIntegral . _unFeatIx
{-# INLINE unFeatIx #-}


----------------------------------------------------------------
-- Complex label
----------------------------------------------------------------


-- TODO: Do we gain anything by representing the
-- complex label with a byte array?  Complex labels
-- should not be directly stored in a model, so if
-- there is something to gain here, its not obvious.
--
-- Perhaps a list representation would be sufficient?


-- -- | A complex label is an array of atomic labels.
-- newtype Cb = Cb { unCb :: BA.ByteArray }


-- | A complex label is a vector of atomic labels.
newtype Cb = Cb { _unCb :: U.Vector Lb }
    deriving (Show, Eq, Ord, Binary)


-- | Smart complex label constructor.
mkCb :: [Lb] -> Cb
mkCb = Cb . U.fromList


-- | Deconstract complex label.
unCb :: Cb -> [Lb]
unCb = U.toList . _unCb


----------------------------------------------------------------
-- Internal dataset representation
----------------------------------------------------------------


-- | A word is represented by a list of its observations
-- and a list of its potential label interpretations.
data X = X {
    -- | A set of observations.
      _unX :: U.Vector Ob
    -- | A vector of potential labels.
    , _unR :: V.Vector Cb }
    deriving (Show, Eq, Ord)


instance Binary X where
    put X{..} = put _unX >> put _unR
    get = X <$> get <*> get


-- | Smart `X` constructor.
mkX :: [Ob] -> [Cb] -> X
mkX x r = X (U.fromList x) (V.fromList r)
{-# INLINE mkX #-}


-- | List of observations.
unX :: X -> [Ob]
unX = U.toList . _unX
{-# INLINE unX #-}


-- | List of potential labels.
unR :: X -> [Cb]
unR = V.toList . _unR
{-# INLINE unR #-}


-- | Potential label at the given position.
lbAt :: X -> CbIx -> Cb
lbAt x = (_unR x V.!)
{-# INLINE lbAt #-}


-- | Vector of chosen labels together with
-- corresponding probabilities in log domain.
newtype Y = Y { _unY :: V.Vector (Cb, Double) }
    deriving (Show, Eq, Ord, Binary)


-- | Y constructor.
mkY :: [(Cb, Double)] -> Y
mkY = Y . V.fromList . map (second log)
{-# INLINE mkY #-}


-- | Y deconstructor symetric to mkY.
unY :: Y -> [(Cb, L.LogFloat)]
unY = map (second L.logToLogFloat) . V.toList . _unY
{-# INLINE unY #-}


----------------------------------------------------------------
-- Feature
----------------------------------------------------------------


-- | Feature; every feature is associated to a layer with `ln` identifier.
data Feat
    -- | Second-order transition feature.
    = TFeat3
        { x1    :: {-# UNPACK #-} !Lb
        , x2    :: {-# UNPACK #-} !Lb
        , x3    :: {-# UNPACK #-} !Lb
        , ln    :: {-# UNPACK #-} !Int }
    -- | First-order transition feature.
    | TFeat2
        { x1    :: {-# UNPACK #-} !Lb
        , x2    :: {-# UNPACK #-} !Lb
        , ln    :: {-# UNPACK #-} !Int }
    -- | Zero-order transition feature.
    | TFeat1
        { x1    :: {-# UNPACK #-} !Lb
        , ln    :: {-# UNPACK #-} !Int }
    -- | Observation feature.
    | OFeat
        { ob    :: {-# UNPACK #-} !Ob
        , x1    :: {-# UNPACK #-} !Lb
        , ln    :: {-# UNPACK #-} !Int }
    deriving (Show, Eq, Ord)


instance Binary Feat where
    put (OFeat o x k)       = putWord8 0 >> put o >> put x >> put k
    put (TFeat3 x y z k)    = putWord8 1 >> put x >> put y >> put z >> put k
    put (TFeat2 x y k)      = putWord8 2 >> put x >> put y >> put k
    put (TFeat1 x k)        = putWord8 3 >> put x >> put k
    get = getWord8 >>= \i -> case i of
        0   -> OFeat  <$> get <*> get <*> get
        1   -> TFeat3 <$> get <*> get <*> get <*> get
        2   -> TFeat2 <$> get <*> get <*> get
        3   -> TFeat1 <$> get <*> get
        _   -> error "get feature: unknown code"


----------------------------------------------------
-- Features generation
----------------------------------------------------


-- | Generate observation features.
obFeats :: Ob -> Cb -> [Feat]
obFeats ob' xs =
    [ OFeat ob' x k
    | (x, k) <- zip (unCb xs) [0..] ]


-- | Generate zero-order transition features.
trFeats1 :: Cb -> [Feat]
trFeats1 xs =
    [ TFeat1 x k
    | (x, k) <- zip (unCb xs) [0..] ]


-- | Generate first-order transition features.
trFeats2 :: Cb -> Cb -> [Feat]
trFeats2 xs1 xs2 =
    [ TFeat2 x1' x2' k
    | (x1', x2', k) <- zip3 (unCb xs1) (unCb xs2) [0..] ]


-- | Generate second-order transition features.
trFeats3 :: Cb -> Cb -> Cb -> [Feat]
trFeats3 xs1 xs2 xs3 =
    [ TFeat3 x1' x2' x3' k
    | (x1', x2', x3', k) <- zip4 (unCb xs1) (unCb xs2) (unCb xs3) [0..] ]
