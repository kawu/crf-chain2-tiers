{-# LANGUAGE GeneralizedNewtypeDeriving #-}


-- | Internal core data types.


module Data.CRF.Chain2.Tiers.Dataset.Internal
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
, Xs
, mkX
, unX
, unR

-- * Output element (choice)
, Y (_unY)
, Ys
, mkY
, unY

-- * Indexing
, lbAt
, lbOn
, lbNum
, lbIxs

-- * AVec
, AVec (unAVec)
, mkAVec
, AVec2 (unAVec2)
, mkAVec2
) where


import           Data.Binary (Binary)
import           Data.Ix (Ix)
import           Control.Arrow (second)
import           Data.Int (Int16, Int32)
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Array.Unboxed as A
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
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
    deriving ( Show, Eq, Ord, Binary, A.IArray A.UArray
             , G.Vector U.Vector, G.MVector U.MVector, U.Unbox )


mkOb :: Int -> Ob
mkOb = Ob . fromIntegral
{-# INLINE mkOb #-}


unOb :: Ob -> Int
unOb = fromIntegral . _unOb
{-# INLINE unOb #-}


-- | An atomic label.
newtype Lb = Lb { _unLb :: Int16 }
    deriving ( Show, Eq, Ord, Binary, A.IArray A.UArray
             , G.Vector U.Vector, G.MVector U.MVector, U.Unbox
             , Num, Ix, Bounds)


mkLb :: Int -> Lb
mkLb = Lb . fromIntegral
{-# INLINE mkLb #-}


unLb :: Lb -> Int
unLb = fromIntegral . _unLb
{-# INLINE unLb #-}


-- | An index of the label.
type CbIx = Int


-- | A feature index.  To every model feature a unique index is assigned.
newtype FeatIx = FeatIx { _unFeatIx :: Int32 }
    deriving ( Show, Eq, Ord, Binary, A.IArray A.UArray
             , G.Vector U.Vector, G.MVector U.MVector, U.Unbox )


mkFeatIx :: Int -> FeatIx
mkFeatIx = FeatIx . fromIntegral
{-# INLINE mkFeatIx #-}


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
    deriving (Show, Eq, Ord)


mkCb :: [Lb] -> Cb
mkCb = Cb . U.fromList


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


-- | Sentence of words.
type Xs = V.Vector X


-- | X constructor.
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


-- | Vector of chosen labels together with
-- corresponding probabilities in log domain.
newtype Y = Y { _unY :: V.Vector (Cb, Double) }
    deriving (Show, Eq, Ord)


-- | Y constructor.
mkY :: [(Cb, Double)] -> Y
mkY = Y . V.fromList . map (second log)
{-# INLINE mkY #-}


-- | Y deconstructor symetric to mkY.
unY :: Y -> [(Cb, L.LogFloat)]
unY = map (second L.logToLogFloat) . V.toList . _unY
{-# INLINE unY #-}


-- | Sentence of Y (label choices).
type Ys = V.Vector Y


-- | Potential label at the given position.
lbAt :: X -> CbIx -> Cb
lbAt x = (_unR x V.!)
{-# INLINE lbAt #-}


lbVec :: Xs -> Int -> V.Vector Cb
lbVec xs = _unR . (xs V.!)
{-# INLINE lbVec #-}


-- | Number of potential labels at the given position of the sentence.
lbNumI :: Xs -> Int -> Int
lbNumI xs = V.length . lbVec xs
{-# INLINE lbNumI #-}


-- | Potential label at the given position and at the given index.
lbOnI :: Xs -> Int -> CbIx -> Cb
lbOnI xs = (V.!) . lbVec xs
{-# INLINE lbOnI #-}


-- | List of label indices at the given position.
lbIxsI :: Xs -> Int -> [CbIx]
lbIxsI xs i = [0 .. lbNum xs i - 1]
{-# INLINE lbIxsI #-}


-- | Number of potential labels at the given position of the sentence.
-- Function extended to indices outside the positions' domain.
lbNum :: Xs -> Int -> Int
lbNum xs i
    | i < 0 || i >= n   = 1
    | otherwise         = lbNumI xs i
  where
    n = V.length xs
{-# INLINE lbNum #-}


-- | Potential label at the given position and at the given index.
-- Return Nothing for positions outside the domain.
lbOn :: Xs -> Int -> CbIx -> Maybe Cb
lbOn xs i
    | i < 0 || i >= n   = const Nothing
    | otherwise         = Just . lbOnI xs i
  where
    n = V.length xs
{-# INLINE lbOn #-}


-- | List of label indices at the given position.  Function extended to
-- indices outside the positions' domain.
lbIxs :: Xs -> Int -> [CbIx]
lbIxs xs i
    | i < 0 || i >= n   = [0]
    | otherwise         = lbIxsI xs i
  where
    n = V.length xs
{-# INLINE lbIxs #-}


----------------------------------------------------------------
-- AVec
--
-- TODO: Move AVec and intersection implementation to a
-- separate module.
----------------------------------------------------------------


-- | An ascending vector of distinct elements.
newtype AVec a = AVec { unAVec :: V.Vector a }
    deriving (Show, Eq, Ord)


-- | Smart AVec constructor which ensures that the
-- underlying vector is strictly ascending.
mkAVec :: Ord a => [a] -> AVec a
mkAVec = AVec . V.fromList . S.toAscList  . S.fromList 
{-# INLINE mkAVec #-}


-- | An ascending vector of distinct elements with respect
-- to 'fst' values.
newtype AVec2 a b = AVec2 { unAVec2 :: V.Vector (a, b) }
    deriving (Show, Eq, Ord)


-- | Smart AVec constructor which ensures that the
-- underlying vector is strictly ascending with respect
-- to fst values.
mkAVec2 :: Ord a => [(a, b)] -> AVec2 a b
mkAVec2 = AVec2 . V.fromList . M.toAscList  . M.fromList 
{-# INLINE mkAVec2 #-}
