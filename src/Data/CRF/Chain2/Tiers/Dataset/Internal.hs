{-# LANGUAGE GeneralizedNewtypeDeriving #-}


-- | Internal core data types.


module Data.CRF.Chain2.Tiers.Dataset.Internal
(
-- * Basic types
  Ob (..)
, Lb (..)
, LbIx
, FeatIx (..)

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
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Array.Unboxed as A
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Generic.Base as G
import qualified Data.Vector.Generic.Mutable as G


----------------------------------------------------------------
-- Basic types
----------------------------------------------------------------


-- | An observation.
newtype Ob = Ob { unOb :: Int }
    deriving ( Show, Eq, Ord, Binary, A.IArray A.UArray
             , G.Vector U.Vector, G.MVector U.MVector, U.Unbox )


-- | A label.
newtype Lb = Lb { unLb :: Int }
    deriving ( Show, Eq, Ord, Binary, A.IArray A.UArray
             , G.Vector U.Vector, G.MVector U.MVector, U.Unbox )
-- 	         , Num, Ix )


-- | An index of the label.
type LbIx = Int


-- | A feature index.  To every model feature a unique index is assigned.
newtype FeatIx = FeatIx { unFeatIx :: Int }
    deriving ( Show, Eq, Ord, Binary, A.IArray A.UArray
             , G.Vector U.Vector, G.MVector U.MVector, U.Unbox )


----------------------------------------------------------------
-- Internal dataset representation
----------------------------------------------------------------


-- | A word represented by a list of its observations
-- and a list of its potential label interpretations.
data X = X {
    -- | A vector of observations.
      _unX :: AVec Ob
    -- | A vector of potential labels.
    , _unR :: AVec Lb }
    deriving (Show, Eq, Ord)


-- | Sentence of words.
type Xs = V.Vector X


-- | X constructor.
mkX :: [Ob] -> [Lb] -> X
mkX x r  = X (mkAVec x) (mkAVec r)
{-# INLINE mkX #-}


-- | List of observations.
unX :: X -> [Ob]
unX = V.toList . unAVec . _unX
{-# INLINE unX #-}


-- | List of potential labels.
unR :: X -> [Lb]
unR = V.toList . unAVec . _unR
{-# INLINE unR #-}


-- | Vector of chosen labels together with
-- corresponding probabilities.
newtype Y = Y { _unY :: AVec2 Lb Double }
    deriving (Show, Eq, Ord)


-- | Y constructor.
mkY :: [(Lb, Double)] -> Y
mkY = Y . mkAVec2
{-# INLINE mkY #-}


-- | Y deconstructor symetric to mkY.
unY :: Y -> [(Lb, Double)]
unY = V.toList . unAVec2 . _unY
{-# INLINE unY #-}


-- | Sentence of Y (label choices).
type Ys = V.Vector Y


-- | Potential label at the given position.
lbAt :: X -> LbIx -> Lb
lbAt x = (unAVec (_unR x) V.!)
{-# INLINE lbAt #-}


lbVec :: Xs -> Int -> AVec Lb
lbVec xs = _unR . (xs V.!)
{-# INLINE lbVec #-}


-- | Number of potential labels at the given position of the sentence.
lbNumI :: Xs -> Int -> Int
lbNumI xs = V.length . unAVec . lbVec xs
{-# INLINE lbNumI #-}


-- | Potential label at the given position and at the given index.
lbOnI :: Xs -> Int -> LbIx -> Lb
lbOnI xs = (V.!) . unAVec . lbVec xs
{-# INLINE lbOnI #-}


-- | List of label indices at the given position.
lbIxsI :: Xs -> Int -> [LbIx]
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
lbOn :: Xs -> Int -> LbIx -> Maybe Lb
lbOn xs i
    | i < 0 || i >= n   = const Nothing
    | otherwise         = Just . lbOnI xs i
  where
    n = V.length xs
{-# INLINE lbOn #-}


-- | List of label indices at the given position.  Function extended to
-- indices outside the positions' domain.
lbIxs :: Xs -> Int -> [LbIx]
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
