-- | Internal core data types.


module Data.CRF.Chain2.Tiers.Dataset.Internal
(
  module Data.CRF.Chain2.Tiers.Core

-- * Input element (word)
, Xs

-- * Output element (choice)
, Ys

-- * Indexing
, lbOn
, lbNum
, lbIxs
) where


-- import           Data.Binary (Binary, put, get)
-- import           Data.Ix (Ix)
-- import           Control.Applicative ((<$>), (<*>))
-- import           Control.Arrow (second)
-- import           Data.Int (Int16, Int32)
-- import qualified Data.Array.Unboxed as A
import qualified Data.Vector as V
-- import qualified Data.Vector.Unboxed as U
-- import           Data.Vector.Unboxed.Deriving
-- import qualified Data.Vector.Generic.Base as G
-- import qualified Data.Vector.Generic.Mutable as G
-- import qualified Data.Number.LogFloat as L
-- -- import qualified Data.Primitive.ByteArray as BA
--
-- import           Data.CRF.Chain2.Tiers.Array (Bounds)


import           Data.CRF.Chain2.Tiers.Core


-- | Sentence of words.
type Xs = V.Vector X


-- | Sentence of Y (label choices).
type Ys = V.Vector Y


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
