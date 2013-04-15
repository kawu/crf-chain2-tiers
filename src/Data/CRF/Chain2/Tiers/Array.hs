{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}


-- | An unboxed array in terms of an unboxed vector.


module Data.CRF.Chain2.Tiers.Array
(
-- * Array
  Array
, mkArray
, (!?)

-- * Bounds
, Bounds (..)
) where


import           Control.Applicative ((<$>), (<*>))
import           Control.Arrow (first)
import           Data.Ix
import           Data.Int (Int16)
import           Data.List (foldl1')
import qualified Data.Vector.Unboxed as U
import           Data.Binary (Binary, get, put)
import           Data.Vector.Binary ()


data Array i a = Array
    { bounds    :: (i, i)
    , array     :: U.Vector a }


instance (Binary i, Binary a, U.Unbox a) => Binary (Array i a) where
    put Array{..} = put bounds >> put array
    get = Array <$> get <*> get


-- | Construct array with a default value.
mkArray :: (Bounds i, U.Unbox a) => a -> [(i, a)] -> Array i a
mkArray defVal xs = Array
    { bounds    = (p, q)
    , array     = zeroed U.// map (first ix) xs }
  where
    p       = foldl1' lower (map fst xs)
    q       = foldl1' upper (map fst xs)
    ix      = index (p, q)
    size    = rangeSize (p, q)
    zeroed  = U.replicate size defVal
{-# INLINE mkArray #-}


(!?) :: (Ix i, U.Unbox a) => Array i a -> i -> Maybe a
Array{..} !? x = if inRange bounds x
    -- TODO: Use unsafe indexing.
    then Just (array U.! index bounds x)
    else Nothing
{-# INLINE (!?) #-}


--------------------------------
-- Bounds
--------------------------------


-- | An extended Ix class.
class Ix i => Bounds i where
    -- | A lower bound of two values.
    lower :: i -> i -> i
    -- | An upper bound of two values.
    upper :: i -> i -> i


instance Bounds Int16 where
    lower x y = min x y
    upper x y = max x y


instance Bounds i => Bounds (i, i) where
    lower (!x1, !y1) (!x2, !y2) =
        ( lower x1 x2
        , lower y1 y2 )
    upper (!x1, !y1) (!x2, !y2) =
        ( upper x1 x2
        , upper y1 y2 )


instance Bounds i => Bounds (i, i, i) where
    lower (!x1, !y1, !z1) (!x2, !y2, !z2) =
        ( lower x1 x2
        , lower y1 y2
        , lower z1 z2 )
    upper (!x1, !y1, !z1) (!x2, !y2, !z2) =
        ( upper x1 x2
        , upper y1 y2
        , upper z1 z2 )
