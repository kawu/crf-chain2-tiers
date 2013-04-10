module Data.CRF.Chain2.Tiers.Feature
( Feat (..)
-- , isSFeat
-- , isTFeat
-- , isOFeat
, featuresIn
) where

import           Data.Binary (Binary, put, get, putWord8, getWord8)
import           Control.Applicative ((<*>), (<$>))
import           Data.List (zip4)
import qualified Data.Vector as V
import qualified Data.Number.LogFloat as L

import Data.CRF.Chain2.Tiers.Dataset.Internal


-- | Feature.
data Feat
    = TFeat3
        { x1    :: {-# UNPACK #-} !Lb
        , x2    :: {-# UNPACK #-} !Lb
        , x3    :: {-# UNPACK #-} !Lb
        , ln    :: {-# UNPACK #-} !Int }
    | TFeat2
        { x1    :: {-# UNPACK #-} !Lb
        , x2    :: {-# UNPACK #-} !Lb
        , ln    :: {-# UNPACK #-} !Int }
    | TFeat1
        { x1    :: {-# UNPACK #-} !Lb
        , ln    :: {-# UNPACK #-} !Int }
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


-- -- | Is it a 'SFeature'?
-- isSFeat :: Feature -> Bool
-- isSFeat (SFeature _) = True
-- isSFeat _            = False
-- {-# INLINE isSFeat #-}
-- 
-- -- | Is it an 'OFeature'?
-- isOFeat :: Feature -> Bool
-- isOFeat (OFeature _ _) = True
-- isOFeat _              = False
-- {-# INLINE isOFeat #-}
-- 
-- -- | Is it a 'TFeature'?
-- isTFeat :: Feature -> Bool
-- isTFeat (TFeature _ _) = True
-- isTFeat _              = False
-- {-# INLINE isTFeat #-}


-- | Transition features with assigned probabilities for given position.
trFeats :: Ys -> Int -> [(Feat, L.LogFloat)]
trFeats ys k
    | k > 1     =
        [ ( TFeat3 x y z i
          , L.logFloat px * L.logFloat py * L.logFloat pz )
        | (cx, px) <- unY (ys V.! k)
        , (cy, py) <- unY (ys V.! (k - 1))
        , (cz, pz) <- unY (ys V.! (k - 2))
        , (x, y, z, i) <- zip4 (unCb cx) (unCb cy) (unCb cz) [0..] ]
    | k == 1    =
        [ (TFeat2 x y i, L.logFloat px * L.logFloat py)
        | (cx, px) <- unY (ys V.! 1)
        , (cy, py) <- unY (ys V.! 0)
        , (x, y, i) <- zip3 (unCb cx) (unCb cy) [0..] ]
    | k == 0    =
        [ (TFeat1 x i, L.logFloat px)
        | (cx, px) <- unY (ys V.! 0)
        , (x, i) <- zip (unCb cx) [0..] ]
    | otherwise =
        error "trFeats: sentence position negative"

-- | Observation features with assigned probabilities for a given position.
obFeats :: Xs -> Ys -> Int -> [(Feat, L.LogFloat)]
obFeats xs ys k =
    [ (OFeat o x i, L.logFloat px)
    | (cx, px) <- unY (ys V.! k)
    , o        <- unX (xs V.! k)
    , (x, i)   <- zip (unCb cx) [0..] ]

-- | All features with assigned probabilities for given position.
features :: Xs -> Ys -> Int -> [(Feat, L.LogFloat)]
features xs ys k = trFeats ys k ++ obFeats xs ys k

-- | All features with assigned probabilities in given labeled sentence.
featuresIn :: Xs -> Ys -> [(Feat, L.LogFloat)]
featuresIn xs ys = concatMap (features xs ys) [0 .. V.length xs - 1]
