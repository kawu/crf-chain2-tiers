module Data.CRF.Chain2.Tiers.Feature
(
-- * Feature
  Feat (..)

-- * Feature generation
, obFeats
, trFeats1
, trFeats2
, trFeats3

-- * Feature extraction
, presentFeats
, hiddenFeats
, obFeatsOn
, trFeatsOn

-- * Feature selection
, FeatSel
, selectPresent
, selectHidden
) where


import           Control.Applicative ((<*>), (<$>))
import           Data.Maybe (maybeToList)
import           Data.List (zip4)
import           Data.Binary (Binary, put, get, putWord8, getWord8)
import qualified Data.Vector as V
import qualified Data.Number.LogFloat as L

import Data.CRF.Chain2.Tiers.Dataset.Internal


----------------------------------------------------
-- Feature
----------------------------------------------------


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


----------------------------------------------------
-- Feature extraction
----------------------------------------------------


-- | Features present in the dataset element together with corresponding
-- occurence probabilities.
presentFeats :: Xs -> Ys -> [(Feat, L.LogFloat)]
presentFeats xs ys = concat
    [ obFs i ++ trFs i
    | i <- [0 .. V.length xs - 1] ]
  where
    obFs i =
        [ (ft, pr)
        | o <- unX (xs V.! i)
        , (u, pr) <- unY (ys V.! i)
        , ft <- obFeats o u ]
    trFs 0 =
        [ (ft, pr)
        | (u, pr) <- unY (ys V.! 0)
        , ft <- trFeats1 u ]
    trFs 1 =
        [ (ft, pr1 * pr2)
        | (u, pr1) <- unY (ys V.! 1)
        , (v, pr2) <- unY (ys V.! 0)
        , ft <- trFeats2 u v ]
    trFs i =
        [ (ft, pr1 * pr2 * pr3)
        | (u, pr1) <- unY (ys V.! i)
        , (v, pr2) <- unY (ys V.! (i-1))
        , (w, pr3) <- unY (ys V.! (i-2))
        , ft <- trFeats3 u v w ]


-- | Features hidden in the dataset element.
hiddenFeats :: Xs -> [Feat]
hiddenFeats xs =
    obFs ++ trFs
  where
    obFs = concat
        [ obFeatsOn xs i u
        | i <- [0 .. V.length xs - 1]
        , u <- lbIxs xs i ]
    trFs = concat
        [ trFeatsOn xs i u v w
        | i <- [0 .. V.length xs - 1]
        , u <- lbIxs xs i
        , v <- lbIxs xs $ i - 1
        , w <- lbIxs xs $ i - 2 ]


-- | Observation features on a given position and with respect
-- to a given label (determined by idenex).
obFeatsOn :: Xs -> Int -> CbIx -> [Feat]
obFeatsOn xs i u = concat
    [ obFeats ob' e
    | e   <- lbs
    , ob' <- unX (xs V.! i) ]
  where 
    lbs     = maybeToList (lbOn xs i u)
{-# INLINE obFeatsOn #-}


-- | Transition features on a given position and with respect
-- to a given labels (determined by indexes).
trFeatsOn :: Xs -> Int -> CbIx -> CbIx -> CbIx -> [Feat]
trFeatsOn xs i u' v' w' =
    doIt a b c
  where
    a = lbOn xs i       u'
    b = lbOn xs (i - 1) v'
    c = lbOn xs (i - 2) w'
    doIt (Just u) (Just v) (Just w) = trFeats3 u v w
    doIt (Just u) (Just v) _        = trFeats2 u v
    doIt (Just u) _ _               = trFeats1 u
    doIt _ _ _                      = []
{-# INLINE trFeatsOn #-}


----------------------------------------------------
-- Feature selection
----------------------------------------------------


-- | A feature selection function type.
type FeatSel = Xs -> Ys -> [Feat]


-- | The 'presentFeats' adapted to fit feature selection specs.
selectPresent :: FeatSel
selectPresent xs = map fst . presentFeats xs


-- | The 'hiddenFeats' adapted to fit feature selection specs.
selectHidden :: FeatSel
selectHidden xs _ = hiddenFeats xs
