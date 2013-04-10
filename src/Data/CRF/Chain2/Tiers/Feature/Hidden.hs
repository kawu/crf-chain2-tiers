-- | The module provides feature selection functions which extract
-- hidden features, i.e. all features which can be constructed 
-- on the basis of observations and potential labels (constraints)
-- corresponding to individual words.
--
-- You can mix functions defined here with the selection functions
-- from the "Data.CRF.Chain2.Tiers.Feature.Present" module.

module Data.CRF.Chain2.Tiers.Feature.Hidden
( hiddenFeats
, hiddenOFeats
, hiddenTFeats
) where

import           Data.List (zip4)
import qualified Data.Vector as V

import Data.CRF.Chain2.Tiers.Dataset.Internal
import Data.CRF.Chain2.Tiers.Feature


-- | Hidden observation features for a given word.
oFeats :: X -> [Feat]
oFeats w =
    [ OFeat o x i
    | cx       <- unR w
    , o        <- unX w
    , (x, i)   <- zip (unCb cx) [0..] ]


-- | Hidden observation features which can be constructed
-- based on the dataset.
hiddenOFeats :: (Xs, b) -> [Feat]
hiddenOFeats = concatMap oFeats . V.toList . fst


-- | Hidden transition features for a given position.
tFeats :: Xs -> Int -> [Feat]
tFeats xs k
    | k > 1     =
        [ TFeat3 x y z i
        | cx <- unR (xs V.! k)
        , cy <- unR (xs V.! (k - 1))
        , cz <- unR (xs V.! (k - 2))
        , (x, y, z, i) <- zip4 (unCb cx) (unCb cy) (unCb cz) [0..] ]
    | k == 1    =
        [ TFeat2 x y i
        | cx <- unR (xs V.! 1)
        , cy <- unR (xs V.! 0)
        , (x, y, i) <- zip3 (unCb cx) (unCb cy) [0..] ]
    | k == 0    =
        [ TFeat1 x i
        | cx <- unR (xs V.! 0)
        , (x, i) <- zip (unCb cx) [0..] ]
    | otherwise =
        error "hiddenTFeats: sentence position negative"


-- | Hidden transition features which can be constructed
-- based on the dataset.
hiddenTFeats :: (Xs, b) -> [Feat]
hiddenTFeats (xs, _) = concatMap (tFeats xs) [0 .. V.length xs - 1]


-- | Hidden features of both types.
hiddenFeats :: (Xs, b) -> [Feat]
hiddenFeats e = hiddenOFeats e ++ hiddenTFeats e
