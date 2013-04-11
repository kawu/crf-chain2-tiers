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
obFeats :: Xs -> Int -> [Feat]
obFeats xs k =
    [ OFeat o x i
    | cx       <- unR (xs V.! k)
    , o        <- unX (xs V.! k)
    , (x, i)   <- zip (unCb cx) [0..] ]


-- | Hidden transition features for a given position.
trFeats :: Xs -> Int -> [Feat]
trFeats xs k
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


-- | Hidden observation features in the given dataset element.
hiddenOFeats :: (Xs, b) -> [Feat]
hiddenOFeats (xs, _) = concatMap (obFeats xs) [0 .. V.length xs - 1]


-- | Hidden transition features in the given dataset element.
hiddenTFeats :: (Xs, b) -> [Feat]
hiddenTFeats (xs, _) = concatMap (trFeats xs) [0 .. V.length xs - 1]


-- | Hidden features of both types in the given dataset element.
hiddenFeats :: (Xs, b) -> [Feat]
hiddenFeats e = hiddenOFeats e ++ hiddenTFeats e
