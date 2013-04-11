module Data.CRF.Chain2.Tiers.Feature.Present
( presentFeats
, presentOFeats
, presentTFeats
) where


import           Data.CRF.Chain2.Tiers.Dataset.Internal
import           Data.CRF.Chain2.Tiers.Feature


-- | Transition features with assigned probabilities for a given position.
presentTFeats :: Ys -> Int -> [(Feat, L.LogFloat)]
presentTFeats ys k
    | k > 1     =
        [ (ft , px * py * pz)
        | (cx, px) <- unY (ys V.! k)
        , (cy, py) <- unY (ys V.! (k - 1))
        , (cz, pz) <- unY (ys V.! (k - 2))
        , ft <- trFeats3 cx cy cz ]
    | k == 1    =
        [ (ft , px * py)
        | (cx, px) <- unY (ys V.! k)
        , (cy, py) <- unY (ys V.! (k - 1))
        , ft <- trFeats2 cx cy ]
    | k == 0    =
        [ (ft , px)
        | (cx, px) <- unY (ys V.! k)
        , ft <- trFeats1 cx ]
    | otherwise =
        error "presentTFeats: sentence position negative"


-- | Observation features with assigned probabilities for a given position.
presentOFeats :: Xs -> Ys -> Int -> [(Feat, L.LogFloat)]
presentOFeats xs ys k =
    [ (OFeat o x i, L.logFloat px)
    | (cx, px) <- unY (ys V.! k)
    , o        <- unX (xs V.! k)
    , (x, i)   <- zip (unCb cx) [0..] ]





-- -- | Present observation features in the given dataset element.
-- presentOFeats :: (Xs, Ys) -> [Feat]
-- presentOFeats = map fst . uncurry probOFeats
-- 
-- 
-- -- | Present transition features in the given dataset element.
-- presentTFeats :: (Xs, Ys) -> [Feat]
-- presentTFeats = map fst . probTFeats . snd
-- 
-- 
-- -- | Present features of both types in the given dataset element.
-- presentFeats :: (Xs, Ys) -> [Feat]
-- presentFeats = map fst . uncurry probFeats
