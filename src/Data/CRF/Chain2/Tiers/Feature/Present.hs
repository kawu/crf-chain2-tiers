-- | The module provides feature selection functions which extract
-- features present in the dataset, i.e. features which directly occure
-- the dataset.
--
-- You can mix functions defined here with the selection functions
-- from the "Data.CRF.Chain2.Tiers.Feature.Hidden" module.

module Data.CRF.Chain2.Tiers.Feature.Present
( presentFeats
, presentOFeats
, presentTFeats
) where


import           Data.CRF.Chain2.Tiers.Dataset.Internal
import           Data.CRF.Chain2.Tiers.Feature


-- | Present observation features in the given dataset element.
presentOFeats :: (Xs, Ys) -> [Feat]
presentOFeats = map fst . uncurry probOFeats


-- | Present transition features in the given dataset element.
presentTFeats :: (Xs, Ys) -> [Feat]
presentTFeats = map fst . probTFeats . snd


-- | Present features of both types in the given dataset element.
presentFeats :: (Xs, Ys) -> [Feat]
presentFeats = map fst . uncurry probFeats
