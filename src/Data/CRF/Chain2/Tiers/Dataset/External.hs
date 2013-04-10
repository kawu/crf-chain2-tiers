-- | External data representation.

module Data.CRF.Chain2.Tiers.Dataset.External
( Word (obs, lbs)
, mkWord
, Sent
, Prob (unProb)
, mkProb
, WordL
, SentL
) where

import qualified Data.Set as S
import qualified Data.Map as M

-- | A word consists of a set of observations and a set of potential labels.
data Word a b = Word {
    -- | Set of observations.
      obs   :: S.Set a
    -- | Non-empty set of potential labels.
    , lbs   :: S.Set [b] }
    deriving (Show, Eq, Ord)

-- | A word constructor which checks non-emptiness of the potential
-- set of labels.
mkWord :: S.Set a -> S.Set [b] -> Word a b
mkWord _obs _lbs
    | S.null _lbs   = error "mkWord: empty set of potential labels"
    | otherwise     = Word _obs _lbs

-- | A sentence of words.
type Sent a b = [Word a b]

-- | A probability distribution defined over elements of type a.
-- All elements not included in the map have probability equal
-- to 0.
newtype Prob a = Prob { unProb :: M.Map a Double }
    deriving (Show, Eq, Ord)

-- | Construct the probability distribution.
mkProb :: Ord a => [(a, Double)] -> Prob a
mkProb =
    Prob . normalize . M.fromListWith (+) . filter ((>0).snd)
  where
    normalize dist 
        | M.null dist =
            error "mkProb: no elements with positive probability"
        | otherwise   =
            let z = sum (M.elems dist)
            in  fmap (/z) dist

-- | A WordL is a labeled word, i.e. a word with probability distribution
-- defined over labels.  We assume that every label from the distribution
-- domain is a member of the set of potential labels corresponding to the
-- word.  TODO: Ensure the assumption using the smart constructor.
type WordL a b = (Word a b, Prob [b])

-- | A sentence of labeled words.
type SentL a b = [WordL a b]
