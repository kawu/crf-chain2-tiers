module Data.CRF.Chain2.Tiers.DAG.Dataset.External
( Sent
, SentL
, module Data.CRF.Chain2.Tiers.Dataset.External
) where


import qualified Data.DAG as DAG
import           Data.DAG (DAG)

import           Data.CRF.Chain2.Tiers.Dataset.External hiding (Sent, SentL)


-- | A sentence (DAG) of words.
type Sent a b = DAG () (Word a b)


-- | A sentence (DAG) of labeled words.
type SentL a b = DAG () (WordL a b)
