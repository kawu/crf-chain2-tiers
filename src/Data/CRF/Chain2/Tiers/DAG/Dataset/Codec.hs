module Data.CRF.Chain2.Tiers.DAG.Dataset.Codec
(
  module Data.CRF.Chain2.Tiers.Dataset.Codec

, Xs
, XYs

, encodeSent'Cu
, encodeSent'Cn
, encodeSent

, encodeData
, encodeDataL
, mkCodec
) where


-- import           Prelude hiding (Word)
import qualified Data.Traversable as T
import           Data.DAG (DAG)

import           Control.Monad.Codec (evalCodec, execCodec)

import qualified Data.CRF.Chain2.Tiers.Dataset.Internal as I
import           Data.CRF.Chain2.Tiers.DAG.Dataset.External
import qualified Data.CRF.Chain2.Tiers.Dataset.Codec as C
import           Data.CRF.Chain2.Tiers.Dataset.Codec hiding
  (encodeSent'Cu, encodeSent'Cn, encodeSent, encodeSentL'Cu, encodeSentL'Cn,
  encodeSentL, encodeData, encodeDataL, mkCodec)


-- | Utility types.
type Xs = DAG () I.X
-- type Ys = DAG () I.Y
type XYs = DAG () (I.X, I.Y)


-------------------------------------
-- Normal sentences
-------------------------------------


-- | Encode the sentence and update the codec.
encodeSent'Cu :: (Ord a, Ord b) => Sent a b -> C.CodecM a b Xs
encodeSent'Cu = T.mapM C.encodeWord'Cu


-- | Encode the sentence and do *not* update the codec.
encodeSent'Cn :: (Ord a, Ord b) => Sent a b -> C.CodecM a b Xs
encodeSent'Cn = T.mapM C.encodeWord'Cn


-- | Encode the sentence using the given codec.
encodeSent :: (Ord a, Ord b) => C.Codec a b -> Sent a b -> Xs
encodeSent codec = evalCodec codec . encodeSent'Cn


-------------------------------------
-- Labeled sentences
-------------------------------------


-- | Encode the labeled sentence and update the codec.
encodeSentL'Cu :: (Ord a, Ord b) => SentL a b -> C.CodecM a b XYs
encodeSentL'Cu = T.mapM C.encodeWordL'Cu


-- | Encode the labeled sentence and do *not* update the codec. Substitute the
-- default label for any label not present in the codec.
encodeSentL'Cn :: (Ord a, Ord b) => SentL a b -> C.CodecM a b XYs
encodeSentL'Cn = T.mapM C.encodeWordL'Cn


-- | Encode the labeled sentence with the given codec.  Substitute the
-- default label for any label not present in the codec.
encodeSentL :: (Ord a, Ord b) => C.Codec a b -> SentL a b -> XYs
encodeSentL codec = evalCodec codec . encodeSentL'Cn


-------------------------------------
-- Datasets
-------------------------------------


-- | Encode the labeled dataset using the codec.  Substitute the default
-- label for any label not present in the codec.
encodeDataL :: (Ord a, Ord b) => C.Codec a b -> [SentL a b] -> [XYs]
encodeDataL = map . encodeSentL


-- | Encode the dataset with the codec.
encodeData :: (Ord a, Ord b) => C.Codec a b -> [Sent a b] -> [Xs]
encodeData = map . encodeSent


-------------------------------------
-- Creation
-------------------------------------


-- | Create codec on the basis of the labeled dataset.
mkCodec
  :: (Ord a, Ord b)
  => Int
  -- ^ The number of layers
  -> [SentL a b]
  -> Codec a b
mkCodec n = execCodec (empty n) . mapM_ encodeSentL'Cu
