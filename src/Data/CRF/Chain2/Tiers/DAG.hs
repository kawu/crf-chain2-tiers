{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TupleSections #-}


module Data.CRF.Chain2.Tiers.DAG
(
-- * CRF
  CRF (..)
, size
, prune

-- * Training
, train
-- , reTrain

-- * Tagging
-- , tag
, marginals
, Inf.ProbType (..)
, probs

-- * Dataset
, module Data.CRF.Chain2.Tiers.DAG.Dataset.External

-- * Feature selection
, Feat.FeatSel
, Feat.selectHidden
, Feat.selectPresent
) where


import           Control.Applicative ((<$>), (<*>))

import           System.IO (hSetBuffering, stdout, BufferMode (..))
import           Data.Maybe (maybeToList)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Data.Binary (Binary, get, put)
import qualified Data.Vector.Unboxed as U
import qualified Data.Number.LogFloat as LogFloat
import qualified Numeric.SGD as SGD
import qualified Numeric.SGD.LogSigned as L

import           Data.DAG (DAG)
import qualified Data.DAG as DAG

import           Data.CRF.Chain2.Tiers.Core (X, Y)
import qualified Data.CRF.Chain2.Tiers.Core as Core
import qualified Data.CRF.Chain2.Tiers.Model as Model
import           Data.CRF.Chain2.Tiers.Model (Model)
-- import qualified Data.CRF.Chain2.Tiers.DAG.Dataset.Internal as Int
-- import qualified Data.CRF.Chain2.Tiers.Dataset.External as Ext
import           Data.CRF.Chain2.Tiers.DAG.Dataset.External
import qualified Data.CRF.Chain2.Tiers.DAG.Dataset.Codec as Codec
import           Data.CRF.Chain2.Tiers.DAG.Dataset.Codec (Codec)
import qualified Data.CRF.Chain2.Tiers.DAG.Feature as Feat
import           Data.CRF.Chain2.Tiers.DAG.Feature (Feat, FeatSel)
import qualified Data.CRF.Chain2.Tiers.DAG.Inference as Inf


----------------------------------------------------
-- CRF model
----------------------------------------------------


-- | CRF model data.
data CRF a b = CRF
    { numOfLayers   :: Int
    , codec         :: Codec a b
    , model         :: Model }


instance (Ord a, Ord b, Binary a, Binary b) => Binary (CRF a b) where
    put CRF{..} = put numOfLayers >> put codec >> put model
    get = CRF <$> get <*> get <*> get


-- | Compute size (number of features) of the model.
size :: CRF a b -> Int
size CRF{..} = M.size (Model.toMap model)


-- | Discard model features with absolute values (in log-domain)
-- lower than the given threshold.
prune :: Double -> CRF a b -> CRF a b
prune x crf =  crf { model = newModel } where
    newModel = Model.fromMap . M.fromList $
        [ (feat, val)
        | (feat, val) <- M.toList $ Model.toMap (model crf)
        , abs (LogFloat.logFromLogFloat val) > x ]


-- | Construct model from a dataset given a feature selection function.
mkModel :: (DAG a (X, Y) -> [Feat]) -> [DAG a (X, Y)] -> Model
mkModel featSel
  = Model.fromSet . S.fromList
  . concatMap featSel -- (map fst . Feat.presentFeats)


----------------------------------------------------
-- Training
----------------------------------------------------


-- | Train the CRF using the stochastic gradient descent method.
train
  :: (Ord a, Ord b)
  => Int                          -- ^ Number of layers (tiers)
  -> Feat.FeatSel ()              -- ^ Feature selection
  -> SGD.SgdArgs                  -- ^ SGD parameters
  -> Bool                         -- ^ Store dataset on a disk
  -> IO [SentL a b]               -- ^ Training data 'IO' action
  -> IO [SentL a b]               -- ^ Evaluation data
  -> IO (CRF a b)                 -- ^ Resulting model
train numOfLayers featSel sgdArgs onDisk trainIO evalIO = do
    hSetBuffering stdout NoBuffering

    -- Create codec and encode the training dataset
    codec <- Codec.mkCodec numOfLayers    <$> trainIO
    trainData_ <- Codec.encodeDataL codec <$> trainIO
    SGD.withData onDisk trainData_ $ \trainData -> do

    -- Encode the evaluation dataset
    evalData_ <- Codec.encodeDataL codec <$> evalIO
    SGD.withData onDisk evalData_ $ \evalData -> do

    -- Train the model
    model <- mkModel featSel <$> SGD.loadData trainData
    para  <- SGD.sgd sgdArgs
        (notify sgdArgs model trainData evalData)
        (gradOn model) trainData (Model.values model)
    return $ CRF numOfLayers codec model { Model.values = para }


-- -- | Re-train the CRF using the stochastic gradient descent method.
-- reTrain
--     :: (Ord a, Ord b)
--     => CRF a b                      -- ^ Existing CRF model
--     -> SGD.SgdArgs                  -- ^ SGD parameters
--     -> Bool                         -- ^ Store dataset on a disk
--     -> IO [SentL a b]               -- ^ Training data 'IO' action
--     -> IO [SentL a b]               -- ^ Evaluation data
--     -> IO (CRF a b)                 -- ^ Resulting model
-- reTrain crf sgdArgs onDisk trainIO evalIO = do
--     hSetBuffering stdout NoBuffering
-- 
--     -- Encode the training dataset
--     trainData_ <- encodeDataL (codec crf) <$> trainIO
--     SGD.withData onDisk trainData_ $ \trainData -> do
-- 
--     -- Encode the evaluation dataset
--     evalData_ <- encodeDataL (codec crf) <$> evalIO
--     SGD.withData onDisk evalData_ $ \evalData -> do
-- 
--     -- Train the model
--     let model' = model crf
--     para  <- SGD.sgd sgdArgs
--         (notify sgdArgs model' trainData evalData)
--         (gradOn model') trainData (values model')
--     return $ crf { model = model' { values = para } }


-- | Compute gradient on a dataset element.
gradOn :: Model -> SGD.Para -> DAG a (X, Y) -> SGD.Grad
-- gradOn model para (xs, ys) = SGD.fromLogList $
gradOn model para dag = SGD.fromLogList $
    [ (Core.unFeatIx ix, L.fromPos val)
    | (ft, val) <- Feat.presentFeats dag
    , ix <- maybeToList (Model.index curr ft) ] ++
    [ (Core.unFeatIx ix, L.fromNeg val)
    | (ft, val) <- Inf.expectedFeaturesIn curr (fmap fst dag)
    , ix <- maybeToList (Model.index curr ft) ]
  where
    curr = model { Model.values = para }


notify
    :: SGD.SgdArgs -> Model
    -> SGD.Dataset (DAG a (X, Y))         -- ^ Training dataset
    -> SGD.Dataset (DAG a (X, Y))         -- ^ Evaluaion dataset
    -> SGD.Para -> Int -> IO ()
notify SGD.SgdArgs{..} model trainData evalData para k
  | doneTotal k == doneTotal (k - 1) = putStr "."
  | otherwise = do
      acc <-
        if SGD.size evalData > 0
        then show . Inf.accuracy (model { Model.values = para }) <$> SGD.loadData evalData
        else return "#"
      putStrLn $
        "\n" ++ "[" ++ show (doneTotal k) ++ "] acc = " ++ acc ++
        ", minVal = " ++ show (U.minimum para) ++
        ", maxVal = " ++ show (U.maximum para)
  where
    doneTotal :: Int -> Int
    doneTotal = floor . done
    done :: Int -> Double
    done i
        = fromIntegral (i * batchSize)
        / fromIntegral trainSize
    trainSize = SGD.size trainData


----------------------------------------------------
-- Tagging
----------------------------------------------------


-- -- | Find the most probable label sequence.
-- tag :: (Ord a, Ord b) => CRF a b -> Sent a b -> [[b]]
-- tag CRF{..} sent
--     = onWords . decodeLabels codec
--     . I.tag model . encodeSent codec
--     $ sent
--   where
--     onWords xs =
--         [ unJust codec word x
--         | (word, x) <- zip sent xs ]


-- | Tag labels with marginal probabilities.
marginals :: (Ord a, Ord b) => CRF a b -> Sent a b -> SentL a b
marginals CRF{..} sent
  = fmap decodeChosen
  . DAG.zipE sent
  . Inf.marginals' model
  . Codec.encodeSent codec
  $ sent
  where
    decodeChosen (word, chosen) = (word,) $ mkProb
      [ (decode word x, LogFloat.fromLogFloat p)
      | (x, p) <- chosen ]
      where
    decode word = Codec.unJust codec word . Codec.decodeLabel codec


-- | Tag labels with marginal probabilities.
probs :: (Ord a, Ord b) => Inf.ProbType -> CRF a b -> Sent a b -> SentL a b
probs probTyp CRF{..} sent
  = fmap decodeChosen
  . DAG.zipE sent
  . Inf.probs' probTyp model
  . Codec.encodeSent codec
  $ sent
  where
    decodeChosen (word, chosen) = (word,) $ mkProb
      [ (decode word x, LogFloat.fromLogFloat p)
      | (x, p) <- chosen ]
      where
    decode word = Codec.unJust codec word . Codec.decodeLabel codec
