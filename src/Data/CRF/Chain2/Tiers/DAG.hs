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
, I.ProbType (..)
, probs

-- * Dataset
, module Data.CRF.Chain2.Tiers.DAG.Dataset.External

-- * Feature selection
, Feat.FeatSel
, Feat.selectHidden
, Feat.selectPresent
) where


import           Control.Applicative ((<$>), (<*>))
import           Control.Monad (when)

import           System.IO (hSetBuffering, stdout, BufferMode (..))
import           Data.Maybe (maybeToList)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Data.Binary (Binary, get, put)
import qualified Data.Vector.Unboxed as U
import qualified Data.Number.LogFloat as LogFloat
import qualified Numeric.SGD.Momentum as SGD
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
import qualified Data.CRF.Chain2.Tiers.DAG.Inference as I
import qualified Data.CRF.Chain2.Tiers.DAG.Probs as P


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
    let trainLenOld = length trainData_
        trainData0 = verifyDataset trainData_
        trainLenNew = length trainData0
    -- mapM_ print $ map dagProb trainData_
    when (trainLenNew < trainLenOld) $ do
      putStrLn $ "Discarded "
        ++ show (trainLenOld - trainLenNew) ++ "/" ++ show trainLenOld
        ++  " elements from the training dataset"
    SGD.withData onDisk trainData0 $ \trainData -> do
    -- SGD.withData onDisk trainData_ $ \trainData -> do

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
    | (ft, val) <- I.expectedFeaturesIn curr (fmap fst dag)
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
      putStrLn "" >> report para
--       report $ U.map (*50.0) para
--       report $ U.map (*10.0) para
--       report $ U.map (*2.0) para
--       report $ U.map (*0.9) para
--       report $ U.map (*0.5) para
--       report $ U.map (*0.1) para
  where

    report para = do
      let crf = model {Model.values = para}
      llh <- show
        . LogFloat.logFromLogFloat
        . P.parLikelihood crf
        <$> SGD.loadData trainData
      acc <-
        if SGD.size evalData > 0
        then show . I.accuracy crf <$> SGD.loadData evalData
        else return "#"
      putStrLn $ "[" ++ show (doneTotal k) ++ "] stats:"
      putStrLn $ "min(params) = " ++ show (U.minimum para)
      putStrLn $ "max(params) = " ++ show (U.maximum para)
      putStrLn $ "log(likelihood(train)) = " ++ llh
      putStrLn $ "acc(eval) = " ++ acc

--     report para = do
--       acc <-
--         if SGD.size evalData > 0
--         then show . I.accuracy (model { Model.values = para }) <$> SGD.loadData evalData
--         else return "#"
--       putStrLn $
--         "[" ++ show (doneTotal k) ++ "] acc = " ++ acc ++
--         ", min(params) = " ++ show (U.minimum para) ++
--         ", max(params) = " ++ show (U.maximum para)

    doneTotal :: Int -> Int
    doneTotal = floor . done
    done :: Int -> Double
    done i
        = fromIntegral (i * batchSize)
        / fromIntegral trainSize
    trainSize = SGD.size trainData

------------------------------------------------------
-- Verification
--
-- TODO: virtually the same computation as in the
-- `crf-chain1-constarined` library!
------------------------------------------------------


-- | Compute the probability of the DAG, based on the probabilities assigned to
-- different edges and their labels.
dagProb :: DAG a (X, Y) -> Double
dagProb dag = sum
  [ fromEdge edgeID
  | edgeID <- DAG.dagEdges dag
  , DAG.isInitialEdge edgeID dag ]
  where
    fromEdge edgeID
      = edgeProb edgeID
      * fromNode (DAG.endsWith edgeID dag)
    edgeProb edgeID =
      let (_x, y) = DAG.edgeLabel edgeID dag
      in  sum . map (LogFloat.fromLogFloat . snd) $ Core.unY y
    fromNode nodeID =
      case DAG.outgoingEdges nodeID dag of
        [] -> 1
        xs -> sum (map fromEdge xs)


-- | Filter out sentences with `dagProb` different from 1.
verifyDataset :: [DAG a (X, Y)] -> [DAG a (X, Y)]
verifyDataset =
  filter verify
  where
    verify dag =
      let p = dagProb dag
      in  p >= 1 - eps && p <= 1 + eps
    eps = 1e-9


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
  . I.marginals' model
  . Codec.encodeSent codec
  $ sent
  where
    decodeChosen (word, chosen) = (word,) $ mkProb
      [ (decode word x, LogFloat.fromLogFloat p)
      | (x, p) <- chosen ]
      where
    decode word = Codec.unJust codec word . Codec.decodeLabel codec


-- | Tag labels with marginal probabilities.
probs :: (Ord a, Ord b) => I.ProbType -> CRF a b -> Sent a b -> SentL a b
probs probTyp CRF{..} sent
  = fmap decodeChosen
  . DAG.zipE sent
  . I.probs' probTyp model
  . Codec.encodeSent codec
  $ sent
  where
    decodeChosen (word, chosen) = (word,) $ mkProb
      [ (decode word x, LogFloat.fromLogFloat p)
      | (x, p) <- chosen ]
      where
    decode word = Codec.unJust codec word . Codec.decodeLabel codec
