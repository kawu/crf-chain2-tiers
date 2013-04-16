{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternGuards #-}


module Data.CRF.Chain2.Tiers
( 
-- * CRF
  CRF (..)
, size
, prune

-- * Training
, train
, reTrain

-- * Tagging
, tag

-- * Modules
, module Data.CRF.Chain2.Tiers.Dataset.External
, module Data.CRF.Chain2.Tiers.Feature
) where


import           System.IO (hSetBuffering, stdout, BufferMode (..))
import           Control.Applicative ((<$>), (<*>))
import           Data.Maybe (maybeToList)
import qualified Data.Map as M
import           Data.Binary (Binary, get, put)
import qualified Data.Number.LogFloat as LogFloat
import qualified Numeric.SGD as SGD
import qualified Numeric.SGD.LogSigned as L

import           Data.CRF.Chain2.Tiers.Dataset.Internal
import           Data.CRF.Chain2.Tiers.Dataset.Codec
import           Data.CRF.Chain2.Tiers.Dataset.External
import           Data.CRF.Chain2.Tiers.Feature
import           Data.CRF.Chain2.Tiers.Model
import qualified Data.CRF.Chain2.Tiers.Inference as I


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
size CRF{..} = M.size (toMap model)


-- | Prune model features with absolute values (in log-domain)
-- greater than the given threshold.
prune :: Double -> CRF a b -> CRF a b
prune x crf =  crf { model = newModel } where
    newModel = fromMap . M.fromList $
        [ (feat, val)
        | (feat, val) <- M.toList $ toMap (model crf)
        , abs (LogFloat.logFromLogFloat val) > x ]


----------------------------------------------------
-- Training
----------------------------------------------------


-- | Train the CRF using the stochastic gradient descent method.
train
    :: (Ord a, Ord b)
    => Int                          -- ^ Number of layers (tiers)
    -> FeatSel                      -- ^ Feature selection
    -> SGD.SgdArgs                  -- ^ SGD parameters
    -> Bool                         -- ^ Store dataset on a disk
    -> IO [SentL a b]               -- ^ Training data 'IO' action
    -> IO [SentL a b]               -- ^ Evaluation data
    -> IO (CRF a b)                 -- ^ Resulting model
train numOfLayers featSel sgdArgs onDisk trainIO evalIO = do
    hSetBuffering stdout NoBuffering

    -- Create codec and encode the training dataset
    codec <- mkCodec numOfLayers    <$> trainIO
    trainData_ <- encodeDataL codec <$> trainIO
    SGD.withData onDisk trainData_ $ \trainData -> do

    -- Encode the evaluation dataset
    evalData_ <- encodeDataL codec <$> evalIO
    SGD.withData onDisk evalData_ $ \evalData -> do

    -- Train the model
    model <- mkModel featSel <$> SGD.loadData trainData
    para  <- SGD.sgd sgdArgs
        (notify sgdArgs model trainData evalData)
        (gradOn model) trainData (values model)
    return $ CRF numOfLayers codec model { values = para }


-- | Re-train the CRF using the stochastic gradient descent method.
reTrain
    :: (Ord a, Ord b)
    => CRF a b                      -- ^ Existing CRF model
    -> SGD.SgdArgs                  -- ^ SGD parameters
    -> Bool                         -- ^ Store dataset on a disk
    -> IO [SentL a b]               -- ^ Training data 'IO' action
    -> IO [SentL a b]               -- ^ Evaluation data
    -> IO (CRF a b)                 -- ^ Resulting model
reTrain crf sgdArgs onDisk trainIO evalIO = do
    hSetBuffering stdout NoBuffering

    -- Encode the training dataset
    trainData_ <- encodeDataL (codec crf) <$> trainIO
    SGD.withData onDisk trainData_ $ \trainData -> do

    -- Encode the evaluation dataset
    evalData_ <- encodeDataL (codec crf) <$> evalIO
    SGD.withData onDisk evalData_ $ \evalData -> do

    -- Train the model
    let model' = model crf
    para  <- SGD.sgd sgdArgs
        (notify sgdArgs model' trainData evalData)
        (gradOn model') trainData (values model')
    return $ crf { model = model' { values = para } }


-- | Compute gradient on a dataset element.
gradOn :: Model -> SGD.Para -> (Xs, Ys) -> SGD.Grad
gradOn model para (xs, ys) = SGD.fromLogList $
    [ (unFeatIx ix, L.fromPos val)
    | (ft, val) <- presentFeats xs ys
    , ix <- maybeToList (index curr ft) ] ++
    [ (unFeatIx ix, L.fromNeg val)
    | (ft, val) <- I.expectedFeatures curr xs
    , ix <- maybeToList (index curr ft) ]
  where
    curr = model { values = para }


notify
    :: SGD.SgdArgs -> Model
    -> SGD.Dataset (Xs, Ys)         -- ^ Training dataset
    -> SGD.Dataset (Xs, Ys)         -- ^ Evaluaion dataset
    -> SGD.Para -> Int -> IO ()
notify SGD.SgdArgs{..} model trainData evalData para k
    | doneTotal k == doneTotal (k - 1) = putStr "."
    | SGD.size evalData > 0 = do
        x <- I.accuracy (model { values = para }) <$> SGD.loadData evalData
        putStrLn ("\n" ++ "[" ++ show (doneTotal k) ++ "] f = " ++ show x)
    | otherwise =
        putStrLn ("\n" ++ "[" ++ show (doneTotal k) ++ "] f = #")
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


-- | Find the most probable label sequence.
tag :: (Ord a, Ord b) => CRF a b -> Sent a b -> [[b]]
tag CRF{..} sent
    = onWords . decodeLabels codec
    . I.tag model . encodeSent codec
    $ sent
  where
    onWords xs =
        [ unJust codec word x
        | (word, x) <- zip sent xs ]
