{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternGuards #-}

module Data.CRF.Chain2.Tiers
( CRF (..)
, train
, tag

, module Data.CRF.Chain2.Tiers.Dataset.External
, module Data.CRF.Chain2.Tiers.Feature
) where


import           System.IO (hSetBuffering, stdout, BufferMode (..))
import           Control.Applicative ((<$>), (<*>))
import           Data.Maybe (maybeToList)
import           Data.Binary (Binary, get, put)
import qualified Data.Vector as V
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


----------------------------------------------------
-- Training
----------------------------------------------------


-- | Train the CRF using the stochastic gradient descent method.
-- When the evaluation data 'IO' action is 'Just', the iterative
-- training process will notify the user about the current accuracy
-- on the evaluation part every full iteration over the training part.
train
    :: (Ord a, Ord b)
    => Int                          -- ^ Number of layers
    -> SGD.SgdArgs                  -- ^ Args for SGD
    -> FeatSel                      -- ^ Feature selection
    -> IO [SentL a b]               -- ^ Training data 'IO' action
    -> Maybe (IO [SentL a b])       -- ^ Maybe evalation data
    -> IO (CRF a b)                 -- ^ Resulting codec and model
train numOfLayers sgdArgs ftSel trainIO evalIO'Maybe = do
    hSetBuffering stdout NoBuffering
    (codec, trainData) <- mkCodec numOfLayers <$> trainIO
    evalDataM <- case evalIO'Maybe of
        Just evalIO -> Just . encodeDataL codec <$> evalIO
        Nothing     -> return Nothing
    let model = mkModel ftSel trainData
    para <- SGD.sgdM sgdArgs
        (notify sgdArgs model trainData evalDataM)
        (gradOn model) (V.fromList trainData) (values model)
    return $ CRF
        numOfLayers
        codec
        model { values = para }


gradOn :: Model -> SGD.Para -> (Xs, Ys) -> SGD.Grad
gradOn model para (xs, ys) = SGD.fromLogList $
    [ (ix, L.fromPos val)
    | (ft, val) <- presentFeats xs ys
    , FeatIx ix <- maybeToList (index curr ft) ] ++
    [ (ix, L.fromNeg val)
    | (ft, val) <- I.expectedFeatures curr xs
    , FeatIx ix <- maybeToList (index curr ft) ]
  where
    curr = model { values = para }


notify
    :: SGD.SgdArgs -> Model -> [(Xs, Ys)] -> Maybe [(Xs, Ys)]
    -> SGD.Para -> Int -> IO ()
notify SGD.SgdArgs{..} model trainData evalDataM para k 
    | doneTotal k == doneTotal (k - 1) = putStr "."
    | Just dataSet <- evalDataM = do
        let x = I.accuracy (model { values = para }) dataSet
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
    trainSize = length trainData


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
