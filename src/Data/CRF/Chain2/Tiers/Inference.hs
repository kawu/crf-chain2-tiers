{-# LANGUAGE RecordWildCards #-}

module Data.CRF.Chain2.Tiers.Inference
( tag
-- , probs
, marginals
, expectedFeatures
, accuracy
, zx
, zx'
) where

import           Data.Ord (comparing)
import           Data.List (maximumBy)
import qualified Data.Array as A
import qualified Data.Vector as V
import qualified Data.Number.LogFloat as L

import           Control.Parallel.Strategies (rseq, parMap)
import           Control.Parallel (par, pseq)
import           GHC.Conc (numCapabilities)

import           Data.CRF.Chain2.Tiers.Dataset.Internal
import           Data.CRF.Chain2.Tiers.Model
import           Data.CRF.Chain2.Tiers.Feature
import           Data.CRF.Chain2.Tiers.Util (partition)
import qualified Data.CRF.Chain2.Tiers.DP as DP


-- Interface on top of internal implementation

-- | Accumulation function.
type AccF = [L.LogFloat] -> L.LogFloat

type ProbArray = CbIx -> CbIx -> CbIx -> L.LogFloat

computePsi :: Model -> Xs -> Int -> CbIx -> L.LogFloat
computePsi crf xs i = (A.!) $ A.array (0, lbNum xs i - 1)
    [ (k, onWord crf xs i k)
    | k <- lbIxs xs i ]

forward :: AccF -> Model -> Xs -> ProbArray
forward acc crf sent = alpha where
    alpha = DP.flexible3 (-1, V.length sent - 1)
                (\i   -> (0, lbNum sent i - 1))
                (\i _ -> (0, lbNum sent (i - 1) - 1))
                (\t i -> withMem (computePsi crf sent i) t i)
    withMem psi alpha i j k
        | i == -1 = 1.0
        | otherwise = acc
            [ alpha (i - 1) k h * psi j
            * onTransition crf sent i j k h
            | h <- lbIxs sent (i - 2) ]

backward :: AccF -> Model -> Xs -> ProbArray
backward acc crf sent = beta where
    beta = DP.flexible3 (0, V.length sent)
               (\i   -> (0, lbNum sent (i - 1) - 1))
               (\i _ -> (0, lbNum sent (i - 2) - 1))
               (\t i -> withMem (computePsi crf sent i) t i)
    withMem psi beta i j k
        | i == V.length sent = 1.0
        | otherwise = acc
            [ beta (i + 1) h j * psi h
            * onTransition crf sent i h j k
            | h <- lbIxs sent i ]

zxBeta :: ProbArray -> L.LogFloat
zxBeta beta = beta 0 0 0

zxAlpha :: AccF -> Xs -> ProbArray -> L.LogFloat
zxAlpha acc sent alpha = acc
    [ alpha (n - 1) i j
    | i <- lbIxs sent (n - 1)
    , j <- lbIxs sent (n - 2) ]
    where n = V.length sent

-- | Normalization factor computed for the Xs sentence using the
-- backward computation.
zx :: Model -> Xs -> L.LogFloat
zx crf = zxBeta . backward sum crf

-- | Normalization factor computed for the Xs sentence using the
-- forward computation.
zx' :: Model -> Xs -> L.LogFloat
zx' crf sent = zxAlpha sum sent (forward sum crf sent)

argmax :: (Ord b) => (a -> b) -> [a] -> (a, b)
argmax f l = foldl1 choice $ map (\x -> (x, f x)) l
    where choice (x1, v1) (x2, v2)
              | v1 > v2 = (x1, v1)
              | otherwise = (x2, v2)

tagIxs :: Model -> Xs -> [Int]
tagIxs crf sent = collectMaxArg (0, 0, 0) [] mem where
    mem = DP.flexible3 (0, V.length sent)
                       (\i   -> (0, lbNum sent (i - 1) - 1))
                       (\i _ -> (0, lbNum sent (i - 2) - 1))
                       (\t i -> withMem (computePsi crf sent i) t i)
    withMem psiMem mem i j k
        | i == V.length sent = (-1, 1)
        | otherwise = argmax eval $ lbIxs sent i
        where eval h =
                  (snd $ mem (i + 1) h j) * psiMem h
                  * onTransition crf sent i h j k
    collectMaxArg (i, j, k) acc mem =
        collect $ mem i j k
        where collect (h, _)
                  | h == -1 = reverse acc
                  | otherwise = collectMaxArg (i + 1, h, j) (h:acc) mem

-- | Find the most probable label sequence satisfying the constraints
-- imposed over label values.
tag :: Model -> Xs -> [Cb]
tag crf sent =
    let ixs = tagIxs crf sent
    in  [lbAt x i | (x, i) <- zip (V.toList sent) ixs]

-- -- | Tag labels with corresponding probabilities.
-- TODO: doesn't work, crashes with "Data.Number.LogFloat.negate:
-- argument out of range" for some reason.
-- probs :: Model -> Xs -> [[L.LogFloat]]
-- probs crf sent =
--     let alpha = forward maximum crf sent
--         beta = backward maximum crf sent
--         normalize xs =
--             let d = - sum xs
--             in map (*d) xs
--         m1 k x = maximum
--             [ alpha k x y * beta (k + 1) x y
--             | y <- lbIxs sent (k - 1) ]
--     in  [ normalize [m1 i k | k <- lbIxs sent i]
--         | i <- [0 .. V.length sent - 1] ]

-- | Tag potential labels with marginal probabilities.
marginals :: Model -> Xs -> [[L.LogFloat]]
marginals crf sent =
    let alpha = forward sum crf sent
        beta = backward sum crf sent
    in  [ [ prob1 crf alpha beta sent i k
          | k <- lbIxs sent i ]
        | i <- [0 .. V.length sent - 1] ]

goodAndBad :: Model -> Xs -> Ys -> (Int, Int)
goodAndBad crf xs ys =
    foldl gather (0, 0) $ zip labels labels'
  where
    labels  = [ (best . unY) (ys V.! i)
              | i <- [0 .. V.length ys - 1] ]
    best zs
        | null zs   = Nothing
        | otherwise = Just . fst $ maximumBy (comparing snd) zs
    labels' = map Just $ tag crf xs
    gather (good, bad) (x, y)
        | x == y = (good + 1, bad)
        | otherwise = (good, bad + 1)

goodAndBad' :: Model -> [(Xs, Ys)] -> (Int, Int)
goodAndBad' crf dataset =
    let add (g, b) (g', b') = (g + g', b + b')
    in  foldl add (0, 0) [goodAndBad crf x y | (x, y) <- dataset]

-- | Compute the accuracy of the model with respect to the labeled dataset.
accuracy :: Model -> [(Xs, Ys)] -> Double
accuracy crf dataset =
    let k = numCapabilities
    	parts = partition k dataset
        xs = parMap rseq (goodAndBad' crf) parts
        (good, bad) = foldl add (0, 0) xs
        add (g, b) (g', b') = (g + g', b + b')
    in  fromIntegral good / fromIntegral (good + bad)

prob3
    :: Model -> ProbArray -> ProbArray -> Xs
    -> Int -> (CbIx -> L.LogFloat) -> CbIx -> CbIx -> CbIx
    -> L.LogFloat
prob3 crf alpha beta sent k psiMem x y z =
    alpha (k - 1) y z * beta (k + 1) x y * psiMem x
    * onTransition crf sent k x y z / zxBeta beta
{-# INLINE prob3 #-}

prob2
    :: Model -> ProbArray -> ProbArray
    -> Xs -> Int -> CbIx -> CbIx -> L.LogFloat
prob2 _ alpha beta _ k x y =
    alpha k x y * beta (k + 1) x y / zxBeta beta
{-# INLINE prob2 #-}

prob1
    :: Model -> ProbArray -> ProbArray
    -> Xs -> Int -> CbIx -> L.LogFloat
prob1 crf alpha beta sent k x = sum
    [ prob2 crf alpha beta sent k x y
    | y <- lbIxs sent (k - 1) ]

expectedFeaturesOn
    :: Model -> ProbArray -> ProbArray
    -> Xs -> Int -> [(Feat, L.LogFloat)]
expectedFeaturesOn crf alpha beta sent k =
    fs3 ++ fs1
    where psi = computePsi crf sent k
          pr1 = prob1 crf alpha beta sent k
          pr3 = prob3 crf alpha beta sent k psi
          fs1 = [ (ft, pr) 
                | a <- lbIxs sent k
                , let pr = pr1 a
                , ft <- obFs a ]
    	  fs3 = [ (ft, pr) 
                | a <- lbIxs sent k
                , b <- lbIxs sent $ k - 1
                , c <- lbIxs sent $ k - 2
                , let pr = pr3 a b c
                , ft <- trFs a b c ]
          obFs = obFeatsOn sent k
          trFs = trFeatsOn sent k

-- | A list of features (represented by feature indices) defined within
-- the context of the sentence accompanied by expected probabilities
-- determined on the basis of the model.
--
-- One feature can occur multiple times in the output list.
expectedFeatures :: Model -> Xs -> [(Feat, L.LogFloat)]
expectedFeatures crf sent =
    -- force parallel computation of alpha and beta tables
    zx1 `par` zx2 `pseq` zx1 `pseq` concat
      [ expectedFeaturesOn crf alpha beta sent k
      | k <- [0 .. V.length sent - 1] ]
    where alpha = forward sum crf sent
          beta = backward sum crf sent
          zx1 = zxAlpha sum sent alpha
          zx2 = zxBeta beta
