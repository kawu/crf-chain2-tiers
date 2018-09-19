module Data.CRF.Chain2.Tiers.Dataset.Codec
( Codec
, empty
, CodecM
, obMax
, lbMax

, encodeWord'Cu
, encodeWord'Cn
, encodeSent'Cu
, encodeSent'Cn
, encodeSent

, encodeWordL'Cu
, encodeWordL'Cn
, encodeSentL'Cu
, encodeSentL'Cn
, encodeSentL

-- , encodeLabels
, decodeLabel
, decodeLabels

, mkCodec
, encodeData
, encodeDataL
, unJust
) where


import Prelude hiding (Word)
import Control.Applicative ((<$>), (<*>), pure)
import Control.Comonad.Store (store)
import Data.Maybe (catMaybes, fromJust)
import Data.Lens.Common (Lens(..), fstLens)
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Control.Monad.Codec as C

import Data.CRF.Chain2.Tiers.Dataset.Internal
import Data.CRF.Chain2.Tiers.Dataset.External


-- | Codec internal data.  The first component is used to
-- encode observations of type a, the second one is used to
-- encode labels of type [b].
type Codec a b =
    ( C.AtomCodec a
    , V.Vector (C.AtomCodec (Maybe b)) )


-- | The maximum internal observation included in the codec.
obMax :: Codec a b -> Ob
obMax =
    let idMax m = M.size m - 1
    in  mkOb . idMax . C.to . fst


-- | The maximum internal labels included in the codec.
lbMax :: Codec a b -> [Lb]
lbMax =
    let idMax m = M.size m - 1
    in  map (mkLb . idMax . C.to) . V.toList . snd


obLens :: Lens (a, b) a
obLens = fstLens


lbLens :: Int -> Lens (a, V.Vector b) b
lbLens k = Lens $ \(a, b) -> store
    (\x -> (a, b V.// [(k, x)]))
    (b V.! k)


--------------------------------------
-- Core
--------------------------------------


-- | The empty codec.  The label parts are initialized with Nothing
-- members, which represent unknown labels.  It is taken in the model
-- implementation into account because it is assigned to the
-- lowest label code and the model assumes that the set of labels
-- is of the {0, ..., 'lbMax'} form.
--
-- Codec depends on the number of layers.
empty :: Ord b => Int -> Codec a b
empty n =
    let withNo = C.execCodec C.empty (C.encode C.idLens Nothing)
    in  (C.empty, V.replicate n withNo)


-- | Type synonym for the codec monad.
type CodecM a b c = C.Codec (Codec a b) c


-- | Encode the observation and update the codec (only in the encoding
-- direction).
encodeObU :: Ord a => a -> CodecM a b Ob
encodeObU = fmap mkOb . C.encode' obLens


-- | Encode the observation and do *not* update the codec.
encodeObN :: Ord a => a -> CodecM a b (Maybe Ob)
encodeObN = fmap (fmap mkOb) . C.maybeEncode obLens


-- | Encode the label and update the codec.
encodeLbU :: Ord b => [b] -> CodecM a b Cb
encodeLbU xs = mkCb <$> sequence
    [ mkLb <$> C.encode (lbLens k) (Just x)
    | (x, k) <- zip xs [0..] ]


-- | Encode the label and do *not* update the codec.
-- In case the label is not a member of the codec,
-- return the label code assigned to Nothing label.
encodeLbN :: Ord b => [b] -> CodecM a b Cb
encodeLbN xs =
    let encode lens x = C.maybeEncode lens (Just x) >>= \mx -> case mx of
            Just x' -> return x'
            Nothing -> fromJust <$> C.maybeEncode lens Nothing
    in  mkCb <$> sequence
            [ mkLb <$> encode (lbLens k) x
            | (x, k) <- zip xs [0..] ]


-- | Decode the label within the codec monad.
decodeLbC :: Ord b => Cb -> CodecM a b (Maybe [b])
decodeLbC xs = sequence <$> sequence
    [ C.decode (lbLens k) (unLb x)
    | (x, k) <- zip (unCb xs) [0..] ]


-- | Is label a member of the codec?
hasLabel :: Ord b => Codec a b -> [b] -> Bool
hasLabel cdc xs = and
        [ M.member
            (Just x)
            (C.to $ snd cdc V.! k)
        | (x, k) <- zip xs [0..] ]


--------------------------------------
-- On top of core
--------------------------------------


-- | Encode the labeled word and update the codec.
encodeWordL'Cu :: (Ord a, Ord b) => WordL a b -> CodecM a b (X, Y)
encodeWordL'Cu (word, choice) = do
    x' <- mapM encodeObU (S.toList (obs word))
    r' <- mapM encodeLbU (S.toList (lbs word))
    let x = mkX x' r'
    y  <- mkY <$> sequence
    	[ (,) <$> encodeLbU lb <*> pure pr
	| (lb, pr) <- (M.toList . unProb) choice ]
    return (x, y)

-- | Encodec the labeled word and do *not* update the codec.
encodeWordL'Cn :: (Ord a, Ord b) => WordL a b -> CodecM a b (X, Y)
encodeWordL'Cn (word, choice) = do
    x' <- catMaybes <$> mapM encodeObN (S.toList (obs word))
    r' <- mapM encodeLbN (S.toList (lbs word))
    let x = mkX x' r'
    y  <- mkY <$> sequence
    	[ (,) <$> encodeLbN lb <*> pure pr
	| (lb, pr) <- (M.toList . unProb) choice ]
    return (x, y)

-- | Encode the word and update the codec.
encodeWord'Cu :: (Ord a, Ord b) => Word a b -> CodecM a b X
encodeWord'Cu word = do
    x' <- mapM encodeObU (S.toList (obs word))
    r' <- mapM encodeLbU (S.toList (lbs word))
    return $ mkX x' r'

-- | Encode the word and do *not* update the codec.
encodeWord'Cn :: (Ord a, Ord b) => Word a b -> CodecM a b X
encodeWord'Cn word = do
    x' <- catMaybes <$> mapM encodeObN (S.toList (obs word))
    r' <- mapM encodeLbN (S.toList (lbs word))
    return $ mkX x' r'

-- | Encode the labeled sentence and update the codec.
encodeSentL'Cu :: (Ord a, Ord b) => SentL a b -> CodecM a b (Xs, Ys)
encodeSentL'Cu sent = do
    ps <- mapM (encodeWordL'Cu) sent
    return (V.fromList (map fst ps), V.fromList (map snd ps))

-- | Encode the labeled sentence and do *not* update the codec.
-- Substitute the default label for any label not present in the codec.
encodeSentL'Cn :: (Ord a, Ord b) => SentL a b -> CodecM a b (Xs, Ys)
encodeSentL'Cn sent = do
    ps <- mapM (encodeWordL'Cn) sent
    return (V.fromList (map fst ps), V.fromList (map snd ps))

-- -- | Encode labels into an ascending vector of distinct label codes.
-- encodeLabels :: Ord b => Codec a b -> [b] -> AVec Lb
-- encodeLabels codec = mkAVec . C.evalCodec codec . mapM encodeLbN

-- | Encode the labeled sentence with the given codec.  Substitute the
-- default label for any label not present in the codec.
encodeSentL :: (Ord a, Ord b) => Codec a b -> SentL a b -> (Xs, Ys)
encodeSentL codec = C.evalCodec codec . encodeSentL'Cn

-- | Encode the sentence and update the codec.
encodeSent'Cu :: (Ord a, Ord b) => Sent a b -> CodecM a b Xs
encodeSent'Cu = fmap V.fromList . mapM encodeWord'Cu

-- | Encode the sentence and do *not* update the codec.
encodeSent'Cn :: (Ord a, Ord b) => Sent a b -> CodecM a b Xs
encodeSent'Cn = fmap V.fromList . mapM encodeWord'Cn

-- | Encode the sentence using the given codec.
encodeSent :: (Ord a, Ord b) => Codec a b -> Sent a b -> Xs
encodeSent codec = C.evalCodec codec . encodeSent'Cn

-- | Create codec on the basis of the labeled dataset.
mkCodec :: (Ord a, Ord b) => Int -> [SentL a b] -> Codec a b
mkCodec n = C.execCodec (empty n) . mapM_ encodeSentL'Cu

-- | Encode the labeled dataset using the codec.  Substitute the default
-- label for any label not present in the codec.
encodeDataL :: (Ord a, Ord b) => Codec a b -> [SentL a b] -> [(Xs, Ys)]
encodeDataL = map . encodeSentL

-- | Encode the dataset with the codec.
encodeData :: (Ord a, Ord b) => Codec a b -> [Sent a b] -> [Xs]
encodeData = map . encodeSent

-- | Decode the label.
decodeLabel :: Ord b => Codec a b -> Cb -> Maybe [b]
decodeLabel codec = C.evalCodec codec . decodeLbC

-- | Decode the sequence of labels.
decodeLabels :: Ord b => Codec a b -> [Cb] -> [Maybe [b]]
decodeLabels codec = C.evalCodec codec . mapM decodeLbC

-- | Return the label when 'Just' or one of the unknown values
-- when 'Nothing'.
unJust :: Ord b => Codec a b -> Word a b -> Maybe [b] -> [b]
unJust _ _ (Just x) = x
unJust codec word Nothing = case allUnk of
    (x:_)   -> x
    []      -> error "unJust: Nothing and all values known"
  where
    allUnk = filter (not . hasLabel codec) (S.toList $ lbs word)

-- -- | Replace 'Nothing' labels with all unknown labels from
-- -- the set of potential interpretations.
-- unJusts :: Ord b => Codec a b -> Word a b -> [Maybe b] -> [b]
-- unJusts codec word xs =
--     concatMap deJust xs
--   where
--     allUnk = filter (not . hasLabel codec) (S.toList $ lbs word)
--     deJust (Just x) = [x]
--     deJust Nothing  = allUnk
