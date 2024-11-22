module HBS2.Merkle.Walk where

import Codec.Serialise
import Control.Exception
import Control.Monad
import Control.Monad.Except
import Control.Monad.Fix
import Control.Monad.Trans.Class
import Data.Bool
import Data.ByteString.Lazy qualified as BSL
import Data.Foldable
import Data.Functor
import Data.String.Conversions (cs)
import GHC.Generics (Generic)
import Prettyprinter
import Streaming (Of (..), Stream)
import Streaming qualified as S
import Streaming.ByteString qualified as Q
import Streaming.Prelude qualified as S

import HBS2.Base58
import HBS2.Data.Types.Refs
import HBS2.Hash
import HBS2.Merkle

walkMerkleDem
    :: forall a m
     . (Serialise (MTree a), Serialise (MTreeAnn a), Serialise a, Monad m)
    => Hash HbSync
    -> (Hash HbSync -> m (Maybe BSL.ByteString))
    -> (Either WalkMerkleError a -> m ())
    -> m ()
walkMerkleDem h flookup sink = walkMerkleV2 flookup sink h

walkMerkleV2
    :: forall a m
     . (Serialise (MTree a), Serialise (MTreeAnn a), Serialise a, Monad m)
    => (Hash HbSync -> m (Maybe BSL.ByteString))
    -> (Either WalkMerkleError a -> m ())
    -> Hash HbSync
    -> m ()
walkMerkleV2 flookup sink =
    either (sink . Left) pure <=< runExceptT . go
  where
    go :: Hash HbSync -> ExceptT WalkMerkleError m ()
    go = fix \go' hash -> do
        bs <-
            maybe (throwError $ MerkleHashNotFound hash) pure
                =<< lift (flookup hash)
        either
            (throwError . MerkleDeserialiseFailure hash)
            (runWithTree (withMLeaf (lift . sink . Right)) (traverse_ go'))
            ( (deserialiseOrFail @(MTree a) bs)
                <> (deserialiseOrFail bs <&> \(MTreeAnn {_mtaTree = t}) -> t)
            )

data WalkStep a
    = WalkLeaf a
    | WalkProcessedTree (Hash HbSync)
    | WalkSkippedTree (Hash HbSync)

walkMerkleConditional
    :: forall a m
     . (Serialise (MTree a), Serialise (MTreeAnn a), Monad m)
    => (Hash HbSync -> m (Maybe BSL.ByteString))
    -> (Hash HbSync -> m Bool)
    -> (Either WalkMerkleError (WalkStep a) -> m ())
    -> Hash HbSync
    -> m ()
walkMerkleConditional getB p sink =
    either (sink . Left) pure <=< runExceptT . go
  where
    go :: Hash HbSync -> ExceptT WalkMerkleError m ()
    go = fix \go' h ->
        (lift . p) h >>= bool (sinkRight (WalkSkippedTree h)) do
            bs <- maybe (throwError $ MerkleHashNotFound h) pure =<< (lift . getB) h
            either
                (throwError . MerkleDeserialiseFailure h)
                (runWithTree (withMLeaf (sinkRight . WalkLeaf)) (traverse_ go'))
                ( (deserialiseOrFail @(MTree a) bs)
                    <> (deserialiseOrFail bs <&> \(MTreeAnn {_mtaTree = t}) -> t)
                )
            sinkRight (WalkProcessedTree h)
      where
        sinkRight = lift . sink . Right

withMLeaf :: (Applicative m) => (a -> m ()) -> MTree a -> m ()
withMLeaf f = \case
    MLeaf s -> f s
    MNode _ _ -> pure ()

runWithTree
    :: forall m a
     . (Monad m)
    => (MTree a -> m ())
    -> ([Hash HbSync] -> m ())
    -> MTree a
    -> m ()
runWithTree f run = \case
    n@(MLeaf _) -> f n
    n@(MNode _ hashes) -> f n >> run hashes

type WalkMerkleError = WalkMerkleError' (Hash HbSync)
data WalkMerkleError' h
    = MerkleHashNotFound h
    | MerkleDeserialiseFailure h DeserialiseFailure
    deriving (Generic)

deriving instance Show (WalkMerkleError' (AsBase58 (Hash HbSync)))

deriving via
    (WalkMerkleError' (AsBase58 (Hash HbSync)))
    instance
        Show (WalkMerkleError' (Hash HbSync))

instance Show (AsBase58 (Hash HbSync)) where
    show (AsBase58 h) = show $ show $ pretty $ h

instance Exception WalkMerkleError

---

streamMerkleConditionalEither
    :: forall a m
     . (Serialise a, Monad m)
    => (Hash HbSync -> m (Maybe BSL.ByteString))
    -> (Hash HbSync -> m Bool)
    -> Hash HbSync
    -> Stream (Of (WalkStep a)) m (Either WalkMerkleError ())
streamMerkleConditionalEither getB p h =
    (runExceptT . S.distribute) do
        streamMerkleConditional (lift . getB) (lift . p) h

streamMerkleConditional
    :: forall a m
     . ( Serialise a
       , Monad m
       , MonadError WalkMerkleError m
       )
    => (Hash HbSync -> m (Maybe BSL.ByteString))
    -> (Hash HbSync -> m Bool)
    -> Hash HbSync
    -> Stream (Of (WalkStep a)) m ()
streamMerkleConditional getB p = do
    walkMerkleConditional (lift . getB) (lift . p) \case
        Left hashNotFound -> throwError hashNotFound
        Right as -> S.yield as

streamMerkle
    :: forall a m
     . (Serialise a, Monad m)
    => (Hash HbSync -> m (Maybe BSL.ByteString))
    -> Hash HbSync
    -> Stream (Of a) m (Either WalkMerkleError ())
streamMerkle getB rv = (runExceptT . S.distribute) do
    streamMerkle' getB rv

-- | Работает для деревьев, у которых в узлах лежат [a]
streamMerkle'
    :: forall a m
     . (Serialise a, Monad m)
    => (Hash HbSync -> m (Maybe BSL.ByteString))
    -> Hash HbSync
    -> Stream (Of a) (ExceptT WalkMerkleError m) ()
streamMerkle' getB = do
    walkMerkleV2 @[a] getB' \case
        Left hashNotFound -> throwError hashNotFound
        Right as -> S.each as
  where
    getB' :: Hash HbSync -> Stream (Of a) (ExceptT WalkMerkleError m) (Maybe BSL.ByteString)
    getB' = lift . lift . getB

streamCatFromMerkle
    :: (Monad m)
    => (Hash HbSync -> m (Maybe BSL.ByteString))
    -> Hash HbSync
    -> Stream (Of BSL.ByteString) m (Either WalkMerkleError ())
streamCatFromMerkle getB =
    fmap join
        . (runExceptT . S.distribute)
        . S.mapM
            ( \(HashRef h) ->
                maybe (throwError (MerkleHashNotFound h)) pure
                    =<< (lift . getB) h
            )
        . streamMerkle (lift . getB)

catFromMerkle
    :: (Monad m)
    => (Hash HbSync -> m (Maybe BSL.ByteString))
    -> Hash HbSync
    -> m (Either WalkMerkleError BSL.ByteString)
catFromMerkle getB =
    fmap (\(bs S.:> ehu) -> const bs <$> ehu)
        . Q.toLazy
        . Q.fromChunks
        . (S.map cs . streamCatFromMerkle getB)

streamToListEither
    :: (Monad m)
    => Stream (Of a) m (Either e ())
    -> m (Either e [a])
streamToListEither = fmap runStreamOfA . S.toList

runStreamOfA :: (Functor m) => Of a (m ()) -> m a
runStreamOfA (a S.:> e) = a <$ e

---

streamMerkle1
    :: forall a m
     . (Serialise a, Monad m)
    => (Hash HbSync -> m (Maybe BSL.ByteString))
    -> Hash HbSync
    -> Stream (Of a) m (Either WalkMerkleError ())
streamMerkle1 getB rv = (runExceptT . S.distribute) do
    streamMerkle1' getB rv

streamMerkle1'
    :: forall a m
     . (Serialise a, Monad m)
    => (Hash HbSync -> m (Maybe BSL.ByteString))
    -> Hash HbSync
    -> Stream (Of a) (ExceptT WalkMerkleError m) ()
streamMerkle1' getB hash = do
    walkMerkle hash getB' \case
        Left hashNotFound -> throwError (MerkleHashNotFound hashNotFound)
        Right as -> S.each as
  where
    getB' :: Hash HbSync -> Stream (Of a) (ExceptT WalkMerkleError m) (Maybe BSL.ByteString)
    getB' = lift . lift . getB

streamCatFromMerkle1
    :: (Monad m)
    => (Hash HbSync -> m (Maybe BSL.ByteString))
    -> Hash HbSync
    -> Stream (Of BSL.ByteString) m (Either WalkMerkleError ())
streamCatFromMerkle1 getB =
    fmap join
        . (runExceptT . S.distribute)
        . S.mapM
            ( \(HashRef h) ->
                maybe (throwError (MerkleHashNotFound h)) pure
                    =<< (lift . getB) h
            )
        . streamMerkle1 (lift . getB)

catFromMerkle1
    :: (Monad m)
    => (Hash HbSync -> m (Maybe BSL.ByteString))
    -> Hash HbSync
    -> m (Either WalkMerkleError BSL.ByteString)
catFromMerkle1 getB =
    fmap (\(bs S.:> ehu) -> const bs <$> ehu)
        . Q.toLazy
        . Q.fromChunks
        . (S.map cs . streamCatFromMerkle1 getB)
