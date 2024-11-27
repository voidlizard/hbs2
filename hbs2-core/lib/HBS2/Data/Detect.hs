module HBS2.Data.Detect where

import HBS2.Prelude.Plated
import HBS2.Hash
import HBS2.Data.Types
import HBS2.Merkle
import HBS2.Merkle.Walk
import HBS2.Storage

-- import HBS2.System.Logger.Simple

-- import Data.Foldable (for_)
import Control.Monad.Trans.Maybe
import Codec.Serialise (deserialiseOrFail)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Either
-- import Data.Function
-- import Data.Functor

import Data.Coerce
import Data.Maybe
import Control.Concurrent.STM
import Data.HashMap.Strict qualified as HashMap
-- import Data.HashMap.Strict (HashMap)
import Data.List qualified as List
import UnliftIO qualified

import Streaming.Prelude qualified as S
-- import Streaming qualified as S

data BlobType =  Merkle (MTree [HashRef])
               | MerkleAnn (MTreeAnn [HashRef])
               | AnnRef AnnotatedHashRef
               | SeqRef SequentialRef
               | Blob (Hash HbSync)
               deriving (Show,Data)


tryDetect :: Hash HbSync -> ByteString -> BlobType
tryDetect hash obj = rights [mbAnn, mbLink, mbMerkle, mbSeq] & headDef orBlob

  where
    mbLink   = deserialiseOrFail @AnnotatedHashRef obj <&> AnnRef
    mbMerkle = deserialiseOrFail @(MTree [HashRef]) obj <&> Merkle
    mbSeq    = deserialiseOrFail @SequentialRef obj <&> SeqRef
    mbAnn    = deserialiseOrFail obj <&> MerkleAnn
    orBlob   = Blob hash

data ScanLevel = ScanShallow | ScanDeep

extractBlockRefs :: Hash HbSync -> ByteString -> [Hash HbSync]
extractBlockRefs hx bs =
  case tryDetect hx bs of
   (SeqRef (SequentialRef _ (AnnotatedHashRef a' b))) ->
    coerce <$> catMaybes [a', Just b]

   AnnRef (AnnotatedHashRef ann h) -> do
    coerce <$> catMaybes [ann, Just h]

   Merkle (MNode _ hs) -> hs

   MerkleAnn (MTreeAnn{..}) -> do

     let meta = case _mtaMeta of
                  AnnHashRef ha -> [ha]
                  _ -> mempty

     let c = case _mtaCrypt of
               CryptAccessKeyNaClAsymm hs -> [hs]
               EncryptGroupNaClSymm1 hs _ -> [hs]
               EncryptGroupNaClSymm2 _ hs _ -> [hs]
               _ -> mempty

     let t = case _mtaTree of
               MNode _ hs -> hs
               _ -> mempty

     meta <> c <> t

   _ -> mempty



-- TODO: control-nesting-level-to-avoid-abuse

-- TODO: switch-all-merkle-walks-to-deep-scan

-- TODO: asap-make-it-support-encryption
--   Передавать параметры расшифровки через тайпкласс
--   Сделать реализацию тайпкласса для MonadIO по умолчанию,
--   будет возращать блоки как есть
--

deepScan :: MonadIO m
         => ScanLevel
         -> ( Hash HbSync -> m () )                 -- ^ missed block handler
         -> Hash HbSync                             -- ^ root
         -> ( Hash HbSync -> m (Maybe ByteString) ) -- ^ block reading function
         -> ( Hash HbSync -> m () )                 -- ^ sink function
         -> m ()
deepScan l miss from reader sink = do
  tv <- liftIO $ newTVarIO mempty

  deepScan_ tv (HashRef from)

  where

    deepScan_ tv item = do

      here <- reader (fromHashRef item) <&> isJust

      when here do
        sink (fromHashRef item)

      void $ runMaybeT $ do
        blk <- MaybeT $ reader (fromHashRef item)

        let what = tryDetect (fromHashRef item) blk

        case what of
         Blob{} -> pure ()

         Merkle t -> do
          lift $ walkTree t

         MerkleAnn ann -> case  _mtaCrypt ann of
          NullEncryption -> do
            lift $ walkTree (_mtaTree ann)

          -- FIXME: ASAP-support-encryption
          CryptAccessKeyNaClAsymm{} -> do
            lift $ walkTree (_mtaTree ann)

          EncryptGroupNaClSymm{} -> do
            lift $ walkTree (_mtaTree ann)

         SeqRef (SequentialRef _ (AnnotatedHashRef ann hx)) -> do
           lift $ maybe1 ann (pure ()) sinkDeep
           lift $ sinkDeep hx

         AnnRef (AnnotatedHashRef ann hx) -> do
           lift $ maybe1 ann (pure ()) sinkDeep
           lift $ sinkDeep hx

      where

        deep = case l of
                 ScanDeep -> True
                 _        -> False

        sinkDeep h = do
          visited <- liftIO $ readTVarIO tv <&> HashMap.member h
          unless visited do
            liftIO $ atomically $ modifyTVar tv (HashMap.insert h ())
            sinkDeep_ h

        sinkDeep_ h | deep = deepScan_ tv h
                    | otherwise = walk (fromHashRef h)

        stepInside = \case
          Left x -> miss x
          Right ( hxx :: [HashRef] ) -> do
            for_ hxx sinkDeep

        walkTree t = do
          walkMerkleTree t reader stepInside

        walk h = walkMerkle h reader stepInside

readBlobFromTree :: forall m . ( MonadIO m )
                 => ( Hash HbSync -> IO (Maybe ByteString) )
                 -> HashRef
                 -> m (Maybe ByteString)

readBlobFromTree readBlock hr = do

  pieces <- S.toList_ $
              deepScan ScanDeep (const $ S.yield Nothing) (fromHashRef hr) (liftIO . readBlock) $ \ha -> do
                unless (fromHashRef hr == ha) do
                  liftIO (readBlock ha) >>= S.yield

  pure $ LBS.concat <$> sequence pieces


readLog :: forall m . ( MonadIO m )
        => ( Hash HbSync -> IO (Maybe ByteString) )
        -> HashRef
        -> m [HashRef]
readLog getBlk (HashRef h)  =
  S.toList_ $ do
    walkMerkle h (liftIO . getBlk) $ \hr -> do
      case hr of
        Left{} -> pure ()
        Right (hrr :: [HashRef]) -> S.each hrr

readLogThrow :: forall m . ( MonadIO m )
        => ( Hash HbSync -> IO (Maybe ByteString) )
        -> HashRef
        -> m [HashRef]
readLogThrow getBlk (HashRef h)  =
  S.toList_ do
      either UnliftIO.throwIO pure =<<
          streamMerkle (liftIO . getBlk) h


-- FIXME: make-it-stop-after-first-missed-block
checkComplete :: forall sto m . (MonadIO m, Storage sto HbSync ByteString IO)
              => sto
              -> HashRef
              -> m Bool
checkComplete sto hr = do

  result <- S.toList_ $
              deepScan ScanDeep (const $ S.yield Nothing) (fromHashRef hr) (liftIO . getBlock sto) $ \ha -> do
                here <- liftIO $ hasBlock sto ha
                S.yield here

  pure $ maybe False (not . List.null)  $ sequence result

