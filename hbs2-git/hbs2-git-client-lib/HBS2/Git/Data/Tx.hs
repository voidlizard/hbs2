module HBS2.Git.Data.Tx
  ( module HBS2.Git.Data.Tx
  , OperationError(..)
  ) where

import HBS2.Git.Client.Prelude
import HBS2.Git.Data.RefLog

import HBS2.Defaults
import HBS2.Data.Detect
import HBS2.KeyMan.Keys.Direct
import HBS2.Peer.Proto
import HBS2.Net.Auth.GroupKeySymm
import HBS2.Net.Auth.Credentials
import HBS2.Storage.Operations.ByteString
import HBS2.Storage.Operations.Missed

import HBS2.Git.Data.GK

import HBS2.Git.Local


import Data.Maybe
import Data.Either
import Data.Word
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString (ByteString)
import Streaming.Prelude qualified as S
import Data.Binary.Get
import Data.ByteArray.Hash (SipHash(..), SipKey(..))
import Data.ByteArray.Hash qualified as BA
import Data.HashMap.Strict qualified as HM

type Rank = Integer


type LBS = LBS.ByteString

type RepoTx = RefLogUpdate L4Proto

data RepoHeadType = RepoHeadType1
                    deriving stock (Enum,Generic)

data RepoHeadExt = RepoHeadExt
                   deriving stock Generic

data RepoHead =
  RepoHeadSimple
  { _repoHeadType   :: RepoHeadType
  , _repoHeadTime   :: Word64
  , _repoHeadGK0    :: Maybe HashRef
  , _repoHeadName   :: Text
  , _repoHeadBrief  :: Text
  , _repoManifest   :: Maybe Text
  , _repoHeadRefs   :: [(GitRef, GitHash)]
  , _repoHeadExt    :: [RepoHeadExt]
  }
  deriving stock (Generic)


instance Serialise RepoHeadType
instance Serialise RepoHeadExt
instance Serialise RepoHead

data TxKeyringNotFound = TxKeyringNotFound
                         deriving stock (Show, Typeable, Generic)

instance Exception TxKeyringNotFound

class GroupKeyOperations m where
  openGroupKey :: GK0 -> m (Maybe GroupSecret)
  loadKeyrings :: HashRef -> m [KeyringEntry HBS2Basic]

makeRepoHeadSimple :: MonadIO m
                   => Text
                   -> Text
                   -> Maybe Text
                   -> Maybe HashRef
                   -> [(GitRef, GitHash)]
                   -> m RepoHead
makeRepoHeadSimple name brief manifest gk refs = do
  t <- getEpoch
  pure $ RepoHeadSimple RepoHeadType1 t gk name brief manifest refs mempty

writeRepoHead :: MonadUnliftIO  m => AnyStorage -> RepoHead -> m HashRef
writeRepoHead sto rh = writeAsMerkle sto (serialise rh) <&> HashRef

makeTx :: forall s m . (MonadUnliftIO m, GroupKeyOperations m, s ~ HBS2Basic)
       => AnyStorage
       -> Bool   -- ^ rewrite bundle merkle tree with new gk0
       -> Rank   -- ^ tx rank
       -> RefLogId
       -> ( PubKey 'Sign s -> m (Maybe (PrivKey 'Sign s) ) )
       -> RepoHead
       -> [HashRef]
       -> [LBS]
       -> m RepoTx

makeTx sto rewrite r puk findSk rh prev lbss = do

  let rfk = RefLogKey @HBS2Basic puk

  privk <- findSk puk
             >>= orThrow TxKeyringNotFound

  -- FIXME: delete-on-fail
  headRef <- writeRepoHead sto rh

  writeEnv <- newWriteBundleEnv sto rh

  cRefs <- for lbss (writeBundle writeEnv)

  let newBundles0 = prev <> cRefs

  newBundles <- do
    if not rewrite then do
      pure newBundles0
    else do
      for newBundles0 \bh -> do

        blk <- getBlock sto (fromHashRef bh)
                 >>= orThrow StorageError

        case tryDetect (fromHashRef bh) blk of

          Merkle{} -> do
            bs <- runExceptT (readFromMerkle sto (SimpleKey (fromHashRef bh)))
                     >>= either throwIO pure

            trace $ "encrypt existed block" <+> pretty bh
            writeBundle writeEnv bs

          MerkleAnn ann@(MTreeAnn {_mtaCrypt = EncryptGroupNaClSymm2 o gkh nonce}) -> do

            gk <- runExceptT (readGK0 sto (HashRef gkh))
                    >>= orThrow (GroupKeyNotFound 4)

            gks <- openGroupKey gk
                     >>= orThrow (GroupKeyNotFound 5)

            debug $ "update GK0 for existed block" <+> pretty bh
            let rcpt = HM.keys (recipients (wbeGk0 writeEnv))
            gk1 <- generateGroupKey @HBS2Basic (Just gks) rcpt

            gk1h <- writeAsMerkle sto (serialise gk1)

            let newCrypt = EncryptGroupNaClSymm2 o gk1h nonce
            let newTreeBlock = ann { _mtaCrypt = newCrypt }

            newTree <- enqueueBlock sto (serialise newTreeBlock)
                         >>= orThrow StorageError

            pure (HashRef newTree)

          _ -> throwIO UnsupportedFormat

  let pt = toPTree (MaxSize defHashListChunk) (MaxNum 256) newBundles

  me <- makeMerkle 0 pt $ \(_,_,bss) -> do
          void $ putBlock sto bss

  let meRef = HashRef me

  -- TODO: post-real-rank-for-tx
  let tx = SequentialRef r (AnnotatedHashRef (Just headRef) meRef)
            & serialise
            & LBS.toStrict

  makeRefLogUpdate @L4Proto @HBS2Basic puk privk tx


unpackTx :: MonadIO m
         => RefLogUpdate L4Proto
         -> m (Integer, HashRef, HashRef)

unpackTx tx = do

  sr <- deserialiseOrFail @SequentialRef (LBS.fromStrict (view refLogUpdData tx))
          & orThrow UnsupportedFormat

  case sr of
   SequentialRef n (AnnotatedHashRef (Just rhh) blkh) -> pure (n,rhh,blkh)
   _ -> throwIO UnsupportedFormat

readTx :: (MonadIO m, MonadError OperationError m)
       => AnyStorage
       -> HashRef
       -> m (Integer, HashRef, RepoHead, HashRef)

readTx sto href = do

  tx <- getBlock sto (fromHashRef href)
           >>= orThrowError MissedBlockError
           <&> deserialiseOrFail @(RefLogUpdate L4Proto)
           >>= orThrowError UnsupportedFormat

  (n,rhh,blkh) <- unpackTx tx

  rh <- runExceptT (readFromMerkle sto (SimpleKey (fromHashRef rhh)))
          >>= orThrowError IncompleteData
          <&> deserialiseOrFail @RepoHead
          >>= orThrowError UnsupportedFormat

  missed <- S.head_ (findMissedBlocks2 sto blkh) <&> isJust

  when missed do
    throwError IncompleteData

  pure (n, rhh, rh, blkh)


readRepoHeadFromTx :: MonadIO m
       => AnyStorage
       -> HashRef
       -> m (Maybe (HashRef, RepoHead))

readRepoHeadFromTx sto href = runMaybeT do

  tx <- getBlock sto (fromHashRef href) >>= toMPlus
           <&> deserialiseOrFail @(RefLogUpdate L4Proto)
           >>= toMPlus

  (n,rhh,_) <- unpackTx tx

  runExceptT (readFromMerkle sto (SimpleKey (fromHashRef rhh)))
    >>= toMPlus
    <&> deserialiseOrFail @RepoHead
    >>= toMPlus
    <&> (rhh,)

readRepoHead :: (MonadIO m, MonadError OperationError m)
             => AnyStorage
             -> HashRef
             -> m RepoHead

readRepoHead sto rhh =
  readFromMerkle sto (SimpleKey (fromHashRef rhh))
    <&> deserialiseOrFail @RepoHead
    >>= \case
           Left{} -> throwError UnsupportedFormat
           Right x -> pure x

data BundleMeta =
  BundleMeta
  { bundleHash      :: HashRef
  , bundleEncrypted :: Bool
  }
  deriving stock (Show,Generic)

data BundleWithMeta =
  BundleWithMeta
  { bundleMeta   :: BundleMeta
  , bundlebBytes :: LBS
  }
  deriving stock (Generic)

readBundle :: (MonadIO m, MonadError OperationError m, GroupKeyOperations m)
           => AnyStorage
           -> RepoHead
           -> HashRef
           -> m BundleWithMeta
readBundle sto rh ref = do

  obj <- getBlock sto (fromHashRef ref)
            >>= orThrow MissedBlockError

  let q = tryDetect (fromHashRef ref) obj

  case q of
    Merkle t -> do
      let meta = BundleMeta ref False
      BundleWithMeta meta <$>
        readFromMerkle sto (SimpleKey key)

    MerkleAnn (MTreeAnn {_mtaCrypt = EncryptGroupNaClSymm gkh _}) -> do
      ke <- loadKeyrings (HashRef gkh)
      let meta = BundleMeta ref True
      BundleWithMeta meta <$> readFromMerkle sto (ToDecryptBS ke key)

    _ -> throwError UnsupportedFormat

  where
    key = fromHashRef ref

readBundleRefs :: (MonadIO m)
               => AnyStorage
               -> HashRef
               -> m (Either [HashRef] [HashRef])

readBundleRefs sto bunh = do
  r <- S.toList_ $
      walkMerkle @[HashRef] (fromHashRef bunh) (getBlock sto) $ \case
        Left h -> S.yield (Left h)
        Right ( bundles :: [HashRef] ) -> do
          mapM_ (S.yield . Right) bundles

  let missed = lefts r

  if not (null missed) then do
    pure (Left (fmap HashRef missed))
  else do
    pure (Right $ rights r)


type GitPack = LBS.ByteString
type UnpackedBundle = (Word32, Word32, [GitHash], GitPack)

unpackPackMay :: LBS.ByteString -> Maybe UnpackedBundle
unpackPackMay co = result $ flip runGetOrFail co do
    w <- getWord32be
    v <- getWord32be
    idx <- lookAheadE (getLazyByteString (fromIntegral w) <&> deserialiseOrFail @[GitHash])
              >>= either (fail.show) pure
    pack <- getRemainingLazyByteString
    pure (w,v,idx,pack)

  where
    result = \case
      Left{} -> Nothing
      Right (_,_,r) -> Just r



data WriteBundleEnv =
  WriteBundleEnvPlain
  { wbeHead :: RepoHead
  , wbeStorage :: AnyStorage
  }
  | WriteBundleEnvEnc
    { wbeSk1     :: SipKey
    , wbeSk2     :: SipKey
    , wbeHead    :: RepoHead
    , wbeGk0     :: GK0
    , wbeGks     :: GroupSecret
    , wbeStorage :: AnyStorage
    }

newWriteBundleEnv :: (MonadIO m, GroupKeyOperations m) => AnyStorage -> RepoHead -> m WriteBundleEnv
newWriteBundleEnv sto rh = case _repoHeadGK0 rh of
  Nothing -> do
    pure $ WriteBundleEnvPlain rh sto

  Just gk0h -> do

    gk0 <- runExceptT (readGK0 sto gk0h)
            >>= either throwIO pure

    gks <- openGroupKey gk0
              >>= orThrow (GroupKeyNotFound 3)

    pure $ WriteBundleEnvEnc
           { wbeSk1     = SipKey 2716370006254639645 507093936407764973
           , wbeSk2     = SipKey 9209704780415729085 272090086441077315
           , wbeHead    = rh
           , wbeGk0     = gk0
           , wbeGks     = gks
           , wbeStorage = sto
           }

makeNonceForBundle :: Monad m => WriteBundleEnv -> LBS.ByteString -> m ByteString
makeNonceForBundle env lbs = do
  let piece = ( LBS.take (fromIntegral defBlockSize * 2) lbs
                <> serialise (wbeHead env)
              ) & hashObject @HbSync & serialise & LBS.drop 1 & LBS.toStrict
  pure piece

writeBundle :: MonadIO m => WriteBundleEnv -> LBS.ByteString -> m HashRef
writeBundle env lbs = do

  case env of
    WriteBundleEnvPlain{..} -> do
      writeAsMerkle wbeStorage lbs <&> HashRef

    WriteBundleEnvEnc{..} -> do
      let bsStream = readChunkedBS lbs defBlockSize

      nonce <- makeNonceForBundle env lbs

      let (SipHash a) = BA.sipHash wbeSk1 nonce
      let (SipHash b) = BA.sipHash wbeSk2 nonce

      let source = ToEncryptSymmBS wbeGks
                                   (Right wbeGk0)
                                   nonce
                                   bsStream
                                   NoMetaData
                                   (Just (EncryptGroupNaClSymmBlockSIP (a,b)))

      th <- runExceptT (writeAsMerkle wbeStorage source)
              >>= orThrow StorageError

      pure $ HashRef th

