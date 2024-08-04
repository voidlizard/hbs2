module HBS2.Sync.Prelude
  ( module HBS2.Sync.Prelude
  , module Exported
  ) where


import HBS2.Prelude.Plated as Exported
import HBS2.Clock
import HBS2.Base58
import HBS2.Data.Detect
import HBS2.Merkle
import HBS2.Merkle.MetaData
import HBS2.OrDie as Exported
import HBS2.Data.Types.Refs as Exported
import HBS2.Data.Types.SignedBox
import HBS2.Net.Auth.Credentials
import HBS2.Net.Auth.GroupKeySymm as Symm
import HBS2.Net.Auth.Schema
import HBS2.Clock as Exported
import HBS2.Net.Proto.Service
import HBS2.Storage
import HBS2.Storage.Operations.Class
import HBS2.Storage.Operations.ByteString
import HBS2.Peer.Proto.RefChan
import HBS2.Peer.CLI.Detect
import HBS2.Peer.RPC.Client
import HBS2.Peer.RPC.Client.Unix
import HBS2.Peer.RPC.Client.RefChan as Client
import HBS2.Peer.RPC.Client.StorageClient
import HBS2.Peer.RPC.API.Peer
import HBS2.Peer.RPC.API.RefChan
import HBS2.Peer.RPC.API.RefLog
import HBS2.Peer.RPC.API.Storage
import HBS2.System.Logger.Simple.ANSI as Exported
import HBS2.Misc.PrettyStuff as Exported

import HBS2.CLI.Run hiding (PeerException(..))
import HBS2.CLI.Run.MetaData
-- import HBS2.CLI.Run.GroupKey

import HBS2.KeyMan.Keys.Direct

import Data.Config.Suckless as Exported
import Data.Config.Suckless.Script as Exported
import Data.Config.Suckless.Script.File

import Codec.Serialise as Exported
import Control.Concurrent.STM (flushTQueue)
import Control.Monad.Reader as Exported
import Control.Monad.Trans.Cont as Exported
import Control.Monad.Trans.Maybe
import Control.Monad.Except
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Coerce
import Data.Either
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.List qualified as L
import Data.List (stripPrefix)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Set qualified as Set
import Data.Set (Set)
import Data.Time.Clock.POSIX
import Data.Time.Clock (UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (utcToLocalTime, getCurrentTimeZone, utc)
import Data.Word
import Lens.Micro.Platform
import Streaming.Prelude qualified as S
import System.Directory (getModificationTime,setModificationTime)
import System.FilePath.Posix
import UnliftIO

import UnliftIO.IO.File qualified as UIO

{- HLINT ignore "Functor law" -}
{- HLINT ignore "Eta reduce" -}

data SyncEnv =
  SyncEnv
  { refchanAPI :: ServiceCaller RefChanAPI UNIX
  , storageAPI :: ServiceCaller StorageAPI UNIX
  , peerAPI    :: ServiceCaller PeerAPI UNIX
  }

newtype SyncApp m a =
  SyncApp { fromSyncApp :: ReaderT (Maybe SyncEnv) m a }
  deriving newtype ( Applicative
                   , Functor
                   , Monad
                   , MonadUnliftIO
                   , MonadIO
                   , MonadReader (Maybe SyncEnv))


type SyncAppPerks m = MonadUnliftIO m

instance MonadIO m => HasClientAPI StorageAPI UNIX (SyncApp m) where
  getClientAPI = ask >>= orThrow PeerNotConnectedException
                     <&> storageAPI

instance MonadIO m => HasClientAPI RefChanAPI UNIX (SyncApp m) where
  getClientAPI = ask >>= orThrow PeerNotConnectedException
                     <&> refchanAPI

instance MonadIO m => HasClientAPI PeerAPI UNIX (SyncApp m) where
  getClientAPI = ask >>= orThrow PeerNotConnectedException
                     <&> peerAPI

instance MonadIO m => HasStorage (SyncApp m) where
  getStorage = do
    api <- getClientAPI @StorageAPI @UNIX
    pure $ AnyStorage (StorageClient api)

withSyncApp :: SyncAppPerks m => Maybe SyncEnv -> SyncApp m a -> m a
withSyncApp env action = runReaderT (fromSyncApp action) env

runSyncApp :: SyncAppPerks m => SyncApp m a -> m a
runSyncApp m = do
  setupLogger
  withSyncApp Nothing m `finally` flushLoggers

recover :: SyncApp IO a -> SyncApp IO a
recover what = do
  catch what $ \case
    PeerNotConnectedException -> do

      soname <- detectRPC
                  `orDie` "can't locate hbs2-peer rpc"

      flip runContT pure do

        client <- lift $ race (pause @'Seconds 1) (newMessagingUnix False 1.0 soname)
                    >>= orThrowUser ("can't connect to" <+> pretty soname)

        void $ ContT $ withAsync $ runMessagingUnix client

        peerAPI    <- makeServiceCaller @PeerAPI (fromString soname)
        refChanAPI <- makeServiceCaller @RefChanAPI (fromString soname)
        storageAPI <- makeServiceCaller @StorageAPI (fromString soname)

        -- let sto = AnyStorage (StorageClient storageAPI)

        let endpoints = [ Endpoint @UNIX  peerAPI
                        , Endpoint @UNIX  refChanAPI
                        , Endpoint @UNIX  storageAPI
                        ]

        void $ ContT $ withAsync $ liftIO $ runReaderT (runServiceClientMulti endpoints) client

        let env = Just (SyncEnv refChanAPI storageAPI peerAPI)

        liftIO $ withSyncApp env what

data PeerException =
  PeerNotConnectedException
  deriving stock (Show, Typeable)

instance Exception PeerException

data RunDirectoryException =
    RefChanNotSetException
  | RefChanHeadNotFoundException
  | EncryptionKeysNotDefined
  | SignKeyNotSet
  deriving stock (Show,Typeable)

instance Exception RunDirectoryException

removePrefix :: FilePath -> FilePath -> FilePath
removePrefix prefix path =
  let prefixDirs = splitDirectories $ normalise prefix
      pathDirs = splitDirectories $ normalise path
  in joinPath $ fromMaybe pathDirs (stripPrefix prefixDirs pathDirs)

getFileTimestamp :: MonadUnliftIO m => FilePath -> m Word64
getFileTimestamp filePath = do
  t0 <- liftIO $ getModificationTime filePath
  pure (round $ utcTimeToPOSIXSeconds t0)


data EntryType = File | Dir | Tomb
                 deriving stock (Eq,Ord,Show,Data,Generic)

data EntryDesc =
  EntryDesc
  { entryType        :: EntryType
  , entryTimestamp   :: Word64
  , entryRemoteHash  :: Maybe HashRef
  }
  deriving stock (Eq,Ord,Show,Data,Generic)

data Entry =
  DirEntry EntryDesc FilePath
  deriving stock (Eq,Ord,Show,Data,Generic)

getEntryTimestamp :: Entry -> Word64
getEntryTimestamp (DirEntry d _) = entryTimestamp d

getEntryHash :: Entry -> Maybe HashRef
getEntryHash (DirEntry d _) = entryRemoteHash d

isFile :: Entry -> Bool
isFile = \case
  DirEntry (EntryDesc { entryType = File}) _ -> True
  _ -> False

entriesFromLocalFile :: MonadUnliftIO m => FilePath -> FilePath -> m (Map FilePath Entry)
entriesFromLocalFile prefix fn' = do
  let fn0 = removePrefix prefix fn
  ts <- getFileTimestamp fn
  pure $ entriesFromFile Nothing ts fn0
  where
    fn = normalise fn'

entriesFromFile :: Maybe HashRef -> Word64 -> FilePath -> Map FilePath Entry
entriesFromFile h ts fn0 = do
  let dirs = splitDirectories (dropFileName fn0)
               & dropWhile (== ".")
  let es = flip L.unfoldr ("",dirs) $ \case
              (_,[])   -> Nothing
              (p,d:ds) -> Just (dirEntry (p </> d), (p </> d, ds) )

  Map.fromList [ (p, e)
               | e@(DirEntry _ p) <- fileEntry fn0 : es
               ]
  where
    dirEntry p   = DirEntry (EntryDesc Dir ts Nothing) p
    fileEntry p  = DirEntry (EntryDesc File ts h) p

runDirectory :: ( IsContext c
                , SyncAppPerks m
                , HasClientAPI RefChanAPI UNIX m
                , HasClientAPI StorageAPI UNIX m
                , HasStorage m
                , Exception (BadFormException c)
                ) => FilePath -> RunM c m ()
runDirectory path = do

  t <- ask
  d0 <- readTVarIO t

  runDir
    `catch` \case
      RefChanNotSetException -> do
        err $ "no refchan set for" <+> pretty path
      RefChanHeadNotFoundException -> do
        err $ "no refchan head found for" <+> pretty path
      EncryptionKeysNotDefined -> do
        err $ "no readers defined in the refchan for " <+> pretty path
      SignKeyNotSet -> do
        err $ "sign key not set or not found " <+> pretty path

    `catch` \case
      (e :: OperationError) -> do
        err $ viaShow e

    `finally` do
      warn "exiting"
      atomically (writeTVar t d0)

  where

    merge :: Entry -> Entry -> Entry
    merge a b = if getEntryTimestamp a > getEntryTimestamp b then a else b

    freshIn :: FilePath -> Entry -> Map FilePath Entry -> Bool
    freshIn p e state = do
      let remote = Map.lookup p state
      maybe1 remote True $ \r -> do
        getEntryTimestamp e > getEntryTimestamp r

    runDir = do

      notice $ yellow "run directory" <+> pretty path

      trc    <- newTVarIO Nothing
      tsign  <- newTVarIO Nothing
      texcl  <- newTQueueIO
      tincl  <- newTQueueIO

      atomically $ writeTQueue tincl "**"

      ins <- liftIO (try @_ @IOError (readFile (path </> ".hbs2-sync/config")))
               <&> fromRight mempty
               <&> parseTop
               <&> either mempty (fmap fixContext)

      bindBuiltins $ bindMatch "refchan" $ nil_ $ \case
        [SignPubKeyLike puk] -> do
          debug $ red "USE FUCKING REFCHAN!" <+> pretty (AsBase58 puk)
          atomically $ writeTVar trc (Just puk)

        _ -> pure ()

      bindBuiltins $ bindMatch "exclude" $ nil_ $ \case
        [StringLike excl] -> do
          debug $ red "EXCLUDE!" <+> pretty excl
          atomically $ writeTQueue texcl excl

        _ -> pure ()

      bindBuiltins $ bindMatch "include" $ nil_ $ \case
        [StringLike s] -> do
          debug $ red "INCLUDE!" <+> pretty s
          atomically $ writeTQueue tincl s

        _ -> pure ()


      bindBuiltins $ bindMatch "sign" $ nil_ $ \case
        [SignPubKeyLike s] -> do
          debug $ red "SIGN" <+> pretty (AsBase58 s)

          runMaybeT do
            creds <- MaybeT $ runKeymanClient $ loadCredentials s
            atomically $ writeTVar tsign (Just creds)

          pure ()

        _ -> pure ()

      debug $ pretty ins
      evalTop ins

      incl <- atomically (flushTQueue tincl) <&> HS.fromList <&> HS.toList
      excl <- atomically (flushTQueue texcl) <&> HS.fromList <&> HS.toList

      refchan <- readTVarIO trc
                   >>= orThrow RefChanNotSetException

      fetchRefChan @UNIX refchan

      rch <- Client.getRefChanHead @UNIX refchan
                   >>= orThrow RefChanHeadNotFoundException

      creds <- readTVarIO tsign
                  >>= orThrow SignKeyNotSet

      sto <- getClientAPI @StorageAPI @UNIX
              <&> AnyStorage . StorageClient

      debug $ "step 1"   <+> "load state from refchan"
      debug $ "step 1.1" <+> "initial state is empty"
      debug $ "step 2"   <+> "create local state"
      debug $ "step 2.1" <+> "scan all files"
      debug $ "step 2.2" <+> "extract all / directories"

      debug $ "step 3"   <+> "merge states"
      debug $ "step 3.1" <+> "generate merge actions"
      debug $ "step 3.2" <+> "apply actions"

      let p0 = normalise path

      es' <- S.toList_ $ do
        glob incl excl path  $ \fn -> do
          let fn0 = removePrefix path fn
          es <- liftIO (entriesFromLocalFile path fn)
          -- debug $ yellow "file" <+> viaShow ts <+> pretty fn0
          S.each es
          pure True

      debug "FUCKING GOT REFCHAN HEAD"

      let local = Map.fromList [ (p,e) | e@(DirEntry _ p) <- es' ]

      remote <- getStateFromRefChan refchan

      let merged = Map.unionWith merge local remote

      flip runContT pure do

        for_ (Map.toList merged) $ \(p,e) -> do
          debug $ yellow "entry" <+> pretty p <+> viaShow e

          callCC $ \next -> do

            when (freshIn p e remote) do

              -- FIXME: dangerous!
              lbs <- liftIO (LBS.readFile (path </> p))

              let (dir,file) = splitFileName p

              let meta = HM.fromList [ ("file-name", fromString file)
                                     ]
                          <> case dir of
                               "./" -> mempty
                               d    -> HM.singleton "location" (fromString d)

              let members = view refChanHeadReaders rch & HS.toList

              -- FIXME: support-unencrypted?
              when (L.null members) do
                throwIO EncryptionKeysNotDefined

              gk <- Symm.generateGroupKey @'HBS2Basic Nothing members

              -- FIXME: survive-this-error?
              href <- lift $ createTreeWithMetadata sto (Just gk) meta lbs
                        >>= orThrowPassIO

              let tx = AnnotatedHashRef Nothing href
              let spk = view peerSignPk creds
              let ssk = view peerSignSk creds

              let box = makeSignedBox @HBS2Basic spk ssk (LBS.toStrict $ serialise tx)

              notice $ red "POST NEW REMOTE ENTRY" <+> pretty p <+> pretty href

              lift $ postRefChanTx @UNIX refchan box

            when (freshIn p e local) do
              h <- ContT $ maybe1 (getEntryHash e) none
              -- let h = getEntryHash e

              notice $ red "WRITE NEW LOCAL ENTRY" <+> pretty path <+> pretty p <+> pretty (getEntryHash e)

              lbs' <- lift (runExceptT (getTreeContents sto h))
                      <&> either (const Nothing) Just

              lbs <- ContT $ maybe1 lbs' none

              liftIO $ UIO.withBinaryFileAtomic (path </> p) WriteMode $ \fh -> do
                LBS.hPutStr fh lbs

              let ts  = getEntryTimestamp e
              let timestamp = posixSecondsToUTCTime (fromIntegral ts)

              liftIO $ setModificationTime (path </> p) timestamp


    getStateFromRefChan rchan = do

      debug $ red "getStateFromRefChan" <+> pretty (AsBase58 rchan)

      sto <- getStorage

      outq <- newTQueueIO
      tss <- newTVarIO mempty

      walkRefChanTx @UNIX rchan $ \case
        A (AcceptTran ts _ what) -> do
          debug $ red "ACCEPT" <+> pretty ts <+> pretty what
          for_ ts $ \w -> do
            atomically $ modifyTVar tss (HM.insertWith max what (coerce @_ @Word64 w))

        P orig (ProposeTran _ box) -> void $ runMaybeT do
          (_, bs) <- unboxSignedBox0 box & toMPlus
          AnnotatedHashRef w href <- deserialiseOrFail @AnnotatedHashRef (LBS.fromStrict bs)
                                      & toMPlus . either (const Nothing) Just

          let findKey gk = liftIO (runKeymanClient (extractGroupKeySecret gk))

          runExceptT (extractMetaData @'HBS2Basic findKey sto href)
           >>= either (const none) ( \meta -> atomically $ writeTQueue outq (orig, (href, meta)) )

      trees <- atomically (flushTQueue outq)

      tsmap <- readTVarIO tss

      ess0 <- S.toList_ do
        for_ trees $ \(txh, (tree, meta)) -> do
          let what = parseTop meta & fromRight mempty
          let loc  = headDef "" [ l | ListVal [StringLike "location:", StringLike l] <- what ]

          void $ runMaybeT do
            fn  <- toMPlus $ headMay [ l | ListVal [StringLike "file-name:", StringLike l] <- what ]
            ts  <- toMPlus $ HM.lookup txh tsmap
            let r = entriesFromFile (Just tree) ts (loc </> fn)
            lift $ S.yield r

      pure $ Map.unionsWith merge ess0


getTreeContents :: ( MonadUnliftIO m
                   , MonadError OperationError m
                   )
                => AnyStorage
                -> HashRef
                -> m LBS.ByteString

getTreeContents sto href = do

  blk <- getBlock sto (coerce href)
           >>= orThrowError MissedBlockError

  let q = tryDetect (coerce href) blk

  case q of

    MerkleAnn (MTreeAnn {_mtaCrypt = NullEncryption }) -> do
      readFromMerkle sto (SimpleKey (coerce href))

    MerkleAnn ann@(MTreeAnn {_mtaCrypt = EncryptGroupNaClSymm gkh _}) -> do

      rcpts  <- Symm.loadGroupKeyMaybe sto (HashRef gkh)
                  >>= orThrowError (GroupKeyNotFound 11)
                  <&> HM.keys . Symm.recipients

      kre <- runKeymanClient do
                loadKeyRingEntries rcpts <&> fmap snd

      readFromMerkle sto (ToDecryptBS kre (coerce href))

    _ -> throwError UnsupportedFormat


syncEntries :: forall c m . (MonadUnliftIO m, IsContext c) => MakeDictM c m ()
syncEntries = do

  entry $ bindMatch "--debug" $ nil_ $ \case
    [SymbolVal "off"] -> do
      setLoggingOff @DEBUG

    _ -> do
      setLogging @DEBUG  debugPrefix

-- debugPrefix :: LoggerEntry -> LoggerEntry
debugPrefix = toStderr . logPrefix "[debug] "

setupLogger :: MonadIO m => m ()
setupLogger = do
  -- setLogging @DEBUG  $ toStderr . logPrefix "[debug] "
  setLogging @ERROR  $ toStderr . logPrefix "[error] "
  setLogging @WARN   $ toStderr . logPrefix "[warn] "
  setLogging @NOTICE $ toStdout . logPrefix ""
  pure ()

flushLoggers :: MonadIO m => m ()
flushLoggers = do
  silence

silence :: MonadIO m => m ()
silence = do
  setLoggingOff @DEBUG
  setLoggingOff @ERROR
  setLoggingOff @WARN
  setLoggingOff @NOTICE

