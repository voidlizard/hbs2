{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language AllowAmbiguousTypes #-}
{-# Language UndecidableInstances #-}
module HBS2Git.App
  ( module HBS2Git.App
  , module HBS2Git.Types
  , HasStorage(..)
  )
  where

import HBS2.Prelude
import HBS2.Actors.Peer.Types
import HBS2.Data.Types.Refs
import HBS2.Base58
import HBS2.OrDie
import HBS2.Hash
import HBS2.Clock
import HBS2.Storage
import HBS2.Storage.Operations.ByteString
import HBS2.System.Logger.Simple
import HBS2.Merkle
import HBS2.Git.Types
import HBS2.Net.Proto.Definition()
import HBS2.Peer.RPC.Client.StorageClient
import HBS2.Net.Auth.Credentials hiding (getCredentials)
import HBS2.Net.Proto.RefLog
import HBS2.Defaults (defBlockSize)

import HBS2.Peer.RPC.Client.Unix
import HBS2.Peer.RPC.API.Peer
import HBS2.Peer.RPC.API.RefLog

import HBS2Git.Types
import HBS2Git.Config as Config
import HBS2Git.Evolve
import HBS2Git.PrettyStuff

import Data.Maybe
import Control.Monad.Trans.Maybe
import Data.Foldable
import Data.Either
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Control.Monad.Except (runExceptT,throwError)
import Crypto.Saltine.Core.Sign qualified as Sign
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Set (Set)
import Data.Set qualified as Set
import Lens.Micro.Platform
import System.Directory
-- import System.FilePath
import System.FilePath
import System.Process.Typed
import Text.InterpolatedString.Perl6 (qc)
import Network.HTTP.Simple
import Network.HTTP.Types.Status
import Control.Concurrent.STM (flushTQueue)
import Codec.Serialise
import Data.HashMap.Strict qualified as HashMap
import Data.List qualified as List
import Data.Text qualified as Text
-- import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Data.Cache qualified as Cache
-- import Control.Concurrent.Async
import System.Environment

import Prettyprinter.Render.Terminal

import Streaming.Prelude qualified as S

import UnliftIO

-- instance HasTimeLimits  UNIX (ServiceProto PeerAPI UNIX) m where

instance MonadIO m => HasCfgKey ConfBranch (Set String) m where
  key = "branch"

instance MonadIO m => HasCfgKey ConfBranch (Set GitRef) m where
  key = "branch"

instance MonadIO m => HasCfgKey HeadBranch (Maybe GitRef) m where
  key = "head-branch"

instance MonadIO m => HasCfgKey KeyRingFile (Maybe FilePath) m where
  key = "keyring"

instance MonadIO m => HasCfgKey KeyRingFiles (Set FilePath) m where
  key = "keyring"

instance MonadIO m => HasCfgKey StoragePref (Maybe FilePath) m where
  key = "storage"

tracePrefix :: SetLoggerEntry
tracePrefix  = toStderr . logPrefix "[trace] "

debugPrefix :: SetLoggerEntry
debugPrefix  = toStderr . logPrefix "[debug] "

errorPrefix :: SetLoggerEntry
errorPrefix  = toStderr . logPrefix "[error] "

warnPrefix :: SetLoggerEntry
warnPrefix   = toStderr . logPrefix "[warn] "

noticePrefix :: SetLoggerEntry
noticePrefix = toStderr

infoPrefix :: SetLoggerEntry
infoPrefix = toStderr

data WithLog = NoLog | WithLog

instance MonadIO m => HasRefCredentials (App m) where
  setCredentials ref cred = do
    asks (view appRefCred) >>= \t -> liftIO $ atomically $
      modifyTVar' t (HashMap.insert ref cred)

  getCredentials ref = do
    hm <- asks (view appRefCred) >>= liftIO . readTVarIO
    pure (HashMap.lookup ref hm) `orDie` "keyring not set"

instance (Monad m, HasStorage m) => (HasStorage (ResourceT m)) where
  getStorage = lift getStorage

instance MonadIO m => HasStorage (App m) where
  getStorage =  asks (rpcStorage . view appRpc) <&> AnyStorage . StorageClient

instance MonadIO m => HasRPC (App m) where
  getRPC =  asks (view appRpc)

withApp :: MonadIO m => AppEnv -> App m a -> m a
withApp env m = runReaderT (fromApp m) env

{-# NOINLINE hBS2PeerCatAPI #-}
hBS2PeerCatAPI :: IORef (Maybe API)
hBS2PeerCatAPI = unsafePerformIO (newIORef Nothing)

detectHBS2PeerCatAPI :: MonadIO m => m API
detectHBS2PeerCatAPI = do
  -- FIXME: hardcoded-hbs2-peer

  v <- liftIO $ readIORef hBS2PeerCatAPI

  case v of
    Just x -> pure x
    Nothing -> do
      (_, o, _) <- readProcess (shell [qc|hbs2-peer poke|])

      let dieMsg = "hbs2-peer is down or it's http is inactive"

      let answ = parseTop (LBS.unpack o) & fromRight mempty

      let po = headMay [ n | ListVal (Key "http-port:" [LitIntVal n]) <- answ  ]
      -- shutUp

      pnum <- pure po `orDie`  dieMsg

      debug $ pretty "using http port" <+> pretty po

      let api = [qc|http://localhost:{pnum}/cat|]

      liftIO $ writeIORef hBS2PeerCatAPI (Just api)

      pure api


detectHBS2PeerSizeAPI :: MonadIO m => m API
detectHBS2PeerSizeAPI = do
  api <- detectHBS2PeerCatAPI
  let new = Text.replace "/cat" "/size" $ Text.pack api
  pure $ Text.unpack new

detectHBS2PeerPutAPI :: MonadIO m => m API
detectHBS2PeerPutAPI = do
  api <- detectHBS2PeerCatAPI
  let new = Text.replace "/cat" "/" $ Text.pack api
  pure $ Text.unpack new

detectHBS2PeerRefLogGetAPI :: MonadIO m => m API
detectHBS2PeerRefLogGetAPI = do
  api <- detectHBS2PeerCatAPI
  let new = Text.replace "/cat" "/reflog" $ Text.pack api
  pure $ Text.unpack new


getAppStateDir :: forall m . MonadIO m => m FilePath
getAppStateDir = liftIO $ getXdgDirectory XdgData Config.appName



runWithRPC :: forall m . MonadUnliftIO m => (RPCEndpoints -> m ()) -> m ()
runWithRPC action = do

  (_, syn) <- configInit

  let soname' = lastMay [ Text.unpack n
                        | ListVal @C (Key "rpc" [SymbolVal "unix", LitStrVal n]) <- syn
                        ]

  soname <- race ( pause @'Seconds 1) (maybe detectRPC pure soname') `orDie` "hbs2-peer rpc timeout!"

  client <- race ( pause @'Seconds 1) (newMessagingUnix False 1.0 soname) `orDie` "hbs2-peer rpc timeout!"

  rpc <- RPCEndpoints <$> makeServiceCaller (fromString soname)
                      <*> makeServiceCaller (fromString soname)
                      <*> makeServiceCaller (fromString soname)

  messaging <- async $ runMessagingUnix client
  link messaging

  let endpoints = [ Endpoint @UNIX  (rpcPeer rpc)
                  , Endpoint @UNIX  (rpcStorage rpc)
                  , Endpoint @UNIX  (rpcRefLog rpc)
                  ]

  c1 <- async $ liftIO $ runReaderT (runServiceClientMulti endpoints) client
  link c1

  test <- race ( pause @'Seconds 1) (callService @RpcPoke (rpcPeer rpc) ()) `orDie` "hbs2-peer rpc timeout!"

  void $ pure test `orDie` "hbs2-peer rpc error!"

  debug $ "hbs2-peer RPC ok" <+> pretty soname

  action rpc

  cancel messaging

  void $ waitAnyCatchCancel [messaging, c1]

  where

    detectRPC = do
      (_, o, _) <- readProcess (shell [qc|hbs2-peer poke|])

      let answ = parseTop (LBS.unpack o) & fromRight mempty

      so <- pure (headMay [ Text.unpack r | ListVal (Key "rpc:" [LitStrVal r]) <- answ  ])
              `orDie` "hbs2-peer rpc not detected"

      -- FIXME: logger-to-support-colors
      liftIO $ hPutDoc stderr $ yellow "rpc: found RPC" <+> pretty so
                                <> line <>
                                yellow "rpc: add option" <+> parens ("rpc unix" <+> dquotes (pretty so))
                                <+> "to the config .hbs2/config"
                                <> line <> line
      pure so

runApp :: MonadUnliftIO m => WithLog -> App m () -> m ()
runApp l m = do

  case l of
    NoLog   -> pure ()
    WithLog -> do
      setLogging @ERROR  errorPrefix
      setLogging @NOTICE noticePrefix
      setLogging @INFO   infoPrefix

  doTrace <- liftIO $ lookupEnv "HBS2TRACE" <&> isJust

  if doTrace then do
    setLogging @DEBUG  debugPrefix
    setLogging @TRACE  tracePrefix
  else do
    setLoggingOff @DEBUG
    setLoggingOff @TRACE

  evolve

  (pwd, syn) <- Config.configInit

  xdgstate <- getAppStateDir

  runWithRPC $ \rpc -> do
    mtCred <- liftIO $ newTVarIO mempty
    let env = AppEnv pwd (pwd </> ".git") syn xdgstate mtCred rpc
    runReaderT (fromApp m) (set appRpc rpc env)

  debug $ vcat (fmap pretty syn)

  setLoggingOff @DEBUG
  setLoggingOff @ERROR
  setLoggingOff @NOTICE
  setLoggingOff @TRACE
  setLoggingOff @INFO

readBlock :: forall m . (MonadIO m, HasStorage m) => HashRef -> m (Maybe ByteString)
readBlock h = do
  sto <- getStorage
  liftIO $ getBlock sto (fromHashRef h)

readRef :: (HasStorage m, MonadIO m) => RepoRef -> m (Maybe HashRef)
readRef ref = do
  sto <- getStorage
  liftIO (getRef sto (refAlias ref)) <&> fmap HashRef

readHashesFromBlock :: (MonadIO m, HasStorage m) => HashRef -> m [HashRef]
readHashesFromBlock (HashRef h) = do
  treeQ <- liftIO newTQueueIO
  walkMerkle h (readBlock . HashRef) $ \hr -> do
    case hr of
      Left{} -> pure ()
      Right (hrr :: [HashRef]) -> liftIO $ atomically $ writeTQueue treeQ hrr
  re <- liftIO $ atomically $ flushTQueue treeQ
  pure $ mconcat re

type ObjType = MTreeAnn [HashRef]

readObject :: forall m . (MonadIO m, HasStorage m) => HashRef -> m (Maybe ByteString)
readObject h = runMaybeT do

  q <- liftIO newTQueueIO

  -- trace $ "readObject" <+> pretty h

  blk <- MaybeT $ readBlock h

  ann <- MaybeT $ pure $ deserialiseOrFail @(MTreeAnn [HashRef]) blk & either (const Nothing) Just

  walkMerkleTree (_mtaTree ann) (lift . readBlock . HashRef) $ \(hr :: Either (Hash HbSync) [HashRef]) -> do
    case hr of
      Left{} -> mzero
      Right (hrr :: [HashRef]) -> do
        for_ hrr $ \(HashRef hx) -> do
            block <- MaybeT $ readBlock (HashRef hx)
            liftIO $ atomically $ writeTQueue q block

  mconcat <$> liftIO (atomically $ flushTQueue q)

calcRank :: forall m . (MonadIO m, HasStorage m) => HashRef -> m Int
calcRank h = fromMaybe 0 <$> runMaybeT do

  blk <- MaybeT $ readBlock h

  ann <- MaybeT $ pure $ deserialiseOrFail @(MTree [HashRef]) blk & either (const Nothing) Just

  n <- S.toList_ $ do
    walkMerkleTree ann (lift . readBlock . HashRef) $ \(hr :: Either (Hash HbSync) [HashRef]) -> do
      case hr of
        Left{} -> pure ()
        Right (hrr :: [HashRef]) -> do
          S.yield (List.length hrr)

  pure $ sum n

postRefUpdate :: ( MonadIO m
                 , HasRefCredentials m
                 , HasRPC m
                 , IsRefPubKey Schema
                 )
              => RepoRef
              -> Integer
              -> HashRef
              -> m ()

postRefUpdate ref seqno hash = do
  info $ "refPostUpdate"  <+> pretty seqno <+> pretty hash

  cred <- getCredentials ref
  let pubk = view peerSignPk cred
  let privk = view peerSignSk cred
  let tran = SequentialRef seqno (AnnotatedHashRef Nothing hash)
  let bs = serialise tran & LBS.toStrict

  msg <- makeRefLogUpdate @HBS2L4Proto pubk privk bs

  rpc <- getRPC <&> rpcRefLog

  callService @RpcRefLogPost rpc msg
    >>= either (err . viaShow) (const $ pure ())


storeObject :: (MonadIO m, HasStorage m, HasConf m)
            => ByteString -> ByteString -> m (Maybe HashRef)
storeObject = storeObjectRPC

storeObjectRPC :: (MonadIO m, HasStorage m)
               => ByteString
               -> ByteString
               -> m (Maybe HashRef)
storeObjectRPC meta bs = do
  sto <- getStorage
  runMaybeT do
    h <- liftIO $ writeAsMerkle sto bs
    let txt = LBS.unpack meta & Text.pack
    blk <- MaybeT $ liftIO $ getBlock sto h

    -- FIXME: fix-excess-data-roundtrip
    mtree <- MaybeT $ deserialiseOrFail @(MTree [HashRef]) blk
                    & either (const $ pure  Nothing) (pure . Just)

    let ann = serialise (MTreeAnn (ShortMetadata txt) NullEncryption mtree)
    MaybeT $ liftIO $ putBlock sto ann <&> fmap HashRef


makeDbPath :: MonadIO m => RepoRef -> m FilePath
makeDbPath h = do
  state <- getAppStateDir
  liftIO $ createDirectoryIfMissing True state
  pure $ state </> show (pretty (AsBase58 h))

loadCredentials :: ( MonadIO m
                   , HasConf m
                   , HasRefCredentials m
                   ) => [FilePath] -> m ()
loadCredentials fp = do

  trace $ "loadCredentials" <+> pretty fp

  krOpt' <- cfgValue @KeyRingFiles @(Set FilePath) <&> Set.toList

  let krOpt = List.nub $ fp <> krOpt'

  when (null krOpt) do
    die "keyring not set"

  for_ krOpt $ \fn -> do
    (puk, cred) <- loadKeyring fn
    trace $ "got creds for" <+> pretty (AsBase58 puk)
    setCredentials (RefLogKey puk) cred
    pure ()

loadCredentials' ::
    ( MonadIO m
    , HasRefCredentials m
    )
    => FilePath -> m Sign.PublicKey
loadCredentials' fn = do
    (puk, cred) <- loadKeyring fn
    trace $ "got creds for" <+> pretty (AsBase58 puk)
    setCredentials (RefLogKey puk) cred
    pure puk

loadKeyring :: (MonadIO m) => FilePath -> m (Sign.PublicKey, PeerCredentials Schema)
loadKeyring fn = do
    krData <- liftIO $ B8.readFile fn
    cred <- pure (parseCredentials @Schema (AsCredFile krData)) `orDie` "bad keyring file"
    let puk = view peerSignPk cred
    pure (puk, cred)


