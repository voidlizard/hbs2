{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language AllowAmbiguousTypes #-}
{-# Language UndecidableInstances #-}
module HBS2Git.App
  ( module HBS2Git.App
  , module HBS2Git.Types
  , HasStorage(..)
  , HasConf(..)
  )
  where

import HBS2.Prelude.Plated
import HBS2.Data.Types.Refs
import HBS2.Base58
import HBS2.OrDie
import HBS2.Hash
import HBS2.Clock
import HBS2.Storage
import HBS2.Storage.Operations.ByteString as OP
import HBS2.Net.Auth.GroupKeySymm qualified as Symm
import HBS2.System.Logger.Simple
import HBS2.Merkle
import HBS2.Git.Types
import HBS2.Peer.RPC.Client.StorageClient
import HBS2.Net.Auth.Credentials hiding (getCredentials)
import HBS2.Peer.Proto
import HBS2.Defaults (defBlockSize)

import HBS2.Peer.RPC.Client.Unix
import HBS2.Peer.RPC.API.Peer
import HBS2.Peer.RPC.API.RefLog

import HBS2Git.Types
import HBS2Git.Config as Config
import HBS2Git.State
import HBS2Git.KeysMetaData
import HBS2Git.Encryption
import HBS2Git.Evolve
import HBS2Git.PrettyStuff
import HBS2Git.Alerts

import Data.Maybe
import Control.Monad.Trans.Maybe
import Data.Foldable
import Data.Either
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Control.Monad.Except (runExceptT)
import Control.Monad.Catch
import Crypto.Saltine.Core.Sign qualified as Sign
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Set (Set)
import Data.Set qualified as Set
import Lens.Micro.Platform
import System.Directory
import System.FilePattern.Directory
import System.FilePath
import System.Process.Typed
import Text.InterpolatedString.Perl6 (qc)
import Control.Concurrent.STM (flushTQueue)
import Codec.Serialise
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.List qualified as List
import Data.Text qualified as Text
import System.Environment

import Prettyprinter.Render.Terminal

import Streaming.Prelude qualified as S

import UnliftIO as UIO

data NoRPCException = NoRPCException
  deriving stock (Show, Typeable)

instance Exception NoRPCException

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


instance MonadIO m => HasGlobalOptions (App m) where
  addGlobalOption k v =
    asks (view appOpts ) >>= \t -> liftIO $ atomically $
      modifyTVar' t (HashMap.insert k v)

  getGlobalOption k = do
    hm <- asks (view appOpts) >>= liftIO . readTVarIO
    pure (HashMap.lookup k hm)

instance MonadIO m => HasRefCredentials (App m) where
  setCredentials ref cred = do
    asks (view appRefCred) >>= \t -> liftIO $ atomically $
      modifyTVar' t (HashMap.insert ref cred)

  getCredentials ref = do
    hm <- asks (view appRefCred) >>= liftIO . readTVarIO
    pure (HashMap.lookup ref hm) `orDie` "keyring not set (1)"

instance MonadIO m => HasEncryptionKeys (App m) where
  addEncryptionKey ke = do
    asks (view appKeys) >>= \t -> liftIO $ atomically do
      modifyTVar' t (HashMap.insert (view krPk ke) (view krSk ke))

  findEncryptionKey puk = (asks (view appKeys) >>= \t -> liftIO $ readTVarIO t) <&> HashMap.lookup puk

  enumEncryptionKeys = do
    them <- (asks (view appKeys) >>= \t -> liftIO $ readTVarIO t) <&> HashMap.toList
    pure $ [KeyringEntry k s Nothing | (k,s) <- them ]

instance (Monad m, HasStorage m) => (HasStorage (ResourceT m)) where
  getStorage = lift getStorage

instance MonadIO m => HasStorage (App m) where
  getStorage =  asks (rpcStorage . view appRpc) <&> AnyStorage . StorageClient

instance MonadIO m => HasRPC (App m) where
  getRPC =  asks (view appRpc)

withApp :: MonadIO m => AppEnv -> App m a -> m a
withApp env m = runReaderT (fromApp m) env


detectRPC :: (MonadIO m, MonadThrow m) => Bool -> m FilePath
detectRPC noisy = do
  (_, o, _) <- readProcess (shell [qc|hbs2-peer poke|])

  let answ = parseTop (LBS.unpack o) & fromRight mempty

  so <- case headMay [ Text.unpack r | ListVal (Key "rpc:" [LitStrVal r]) <- answ  ] of
          Nothing -> throwM NoRPCException
          Just w  -> pure w

  when noisy do

    -- FIXME: logger-to-support-colors
    liftIO $ hPutDoc stderr $ yellow "rpc: found RPC" <+> pretty so
                              <> line <>
                              yellow "rpc: add option" <+> parens ("rpc unix" <+> dquotes (pretty so))
                              <+> "to the config .hbs2/config"
                              <> line <> line


  pure so

runWithRPC :: forall m . (MonadUnliftIO m, MonadThrow m) => (RPCEndpoints -> m ()) -> m ()
runWithRPC action = do

  (_, syn) <- configInit

  let soname' = lastMay [ Text.unpack n
                        | ListVal (Key "rpc" [SymbolVal "unix", LitStrVal n]) <- syn
                        ]

  soname <- race ( pause @'Seconds 1) (maybe (detectRPC True) pure soname') `orDie` "hbs2-peer rpc timeout!"

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

runInit :: (MonadUnliftIO m, MonadThrow m) => m () -> m ()
runInit m = m

runApp :: (MonadUnliftIO m, MonadThrow m) => WithLog -> App m () -> m ()
runApp l m = do

  flip UIO.catches dealWithException do

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
      mtKeys <- liftIO $ newTVarIO mempty
      mtOpt <- liftIO $ newTVarIO mempty
      let env = AppEnv pwd (pwd </> ".git") syn xdgstate mtCred mtKeys mtOpt rpc
      runReaderT (fromApp (loadKeys >> m)) (set appRpc rpc env)

    debug $ vcat (fmap pretty syn)

    setLoggingOff @DEBUG
    setLoggingOff @ERROR
    setLoggingOff @NOTICE
    setLoggingOff @TRACE
    setLoggingOff @INFO

  where
    dealWithException = [ noWorkDir ]

    noWorkDir = Handler $
      \NoWorkDirException -> liftIO do
        hPutDoc stderr $ "hbs2-git:" <+> red "*** no git working directory found."
                                     <+> yellow "Perhaps you'd call" <+> "'git init'" <+> "first"
                                     <> line
        exitFailure

readBlock :: forall m . (MonadIO m, HasStorage m) => HashRef -> m (Maybe ByteString)
readBlock h = do
  sto <- getStorage
  liftIO $ getBlock sto (fromHashRef h)

readRef :: (HasStorage m, MonadIO m) => RepoRef -> m (Maybe HashRef)
readRef ref = do
  sto <- getStorage
  liftIO (getRef sto ref) <&> fmap HashRef

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
                 , MonadMask m
                 , HasStorage m
                 , HasConf m
                 , HasRefCredentials m
                 , HasEncryptionKeys m
                 , HasRPC m
                 , IsRefPubKey Schema
                 )
              => RepoRef
              -> Integer
              -> HashRef
              -> m ()

postRefUpdate ref seqno hash = do

  cred <- getCredentials ref
  let pubk = view peerSignPk cred
  let privk = view peerSignSk cred

  ann <- genKeysAnnotations ref

  --  вот прямо сюда ОЧЕНЬ удобно вставить метаданные для GK1
  let tran = SequentialRef seqno (AnnotatedHashRef ann hash)
  let bs = serialise tran & LBS.toStrict

  msg <- makeRefLogUpdate @HBS2L4Proto pubk privk bs

  rpc <- getRPC <&> rpcRefLog

  callService @RpcRefLogPost rpc msg
    >>= either (err . viaShow) (const $ pure ())


storeObject :: ( MonadIO m
               , MonadMask m
               , HasStorage m
               , HasConf m
               , HasRefCredentials m
               , HasEncryptionKeys m
               )
            => RepoRef
            -> ByteString
            -> ByteString
            -> m (Maybe HashRef)
storeObject repo meta bs = do
  encrypted <- isRefEncrypted (fromRefLogKey repo)
  info $ "encrypted" <+> pretty repo <> colon <+> if encrypted then "yes" else "no"
  storeObjectRPC encrypted repo meta bs



data WriteOp = WritePlain | WriteEncrypted B8.ByteString

storeObjectRPC :: ( MonadIO m
                  , MonadMask m
                  , HasStorage m
                  , HasConf m
                  , HasRefCredentials m
                  , HasEncryptionKeys m
                  )
               => Bool
               -> RepoRef
               -> ByteString
               -> ByteString
               -> m (Maybe HashRef)

storeObjectRPC False repo meta bs = do
  sto <- getStorage
  db <- makeDbPath repo >>= dbEnv

  runMaybeT do


    h <- liftIO $ writeAsMerkle sto bs
    let txt = LBS.unpack meta & Text.pack
    blk <- MaybeT $ liftIO $ getBlock sto h

    -- FIXME: fix-excess-data-roundtrip
    mtree <- MaybeT $ deserialiseOrFail @(MTree [HashRef]) blk
                    & either (const $ pure  Nothing) (pure . Just)

    -- TODO: upadte-metadata-right-here
    let ann = serialise (MTreeAnn (ShortMetadata txt) NullEncryption mtree)
    MaybeT $ liftIO $ putBlock sto ann <&> fmap HashRef


storeObjectRPC True repo meta bs = do

  sto <- getStorage
  db <- makeDbPath repo >>= dbEnv

  runMaybeT do

    let txt = LBS.unpack meta & Text.pack

    ki <- lift $ getKeyInfo (fromRefLogKey repo) >>= maybe noKeyInfo pure
    gkh0 <- withDB db $ stateGetLocalKey ki >>= maybe noKeyFound pure

    gk0  <- runExceptT (readFromMerkle sto (SimpleKey (fromHashRef gkh0)))
               >>= either (const $ noKeyFound) (pure . deserialiseOrFail @(GroupKey 'Symm HBS2Basic))
               >>= either (const $ noKeyFound) pure

    let pk = keyInfoOwner ki

    sk <- lift (findEncryptionKey pk) >>= maybe noKeyFound pure

    gks <- maybe noKeyFound pure (Symm.lookupGroupKey sk pk gk0)

    let nonce = hashObject @HbSync bs & serialise
                                      & LBS.drop 2
                                      & LBS.toStrict

    let bsStream = readChunkedBS bs defBlockSize

    let source = ToEncryptSymmBS gks
                                 (Left gkh0 :: LoadedRef (GroupKey 'Symm HBS2Basic))
                                 nonce
                                 bsStream
                                 (ShortMetadata txt)
                                 Nothing

    h <- runExceptT (writeAsMerkle sto source) >>= either (const cantWriteMerkle) pure

    pure (HashRef h)

  where

    cantWriteMerkle :: forall a m . MonadIO m => m a
    cantWriteMerkle = die "Can't write encrypted merkle tree"

    noKeyFound :: forall a m . MonadIO m => m a
    noKeyFound = do
      liftIO $ hPutDoc stderr (red $ "No group key found for repo" <+> pretty repo <> line)
      die "*** fatal"

    noKeyInfo = do
      liftIO $ hPutDoc stderr (red $ pretty (noKeyInfoMsg repo) <> line)
      die "*** fatal"


loadCredentials :: ( MonadIO m
                   , HasConf m
                   , HasRefCredentials m
                   ) => [FilePath] -> m ()
loadCredentials fp = do

  debug $ "loadCredentials" <+> pretty fp

  krOpt' <- cfgValue @KeyRingFiles @(Set FilePath) <&> Set.toList

  let krOpt = List.nub $ fp <> krOpt'

  void $ runMaybeT do

    when (null krOpt) do
      debug "keyring not set (2)"
      mzero

    for_ krOpt $ \fn -> do
      (puk, cred) <- loadKeyring fn
      trace $ "got creds for" <+> pretty (AsBase58 puk)
      lift $ setCredentials (RefLogKey puk) cred
      pure ()

loadCredentials' ::
    ( MonadIO m
    , HasRefCredentials m
    )
    => FilePath -> m Sign.PublicKey
loadCredentials' fn = do
    (puk, cred) <- runMaybeT (loadKeyring fn) `orDie` [qc|Can't load credentials {fn}|]
    trace $ "got creds for" <+> pretty (AsBase58 puk)
    setCredentials (RefLogKey puk) cred
    pure puk

loadKeyring :: (MonadIO m, MonadPlus m) => FilePath -> m (Sign.PublicKey, PeerCredentials Schema)
loadKeyring fn = do
    krData <- liftIO $ B8.readFile fn

    let cred' = parseCredentials @Schema (AsCredFile krData)

    maybe1 cred' mzero $ \cred -> do
      let puk = view peerSignPk cred
      pure (puk, cred)


makeFilter :: String -> (String, [String])
makeFilter = norm . over _1 sub1 . over _2 List.singleton . go ""
  where
    go pref ( cn : cs ) | cn `elem` "?*" = (p0, p1 <> p2)
      where
        (p0, p1) = splitFileName pref
        p2 = cn : cs

    go pref ( '/' : cn : cs ) | cn `elem` "?*" = (pref <> ['/'], cn : cs)

    go pref ( c : cs ) = go (pref <> [c]) cs

    go pref [] = (pref, "")

    sub1 "" = "."
    sub1 x = x

    norm (xs, [""]) = (p1, [p2])
      where
        (p1, p2) = splitFileName xs

    norm x = x

loadKeys :: ( MonadIO m
            , HasConf m
            , HasEncryptionKeys m
            , HasGlobalOptions m
            ) => m ()
loadKeys = do
  conf <- getConf

  trace $ "loadKeys"

  found1 <- findKeyFiles =<< liftIO (lookupEnv "HBS2KEYS")
  found2 <- findKeyFiles =<< getGlobalOption "key"

  found <- liftIO $ mapM canonicalizePath (found1 <> found2)

  let enc = [ args | (ListVal (SymbolVal "encrypted" : (LitStrVal r) : args)) <- conf ]

  let owners  = [ fromStringMay @(PubKey 'Encrypt Schema) (Text.unpack o)
                | ListVal (Key "owner" [LitStrVal o]) :: Syntax C <- universeBi enc
                ] & catMaybes & HashSet.fromList


  let members = [ fromStringMay @(PubKey 'Encrypt Schema) (Text.unpack o)
                | ListVal (Key "member" [LitStrVal o]) :: Syntax C <- universeBi enc
                ] & catMaybes & HashSet.fromList

  let decrypt =  [ Text.unpack o
                 | ListVal (Key "decrypt" [LitStrVal o]) <- conf
                 ]

  let keyrings = [ Text.unpack o | (ListVal (Key "keyring" [LitStrVal o]) :: Syntax C)
                 <- universeBi enc
                 ] <> decrypt <> found
                 & List.nub

  forM_ keyrings $ \k -> void $ runMaybeT do
    trace $ "loadKeys: keyring" <+> pretty k
    (_, pc) <- loadKeyring k

    forM_ (view peerKeyring pc) $ \ke -> do
      let pk = view krPk  ke

      trace $ "loadKeyring: key" <+> pretty (AsBase58 pk)
      lift $ addEncryptionKey ke


  where
    findKeyFiles w = do
        let flt = makeFilter <$> w
        maybe1 flt (pure mempty) $
          \(p, fmask) -> liftIO do
            getDirectoryFiles p fmask <&> fmap (p</>)

