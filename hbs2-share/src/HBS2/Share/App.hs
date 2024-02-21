{-# Language MultiWayIf #-}
module HBS2.Share.App
  ( module HBS2.Share.App.Types
  , AppOption(..)
  , Command
  , AppPerks
  , runApp
  , runSync
  ) where

import HBS2.Prelude.Plated
import HBS2.Base58
import HBS2.Merkle
import HBS2.Data.Detect
import HBS2.Defaults (defBlockSize)
import HBS2.Hash
import HBS2.Clock
import HBS2.OrDie
import HBS2.Net.Proto.RefChan.Types
import HBS2.Net.Auth.Credentials
import HBS2.Net.Auth.Credentials.Sigil
import HBS2.Data.Types.SignedBox
import HBS2.Net.Auth.GroupKeySymm
import HBS2.Net.Auth.GroupKeySymm qualified as Symm
import HBS2.Net.Proto.Definition()
import HBS2.Net.Proto.RefChan

import HBS2.Net.Messaging.Unix
import HBS2.Net.Proto.Service
import HBS2.Storage
import HBS2.Storage.Operations.ByteString
import HBS2.Storage.Operations.Missed (findMissedBlocks,findMissedBlocks2)

import HBS2.Peer.CLI.Detect (detectRPC)
import HBS2.Peer.RPC.Client.StorageClient

import HBS2.KeyMan.Keys.Direct

import HBS2.Share.App.Types
import HBS2.Share.Config hiding (key)
import HBS2.Share.State
import HBS2.Share.Files qualified as F
import HBS2.Share.Keys
import HBS2.Share.MetaData
import HBS2.Share.LocalHash

import HBS2.System.Logger.Simple.ANSI
import DBPipe.SQLite

import Control.Applicative
import Control.Concurrent.STM (flushTQueue)
import Control.Monad.Except (runExceptT)
import Control.Monad.Trans.Maybe
import Data.ByteArray.Hash qualified as BA
import Data.ByteArray.Hash (SipHash(..), SipKey(..))
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString qualified as BS
import Data.HashSet  qualified as HashSet
import Data.HashMap.Strict qualified as HashMap
import Data.List qualified as List
import Data.Maybe
import Data.Set qualified as Set
import Data.Set (Set)
import Data.Either
import System.Directory
import System.FilePath

import Codec.Serialise
import Codec.Compression.GZip as GZip
import System.AtomicWrite.Writer.LazyByteString qualified as AwL

import System.TimeIt

import Streaming.Prelude qualified as S


type Command m = m ()


runApp :: MonadUnliftIO m => [AppOption] -> ShareCLI m () -> m ()
runApp opts action = do

  getLocalConfigDir' >>=
    liftIO . createDirectoryIfMissing True

  getLocalConfigFile >>= \fn -> do
    here <- liftIO $ doesFileExist fn

    unless here do
      liftIO $ appendFile fn ""

  env <- liftIO (newAppEnv opts)
  let db = view appDb env

  setLogging @INFO   defLog
  setLogging @ERROR  (logPrefix "" . toStderr)
  setLogging @WARN   (logPrefix "" . toStdout)
  setLogging @NOTICE (logPrefix "" . toStdout)

  when ( AppDebugOpt `elem` opts || AppTraceOpt `elem` opts) do
    setLogging @DEBUG  (logPrefix "" . toStderr)

  when (AppTraceOpt `elem` opts) do
    setLogging @TRACE (logPrefix "" . toStderr)

  flip runContT pure $ do
    void $ ContT $ bracket (async (runPipe db)) cancel

    lift $ withAppEnv env do
      withState populateState
      loadAllEncryptionStuff
      action

  setLoggingOff @INFO
  setLoggingOff @ERROR
  setLoggingOff @WARN
  setLoggingOff @NOTICE
  setLoggingOff @DEBUG
  setLoggingOff @TRACE


withAppEnv :: MonadIO m => AppEnv -> ShareCLI m a -> m a
withAppEnv env action = do
  runReaderT (fromShareCLI action) env


newAppEnv :: forall m . MonadUnliftIO m => [AppOption] -> m AppEnv
newAppEnv opts = do
  let dbOpts = dbPipeOptsDef

  w <- getWorkingDir

  conf <- readConfig

  let sonameOpt  = runReader (cfgValue @RpcUnixOpt @(Maybe String) @(Reader [Syntax C])) conf

  rchan  <- orThrowUser "refchan not set" (runReader (cfgValue @RefChanOpt @(Maybe RChan)) conf)

  sonameDetect <- detectRPC

  soname <- orThrowUser "rpc not detected" (sonameOpt <|> sonameDetect)

  AppEnv opts conf rchan
    <$> (getLocalStatePath >>= newDBPipeEnv dbOpts)
    <*> pure w
    <*> pure soname
    <*> newIORef Nothing

withState :: (MonadReader AppEnv m, MonadIO m)
          => DBPipeM m b
          -> m b

withState m = do
  d <- asks (view appDb)
  withDB d m


makeGK0Key :: forall e s m . ( AppPerks m
                             , HasProtocol e (ServiceProto StorageAPI e)
                             , s ~ Encryption L4Proto
                             )
                     => RpcEndpoints e
                     -> ShareCLI m (Maybe GK0Key)

makeGK0Key rpc = runMaybeT do
  lift (getOwnRefChanHeadRef rpc)
    >>= toMPlus
    <&> GK0Key


getGK0 :: forall e s m . ( AppPerks m
                         , HasProtocol e (ServiceProto StorageAPI e)
                         , ForGroupKeySymm HBS2Basic
                         , s ~ HBS2Basic
                         )
       => RpcEndpoints e
       -> ShareCLI m (GK0 s)
getGK0 rpc = do

  rchan <- asks (view appRefChan)

  let sto = AnyStorage (StorageClient (rpcStorage rpc))

  gk0key <- makeGK0Key @e rpc
              >>= orThrowUser "makeGK0Key(1): refchan not available"

  mgk <- runMaybeT do
          gkh <- toMPlus =<< lift (withState $ selectGK0 gk0key)

          debug $ "found gk!" <+> pretty gkh

          runExceptT (readFromMerkle sto (SimpleKey (fromHashRef gkh)))
             >>= toMPlus
             <&> deserialiseOrFail @(GK0 s)
             >>= toMPlus

  case mgk of
    Just x  -> do
      pure x

    Nothing -> do
      hd <- getRefChanHead @L4Proto sto (RefChanHeadKey (toRefChanId rchan))
              >>= orThrowUser "makeGK0Key(2): refchan not available"

      let readers = view refChanHeadReaders hd & HashSet.toList
      gk <- generateGroupKey @s Nothing readers
      href <- writeAsMerkle sto (serialise gk) <&> HashRef

      withState (insertGK0 gk0key href >> commitAll)

      debug $ "generated gk0!" <+> pretty href

      pure gk

getOwnRefChanHeadRef :: forall e s m . ( AppPerks m
                                       , HasProtocol e (ServiceProto StorageAPI e)
                                       , s ~ Encryption L4Proto
                                       )
                     => RpcEndpoints e
                     -> ShareCLI m (Maybe HashRef)
getOwnRefChanHeadRef rpc = do

  let sto = AnyStorage (StorageClient (rpcStorage rpc))

  runMaybeT do
    rchan <- toMPlus =<< lift (cfgValue @RefChanOpt @(Maybe RChan))
    let puk = toRefChanId rchan
    getRef sto (RefChanHeadKey @s puk)
      >>= toMPlus
      <&> HashRef

withRpcClientUnix :: forall a e m . ( MonadUnliftIO m
                                  , HasProtocol e (ServiceProto PeerAPI e)
                                  , HasProtocol e (ServiceProto StorageAPI e)
                                  , HasProtocol e (ServiceProto RefChanAPI e)
                                  , e ~ UNIX
                                  , MonadReader AppEnv m
                                  )
                  => ( RpcEndpoints e -> m a )
                  -> m a

withRpcClientUnix action = do

  -- FIXME: use-ContT

  soname <- asks (view appRpcSock)

  client <- race ( pause @'Seconds 1) (newMessagingUnix False 1.0 soname) `orDie` "hbs2-peer rpc timeout!"

  messaging <- async $ runMessagingUnix client
  link messaging

  rpcPeer    <- makeServiceCaller @PeerAPI @e (fromString soname)
  rpcStorage <- makeServiceCaller @StorageAPI @e (fromString soname)
  rpcRefChan <- makeServiceCaller @RefChanAPI @e (fromString soname)

  let endpoints = [ Endpoint @e rpcPeer
                  , Endpoint @e rpcStorage
                  , Endpoint @e rpcRefChan
                  ]

  c1 <- async $ liftIO $ runReaderT (runServiceClientMulti endpoints) client

  link c1

  r <- action $ RpcEndpoints rpcPeer rpcStorage rpcRefChan

  pause @'Seconds 0.1

  cancel c1

  void $ waitAnyCatchCancel [c1, messaging]

  pure r


loadSigil :: forall e s m . ( s ~ Encryption e
                            , ForSigil e
                            , AppPerks m
                            ) => ShareCLI m (PubKey 'Sign s, SigilData e)
loadSigil = do

  dir <- getLocalConfigDir

  path' <- cfgValue @SigilPathOpt @(Maybe String)
            >>= orThrowUser "sigil not set"

  let nonLocalPath = List.isPrefixOf "./" path' || List.isPrefixOf "/" path'

  path <- if not nonLocalPath then do
            pure $ dir </> path'
          else do
            pure path'

  trace $ "SIGIL PATH" <+> pretty path

  sigil <- liftIO $ (BS.readFile path <&> parseSerialisableFromBase58 @(Sigil e))
             >>= orThrowUser ("invalid sigil format" <+> pretty path)

  w@(_,sd) <- orThrowUser "malformed sigil" (unboxSignedBox0 @(SigilData e) (sigilData sigil))

  pure w

loadAllEncryptionStuff :: AppPerks m => ShareCLI m ()
loadAllEncryptionStuff = do

  -- 1. загружаем sigil
  (pk, sd) <- loadSigil @L4Proto

  trace $ "sigil loaded" <+> pretty (AsBase58 pk)

  enc <- runKeymanClient do
      cr  <- loadCredentials pk
                  >>= orThrowUser "can't find credentials"

      enc <- loadKeyRingEntry (sigilDataEncKey sd)
                  >>= orThrowUser "can't find keyring entry"

      pure $ EncryptionStuff cr enc

  encIO <- asks (view appEnc)

  writeIORef encIO (Just enc)
  debug "encryption data loaded ok"


data UpdateFileMethod = UpdateFileForce
                      | UpdateFileSync

updateFile :: (AppPerks m, HasProtocol e (ServiceProto StorageAPI e))
           => RpcEndpoints e
           -> RemoteFile
           -> ShareCLI m ()
updateFile rpc fe = do
  dir <- asks (view appWorkDir)
  replica <- isReplica
  if replica then do
    updateFileMethod UpdateFileForce rpc fe
  else do
    updateFileMethod UpdateFileSync rpc fe

updateFileMethod :: (AppPerks m, HasProtocol e (ServiceProto StorageAPI e))
                 => UpdateFileMethod
                 -> RpcEndpoints e
                 -> RemoteFile
                 -> ShareCLI m ()
updateFileMethod UpdateFileForce rpc fe = do

  dir <- asks (view appWorkDir)

  let key = _remoteFileKey fe

  let fn = dir </> toFilePath key

  let sto = AnyStorage (StorageClient (rpcStorage rpc))

  encStuff <- asks (view appEnc)
                >>= readIORef
                >>= orThrowUser "credentials not available"

  let kr = [view kre encStuff]

  for_ (getDirs key) $ \d -> do
    let fpath = dir </> d
    here <- liftIO $ doesFileExist fpath
    when here do
      liftIO (removeFile fpath)
    liftIO $ createDirectoryIfMissing True fpath

  here <- liftIO $ doesFileExist fn

  l <- withState (selectLocalFile key)

  let lh = view localFileHash <$> l

  when (lh /= Just (_remoteLocalHash fe) || not here) do
    info $ "update file" <+> pretty key

    let h = view remoteTree fe & fromHashRef

    lbs <- runExceptT (readFromMerkle sto (ToDecryptBS kr h))
          >>= orThrowUser ("can't read file" <+> pretty h <+> pretty key)

    liftIO $ AwL.atomicWriteFile fn lbs

updateFileMethod UpdateFileSync rpc fe = do
  w <- asks (view appWorkDir)
  let sto = AnyStorage (StorageClient (rpcStorage rpc))

  encStuff <- asks (view appEnc)
                >>= readIORef
                >>= orThrowUser "credentials not available"

  let kr = [view kre encStuff]

  let key = _remoteFileKey fe

  (doUpdate, mt) <- withState do
    let fn = _remoteFileKey fe
    lf <- selectLocalFile (_remoteFileKey fe)
    -- floc <- selectLocalFile (_remoteFileKey fe)
    let tLoc = _localFileModTime <$> lf
    let tRem = Just (_remoteFileTime fe)

    let rhash = Just $ _remoteLocalHash fe
    let lhash = _localFileHash <$> lf

    pure (tRem > tLoc && rhash /= lhash, tRem)

  dont <- dontPost

  when (doUpdate && not dont) do

    let dirs = getDirs key

    info $ "U" <+> pretty key <+> pretty (_remoteTree fe)

    for_ dirs $ \d -> do
      let fpath = w </> d
      isFile <- liftIO $ doesFileExist fpath

      when isFile do
        -- TODO: unique-rename?
        fnew <- renameFileUniq fpath
        info $ "renamed" <+> pretty fpath <+> pretty fnew

      debug $ "create dir" <+> pretty fpath
      liftIO $ createDirectoryIfMissing True fpath

    let h = view remoteTree fe & fromHashRef

    lbs <- runExceptT (readFromMerkle sto (ToDecryptBS kr h))
          >>= orThrowUser ("can't read file" <+> pretty h <+> pretty key)

    let fn = w </> toFilePath key

    liftIO $ AwL.atomicWriteFile fn lbs
    forM_ mt (liftIO . setModificationTime fn)

renameFileUniq :: MonadUnliftIO  m => FilePath -> m FilePath
renameFileUniq fs = do

  fnew' <- S.head_ do
    for_ [1..] $ \i -> do
      let new = fs <> "~" <> show i
      here <- liftIO (doesFileExist new)
      unless here do
        S.yield new

  fnew <- orThrowUser ("can't rename file" <> pretty fs) fnew'

  liftIO $ renameFile fs fnew

  pure fnew

isMissed :: (AppPerks m, MonadReader AppEnv m)
         => AnyStorage
         -> HashRef
         -> m Bool

isMissed sto h = do
  miss <- withState (selectMissed h)
  case miss of
    Just False -> pure False
    _  -> do
         missed <- S.head_ (findMissedBlocks2 sto h) <&> isJust
         withState (insertMissed h missed)
         pure missed

scanState :: forall e m . ( AppPerks m
                          , HasProtocol e (ServiceProto StorageAPI e)
                          , HasProtocol e (ServiceProto RefChanAPI e)
                          )
          => RpcEndpoints e
          -> ShareCLI m HashRef

scanState rpc = do

  debug "scanState"

  encStuff <- asks (view appEnc)
                >>= readIORef
                >>= orThrowUser "credentials not available"

  let kr = view kre encStuff

  let sto = AnyStorage (StorageClient (rpcStorage rpc))
  refchan <- asks (toRefChanId . view appRefChan)

  debug $ "scan state for" <+> pretty (AsBase58 refchan)

  rv <- callService @RpcRefChanGet (rpcRefChan rpc) refchan
          >>= orThrowUser "getRefchan: rpc failure"
          >>= orThrowUser "refchan not found"

  debug $ "refchan value" <+> pretty rv

  withState do
    seen <- selectSeen rv
    unless seen do
      scanTx sto rv
      commitAll

  props <- withState selectProposes

  -- FIXME: cache-somehow
  ((px,e), meta) <- findGoodNewBlock kr sto props
                      >>= orThrowUser "no meta block found"

  withState do
    for_ (mdFiles meta) $ \fe -> do
      insertRemoteFile px (realToFrac e) meta fe
    commitAll

  rfs <- withState $ selectRemoteFiles px

  for_ rfs $ \rf -> do
    updateFile rpc rf

  withState $ insertSeen rv

  pure px

  where

    findGoodNewBlock kr sto props = do
      runMaybeT (go props)

      where

        go [] = mzero
        go (p:ps) = do

          let btx = fst p
          missed <- lift $ isMissed sto btx
          if missed then
            go ps
          else do

            what <- S.head_ do
              walkMerkle (fromHashRef btx) (getBlock sto) $ \case
                Right ( (hx:_) :: [HashRef] ) -> do
                  S.yield hx

                _ -> pure ()

            hmeta <- toMPlus what

            meta <- runExceptT (readFromMerkle sto (ToDecryptBS [kr] (fromHashRef hmeta)))
                      >>= toMPlus
                      <&> GZip.decompress
                      <&> deserialiseOrFail @MetaData
                      >>= toMPlus

            if List.null (mdFiles meta) then do
              go ps
            else
              pure (p,meta)

    scanTx sto rv =
      -- FIXME: dont-process-twice
      walkMerkle (fromHashRef rv) (getBlock sto) $ \case
        Left h   -> warn $ "missed block" <+> pretty h

        Right (hs ::[HashRef]) -> void $ runMaybeT do
          trace $ "got some" <+> pretty (length hs)

          for_ hs $ \htx -> void $ runMaybeT do

            seen <- lift $ lift $ selectSeen htx

            -- debug $ "SEEN" <+> pretty seen <+> pretty htx
            guard (not seen)

            bs <- toMPlus =<< getBlock sto (fromHashRef htx)
            tx <- toMPlus $ deserialiseOrFail @(RefChanUpdate L4Proto) bs

            case tx of
              Accept _ box  -> do
                (_, txx@(AcceptTran mt _ hp)) <- toMPlus $ unboxSignedBox0 box
                trace $ "tx accept" <+> pretty htx <+> pretty hp <+> pretty mt
                t <- toMPlus mt
                lift $ lift $ insertAccept htx hp (fromIntegral t)

              Propose _ box -> do
                (_, ProposeTran _ pbox :: ProposeTran L4Proto) <- toMPlus $ unboxSignedBox0 box
                (_, bs2) <- toMPlus $ unboxSignedBox0 pbox

                let wtf = [ tryDetect (hashObject bs) (LBS.fromStrict bs2) ]

                mytx <- [ ha | AnnotatedHashRef _ ha <- universeBi wtf ] & listToMaybe & toMPlus

                trace $ "tx propose" <+> pretty htx <+> pretty mytx
                lift $ lift $ insertPropose htx mytx

            lift $ lift $ insertSeen htx

dontPost :: AppPerks m => ShareCLI m Bool
dontPost = do
  opts <- asks ( view appOpts )
  replica <- isReplica
  pure $ replica || or [ True | AppDontPostOpt <- opts ]

isReplica :: AppPerks m => ShareCLI m Bool
isReplica = do
  re <- asks _appOpts <&> (AppReplicaOpt `elem`)
  conf <- getConf
  pure $ re || or [ True | ListVal [SymbolVal "replica"] <- conf ]

updateLocalState :: AppPerks m => ShareCLI m ()
updateLocalState = do

  debug "updateLocalState"

  skip <- cfgValue @IgnoreOpt @(Set String) <&> Set.toList

  dir <- asks (view appWorkDir)

  let d = makeEntryKey mempty dir

  q <- newTQueueIO

  es <- liftIO (F.listFiles skip dir (atomically . writeTQueue q . makeEntryKey d))
        >> atomically (flushTQueue q)

  withState do
    for_ es $ \e -> do
      let fn = toFilePath e
      t <- liftIO $ getModificationTime fn

      lf <- selectLocalFile e

      let newF = isNothing lf || (view localFileModTime <$> lf) /= Just t

      when newF do
        h <- localHash (toFilePath e)
        insertLocalFile e t h

    commitAll

postState :: forall e s m . ( AppPerks m
                            , HasProtocol e (ServiceProto RefChanAPI e)
                            , HasProtocol e (ServiceProto StorageAPI e)
                            , s ~ HBS2Basic
                            )

          => RpcEndpoints e
          -> HashRef -- ^ current state
          -> ShareCLI m ()
postState rpc px = do

  debug "postState"

  encStuff <- asks (view appEnc)
                >>= readIORef
                >>= orThrowUser "credentials not available"

  let kr = view kre encStuff

  let (KeyringKeys pk sk) = view kre encStuff

  let sto = AnyStorage (StorageClient (rpcStorage rpc))
  refchan <- asks (toRefChanId . view appRefChan)

  -- генерим gk0 если нету:
  gk0key <- makeGK0Key rpc
              >>= orThrowUser "can't make gk0key (perhaps refchan is not available)"

  debug $ "gk0 key" <+> pretty gk0key

  gk0 <- getGK0 rpc
  gkh <- writeAsMerkle sto (serialise gk0)

  debug $ "got GK0, okay"

  gks <- Symm.lookupGroupKey sk pk gk0
          & orThrow (userError $ show ("*** Can't decrypt group key" <+> pretty gkh))

  w <- asks (view appWorkDir)
  locals <- withState selectLocalFiles

  withState do
    fee <- S.toList_ $ for_ locals $ \l -> do
      let key = _localFileKey l
      let fpath = w </> toFilePath key
      r <- lift $ selectRemoteFile px key

      let rhash = _remoteLocalHash <$> r
      let rtree = _remoteTree <$> r
      let lhash = _localFileHash l

      here <- liftIO $ doesFileExist fpath

      when here do
        if Just lhash == rhash && isJust r then do

          -- FIXME: only-if-readers-are-chanhed
          --  делать только если поменялись читатели,
          --  иначе будет тормозить на большом числе файлов
          override <- genTreeOverride sto encStuff gk0 (fromJust rtree)

          case override of
            Just (Left{}) -> do
              -- nothing happen, no action required
              S.yield $ Left $ FileEntry key lhash (fromJust rtree)

            Just (Right new) -> do
              -- tree is overriden with new gk0
              S.yield $ Right $ FileEntry key lhash new

            Nothing -> do
              -- errors during tree overriding, post new file
              warn $ "errors while overriding tree" <+> pretty rtree
              tree <- writeEncryptedFile gks gk0 sto fpath lhash
              S.yield $ Right $ FileEntry key lhash tree

        else do
          tree <- writeEncryptedFile gks gk0 sto fpath lhash
          S.yield $ Right $ FileEntry key lhash tree

    let fe = List.sortOn (view feKey) (lefts fee <> rights fee)

    let updated = not $ List.null (rights fee)

    when updated do

      let gk1 = mempty

      let base = Just px

      let md = MetaData base gk1 fe

      -- можно брать только правые
      let hashes = [ t | FileEntry _ _ t <- fe ]

      for_ (rights fee) $ \f -> do
        info $ "M" <+> pretty (_feTree f) <+> pretty (_feKey f)

      let metabs = serialise md
                    & GZip.compressWith (defaultCompressParams { compressLevel = bestCompression })

      manifest <- getLocalConfigDir <&> (</> "manifest")
      liftIO $ AwL.atomicWriteFile manifest metabs

      lh <- localHash manifest
      mfhash <- writeEncryptedFile gks gk0 sto manifest lh

      let pt = toPTree (MaxSize 1024) (MaxNum 1024) (mfhash : hashes) -- FIXME: settings

      metaHash <- makeMerkle 0 pt $ \(_,_,bss) -> do
                    void $ liftIO (putBlock sto bss)

      info $ "entries:" <+> pretty (length hashes) <+> pretty metaHash

      let tx = AnnotatedHashRef Nothing (HashRef metaHash)
      let ssk = view (creds . peerSignSk) encStuff
      let spk = view (creds . peerSignPk) encStuff

      let box = makeSignedBox @L4Proto @BS.ByteString spk ssk (LBS.toStrict $ serialise tx)

      dont <- lift dontPost

      unless dont do
        debug "POST TX"
        r <- callService @RpcRefChanPropose (rpcRefChan rpc) (refchan, box)
        pure ()

  where
    -- genTreeOverride :: AnyStorage -> EncryptionStuff -> GK0 HBS2Basic -> HashRef -> m ()
    genTreeOverride sto enc gk0 tree = do
      let (KeyringKeys pk sk) = view kre enc
      runMaybeT do
        obj <- MaybeT $ getBlock sto (fromHashRef tree)
        case tryDetect (fromHashRef tree) obj of
          MerkleAnn ann@(MTreeAnn {_mtaCrypt = EncryptGroupNaClSymm2 o gkh0 nonce}) -> do

            gk0old <- runExceptT (readFromMerkle sto (SimpleKey gkh0))
                        >>= toMPlus
                        <&> deserialiseOrFail @(GroupKey 'Symm s)
                        >>= toMPlus

            let rcptOld = HashMap.keysSet (recipients gk0old)
            let rcptNew = HashMap.keysSet (recipients gk0)

            if rcptOld == rcptNew then do
              pure (Left tree)
            else do

              gksOld <- toMPlus $ Symm.lookupGroupKey sk pk gk0old

              gk1 <- generateGroupKey @s (Just gksOld) (HashSet.toList rcptNew)

              gk1h <- writeAsMerkle sto (serialise gk1)

              let newCrypt = EncryptGroupNaClSymm2 o gk1h nonce
              let newTreeBlock = ann { _mtaCrypt = newCrypt }

              newTree <- enqueueBlock sto (serialise newTreeBlock)
                          >>= toMPlus
                          <&> HashRef

              pure (Right newTree)

          _ -> mzero


runSync :: AppPerks m => ShareCLI m ()
runSync = do

  replica <- isReplica
  info $ "replica:" <+> pretty replica

  flip runContT pure $ do

    rpc <- ContT $ withRpcClientUnix

    lift do
      updateLocalState
      px <- scanState rpc
      updateLocalState
      postState rpc px

writeEncryptedFile :: forall m s nonce . (MonadIO m, Serialise nonce, s ~ HBS2Basic)
                   => GroupSecret
                   -> GroupKey 'Symm s
                   -> AnyStorage
                   -> FilePath
                   -> nonce
                   -> m HashRef
writeEncryptedFile gks gk0 sto fn h = do
  let nonce = LBS.drop 1 (serialise h) & LBS.toStrict

  let sk1 = SipKey 2716310006254639645 507093936407764973
  let sk2 = SipKey 9209724780415729085 2720900864410773155
  let (SipHash a) = BA.sipHash sk1 nonce
  let (SipHash b) = BA.sipHash sk2 nonce

  let bsStream = flip readChunkedBS defBlockSize =<< liftIO (LBS.readFile fn)

  -- TODO: fix-metadata
  let source = ToEncryptSymmBS @s gks
                               (Right gk0)
                               nonce
                               bsStream
                               NoMetaData
                               (Just (EncryptGroupNaClSymmBlockSIP (a,b)))

  th <- runExceptT (writeAsMerkle sto source)
          >>= orThrowUser "can't encrypt data"

  pure $ HashRef th

