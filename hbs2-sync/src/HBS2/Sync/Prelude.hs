{-# Language UndecidableInstances #-}
{-# Language AllowAmbiguousTypes #-}
{-# Language TemplateHaskell #-}
{-# Language MultiWayIf #-}
{-# Language PatternSynonyms #-}
{-# Language ViewPatterns #-}
module HBS2.Sync.Prelude
  ( module HBS2.Sync.Prelude
  , module Exported
  ) where


import HBS2.Prelude.Plated as Exported
import HBS2.Base58 as Exported
import HBS2.OrDie as Exported
import HBS2.Data.Types.Refs as Exported
import HBS2.Net.Auth.Credentials as Exported hiding (encode,decode)
import HBS2.Net.Proto.Service hiding (encode,decode)
import HBS2.Peer.Proto.RefChan.Types as Exported
import HBS2.Net.Messaging.Unix (runMessagingUnix,newMessagingUnix)
import HBS2.Storage as Exported
import HBS2.Storage.Compact as Compact
import HBS2.Peer.CLI.Detect
import HBS2.Peer.RPC.Client hiding (encode,decode) -- as Exported
import HBS2.Peer.RPC.Client.Unix (UNIX)
import HBS2.Peer.RPC.Client.StorageClient
import HBS2.Peer.RPC.API.Peer
import HBS2.Peer.RPC.API.RefChan
import HBS2.Peer.RPC.API.Storage
import HBS2.System.Logger.Simple.ANSI as Exported
import HBS2.System.Dir
import HBS2.Misc.PrettyStuff as Exported

import HBS2.CLI.Run hiding (PeerException(..))

import HBS2.KeyMan.Keys.Direct as Exported ( runKeymanClient
                                           , loadCredentials
                                           , loadKeyRingEntries
                                           , extractGroupKeySecret
                                           , KeyManClientEnv
                                           )

import HBS2.KeyMan.Keys.Direct qualified as KE

import Data.Config.Suckless as Exported
import Data.Config.Suckless.Script as Exported

import Codec.Serialise as Exported
import Control.Applicative
import Control.Monad.Reader as Exported
import Control.Monad.Trans.Cont as Exported
import Control.Monad.Trans.Maybe
import Data.Coerce as Exported
import Data.Either as Exported
import Data.List qualified as L
import Data.List (stripPrefix)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe as Exported
import Data.Time.Clock.POSIX
import Data.Word
import Lens.Micro.Platform
import System.Exit qualified as Exit
import System.Directory
import UnliftIO as Exported

{- HLINT ignore "Functor law" -}
{- HLINT ignore "Eta reduce" -}

type MyRefChan = PubKey 'Sign 'HBS2Basic

data DirSyncEnv =
  DirSyncEnv
  { _dirSyncPath     :: Maybe FilePath
  , _dirSyncRefChan  :: Maybe MyRefChan
  , _dirSyncCreds    :: Maybe (PeerCredentials 'HBS2Basic)
  , _dirSyncInclude  :: [FilePattern]
  , _dirSyncExclude  :: [FilePattern]
  , _dirSyncBackup   :: Bool
  , _dirSyncFollowSymlinks :: Bool
  }
  deriving stock (Generic)

makeLenses 'DirSyncEnv

instance Monoid DirSyncEnv where
  mempty = DirSyncEnv Nothing Nothing Nothing mempty defExcl False False
    where
      defExcl = ["**/.hbs2-sync/*"]


instance Semigroup DirSyncEnv where
  (<>) a b = DirSyncEnv ( view dirSyncPath b <|> view dirSyncPath a )
                        ( view dirSyncRefChan b  <|> view dirSyncRefChan a )
                        ( view dirSyncCreds b  <|> view dirSyncCreds a )
                        (L.nub $  view dirSyncInclude a <> view dirSyncInclude b )
                        (L.nub $  view dirSyncExclude a <> view dirSyncExclude b )
                        ( view dirSyncBackup b  || view dirSyncBackup a )
                        ( view dirSyncFollowSymlinks b  || view dirSyncFollowSymlinks a )

instance Pretty DirSyncEnv where
  pretty e = do
    vcat $ catMaybes
         [ pure ("; path" <+> pretty (view dirSyncPath e))
         , view dirSyncRefChan e >>= \x -> pure $ pretty $ mkList  @C [mkSym "refchan", mkSym (show $ pretty (AsBase58 x))]
         , view dirSyncCreds e >>=
                 \x -> pure $ pretty
                            $ mkList @C [mkSym "sign", mkSym (show $ pretty $ AsBase58 $ view peerSignPk  x)]
         , pure $ vcat (fmap (mkPattern "include") (view dirSyncInclude e))
         , pure $ vcat (fmap (mkPattern "exclude") (view dirSyncExclude e))
         ]

    where
      mkPattern name p = pretty $ mkList @C [mkSym name, mkSym p]

data SyncEnv =
  SyncEnv
  { refchanAPI      :: ServiceCaller RefChanAPI UNIX
  , storageAPI      :: ServiceCaller StorageAPI UNIX
  , peerAPI         :: ServiceCaller PeerAPI UNIX
  , dirSyncEnv      :: TVar (Map FilePath DirSyncEnv)
  , dirThis         :: TVar (Maybe FilePath)
  , dirTombs        :: TVar (Map FilePath (CompactStorage HbSync))
  , dirCache        :: TVar (Map FilePath (CompactStorage HbSync))
  , keymanClientEnv :: TVar (Maybe KeyManClientEnv)
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

class Monad m => HasTombs m where
  getTombs :: m (CompactStorage HbSync)
  closeTombs :: m ()


class Monad m => HasCache m where
  getCache :: m (CompactStorage HbSync)
  closeCache :: m ()

class Monad m => HasKeyManClient m where
  getKeyManClientEnv :: m KeyManClientEnv

instance MonadUnliftIO m => HasTombs (SyncApp m) where
  getTombs = do
    SyncEnv{..} <- ask >>= orThrow PeerNotConnectedException
    path <- getRunDir

    mbTomb <- dirTombs & readTVarIO
               <&> Map.lookup path

    case mbTomb of
      Just tomb -> pure tomb
      Nothing -> do
        -- FIXME: path-hardcode
        let tombsPath = path </> ".hbs2-sync" </> "state" </> "tombs"
        mkdir (dropFileName tombsPath)
        stoTombs <- compactStorageOpen mempty tombsPath
        atomically (modifyTVar dirTombs (Map.insert path stoTombs))
        pure stoTombs

  closeTombs = do
    path <- getRunDir

    void $ runMaybeT do

      SyncEnv{..} <- lift ask >>= toMPlus

      tombs <- dirTombs & readTVarIO
                   <&> Map.lookup path
                   >>= toMPlus

      compactStorageClose tombs

instance MonadUnliftIO m => HasCache (SyncApp m) where
  getCache = do
    SyncEnv{..} <- ask >>= orThrow PeerNotConnectedException
    path <- getRunDir

    mbCache <- dirCache & readTVarIO
               <&> Map.lookup path

    case mbCache of
      Just tomb -> pure tomb
      Nothing -> do
        -- FIXME: path-hardcode
        let cachePath = path </> ".hbs2-sync" </> "state" </> "txcache"
        mkdir (dropFileName cachePath)
        stoCache <- compactStorageOpen mempty cachePath
        atomically (modifyTVar dirCache (Map.insert path stoCache))
        pure stoCache

  closeCache = do
    path <- getRunDir

    void $ runMaybeT do

      SyncEnv{..} <- lift ask >>= toMPlus

      cache <- dirCache & readTVarIO
                   <&> Map.lookup path
                   >>= toMPlus

      compactStorageClose cache


instance MonadUnliftIO m => HasKeyManClient (SyncApp m) where
  getKeyManClientEnv = do
    SyncEnv{..} <- ask >>= orThrow PeerNotConnectedException
    e <- readTVarIO keymanClientEnv

    case e of
      Just env -> pure env
      -- NOTE: race-but-harmless
      --   если у нас в двух потоках позовут этот метод,
      --   то будет открыто два соединения, и сохранено
      --   последнее. Поскольку соединение readonly это
      --   безобидно. В целом, надо навести с этим порядок
      Nothing -> do
        env <- KE.newKeymanClientEnv
        atomically  $ writeTVar keymanClientEnv (Just env)
        pure env

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

        dsync <- newTVarIO mempty
        this  <- newTVarIO Nothing
        tombs <- newTVarIO mempty
        cache <- newTVarIO mempty
        dummyKeyman <- newTVarIO Nothing

        let env = Just (SyncEnv refChanAPI storageAPI peerAPI dsync this tombs cache dummyKeyman)

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
  | DirNotSet
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

-- FIXME: move-to-suckless-conf
class IsContext c => ToSexp c a where
  toSexp :: a -> Syntax c



newtype AsSexp c a = AsSexp a

pattern TombLikeOpt :: forall {c} . Syntax c
pattern TombLikeOpt <- ListVal [StringLike "tomb:", tombLikeValue -> True]

tombLikeValue :: Syntax c -> Bool
tombLikeValue = \case
  StringLike "#t" -> True
  StringLike "true" -> True
  StringLike "yes" -> True
  StringLike "tomb" -> True
  LitBoolVal True -> True
  _ -> False

instance (IsContext c, ToSexp c w) => Pretty (AsSexp c w) where
  pretty (AsSexp s) = pretty (toSexp @c s)


backupMode :: (MonadUnliftIO m, HasRunDir m) => m Bool
backupMode = do
  d <- getRunDir

  b <- runMaybeT do
         env <- getRunDirEnv d >>= toMPlus
         pure $ view dirSyncBackup env

  pure $ fromMaybe False b





class MonadIO m => HasRunDir m where
  getRunDir       :: m FilePath
  getRunDirEnv    :: FilePath -> m (Maybe DirSyncEnv)
  alterRunDirEnv  :: FilePath -> ( Maybe DirSyncEnv -> Maybe DirSyncEnv ) -> m ()

instance (MonadUnliftIO m) => HasRunDir (SyncApp m) where
  getRunDir = ask >>= orThrow PeerNotConnectedException
                  >>= readTVarIO . dirThis
                  >>= orThrow DirNotSet

  getRunDirEnv dir =  do
    env <- ask >>= orThrow PeerNotConnectedException
               >>= readTVarIO . dirSyncEnv
    pure $ Map.lookup dir env

  alterRunDirEnv dir action = do
    tenv <- ask >>= orThrow PeerNotConnectedException
            <&> dirSyncEnv
    atomically $ modifyTVar tenv (Map.alter action dir)

instance HasRunDir m => HasRunDir (RunM c m) where
  getRunDir = lift getRunDir
  getRunDirEnv d = lift (getRunDirEnv d)
  alterRunDirEnv d a = lift (alterRunDirEnv d a)

instance HasRunDir m => HasRunDir (MaybeT m) where
  getRunDir = lift getRunDir
  getRunDirEnv d = lift (getRunDirEnv d)
  alterRunDirEnv d a = lift (alterRunDirEnv d a)

instance HasRunDir m => HasRunDir (ContT r m) where
  getRunDir = lift getRunDir
  getRunDirEnv d = lift (getRunDirEnv d)
  alterRunDirEnv d a = lift (alterRunDirEnv d a)

instance HasTombs m => HasTombs (ContT r m) where
  getTombs = lift getTombs
  closeTombs = lift closeTombs

instance HasTombs m => HasTombs (MaybeT m) where
  getTombs = lift getTombs
  closeTombs = lift closeTombs

instance (Monad m, HasTombs m) => HasTombs (RunM c m) where
  getTombs = lift getTombs
  closeTombs = lift closeTombs

instance HasCache m => HasCache (ContT r m) where
  getCache = lift getCache
  closeCache = lift closeCache

instance HasCache m => HasCache (MaybeT m) where
  getCache = lift getCache
  closeCache = lift closeCache

instance (Monad m, HasCache m) => HasCache (RunM c m) where
  getCache = lift getCache
  closeCache = lift closeCache

instance (MonadUnliftIO m, HasKeyManClient m) => HasKeyManClient (RunM c m) where
  getKeyManClientEnv = lift getKeyManClientEnv

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


quit :: forall m . MonadUnliftIO m => m ()
quit = liftIO Exit.exitSuccess

die :: forall a m . (MonadUnliftIO m, Pretty a) => a -> m ()
die what = liftIO do
  hPutDoc stderr (pretty what)
  Exit.exitFailure


