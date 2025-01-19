{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language UndecidableInstances #-}
module HBS2.Git3.State.Internal.Types
  ( module HBS2.Git3.State.Internal.Types
  , pattern SignPubKeyLike
  ) where


import HBS2.Git3.Prelude
import HBS2.Git3.Config.Local
import HBS2.Net.Auth.Credentials
import HBS2.KeyMan.Keys.Direct
import HBS2.System.Dir
import HBS2.Data.Detect (readLogThrow)
import HBS2.CLI.Run.MetaData (getTreeContents)

import Data.Config.Suckless

import HBS2.Defaults as Exported
import HBS2.OrDie as Exported
import HBS2.Data.Types.Refs as Exported
import HBS2.Base58 as Exported
import HBS2.Merkle as Exported
import HBS2.Misc.PrettyStuff as Exported
import HBS2.Net.Auth.Credentials
import HBS2.Peer.Proto.LWWRef as Exported
import HBS2.Peer.Proto.RefLog as Exported
import HBS2.Peer.RPC.API.RefLog as Exported
import HBS2.Peer.RPC.API.Peer as Exported
import HBS2.Peer.RPC.API.LWWRef as Exported
import HBS2.Peer.RPC.API.Storage as Exported
import HBS2.Peer.RPC.Client hiding (encode,decode)
import HBS2.Peer.RPC.Client.Unix hiding (encode,decode)
import HBS2.Peer.RPC.Client.StorageClient
import HBS2.Peer.CLI.Detect (detectRPC)
import HBS2.Data.Types.SignedBox as Exported
import HBS2.Storage as Exported
import HBS2.Storage.Operations.Class as Exported
import HBS2.System.Logger.Simple.ANSI as Exported

import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TE
import Data.ByteString.Lazy qualified as LBS
import Data.Word

import Data.Kind
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Lens.Micro.Platform

import System.FilePath

unit :: FilePath
unit = "hbs2-git"

getStatePath :: (MonadIO m, Pretty ref) => ref -> m FilePath
getStatePath p = do
  d <- getConfigPath
  pure $ d </> show (pretty p)

data HBS2GitExcepion =
    RefLogNotSet
  | GitRepoRefNotSet
  | GitRepoRefEmpty
  | GitRepoManifestMalformed
  | RefLogCredentialsNotMatched
  | RpcTimeout
  deriving stock (Show,Typeable)

instance Exception HBS2GitExcepion

defSegmentSize :: Int
defSegmentSize = 50 * 1024 * 1024

defCompressionLevel :: Int
defCompressionLevel = maxCLevel

defIndexBlockSize :: Natural
defIndexBlockSize = 32 * 1024 * 1024

type HBS2GitPerks m = (MonadUnliftIO  m)


class GitWritePacksOpts a where
  excludeParents :: a -> Bool

instance GitWritePacksOpts () where
  excludeParents = const True

data GitWritePacksOptVal =
  WriteFullPack
  deriving stock (Eq,Ord,Show,Generic)

instance Hashable GitWritePacksOptVal

instance GitWritePacksOpts (HashSet GitWritePacksOptVal) where
  excludeParents o = not $ HS.member WriteFullPack o

data Git3Exception =
    Git3PeerNotConnected
  | Git3ReflogNotSet
  | Git3RpcTimeout
  deriving (Show,Typeable,Generic)

instance Exception Git3Exception

data Git3Env =
    Git3Disconnected
    { gitPackedSegmentSize :: TVar Int
    , gitCompressionLevel  :: TVar Int
    , gitIndexBlockSize    :: TVar Natural
    , gitRepoKey           :: TVar (Maybe GitRepoKey)
    }
  | Git3Connected
    { peerSocket  :: FilePath
    , peerStorage :: AnyStorage
    , peerAPI     :: ServiceCaller PeerAPI UNIX
    , reflogAPI   :: ServiceCaller RefLogAPI UNIX
    , lwwAPI      :: ServiceCaller LWWRefAPI UNIX
    , gitRepoKey  :: TVar (Maybe GitRepoKey)
    , gitRefLog   :: TVar (Maybe GitRemoteKey)
    , gitPackedSegmentSize :: TVar Int
    , gitCompressionLevel  :: TVar Int
    , gitIndexBlockSize :: TVar Natural
    }

class HasExportOpts m where
  setPackedSegmedSize :: Int -> m ()
  getPackedSegmetSize :: m Int
  getCompressionLevel :: m Int
  setCompressionLevel :: Int -> m ()


instance (MonadIO m, MonadReader Git3Env m) => HasExportOpts m where
  getPackedSegmetSize = asks gitPackedSegmentSize >>= readTVarIO
  setPackedSegmedSize x = do
    e <- asks gitPackedSegmentSize
    atomically $ writeTVar e x

  getCompressionLevel = asks gitCompressionLevel >>= readTVarIO
  setCompressionLevel x = do
    e <- asks gitCompressionLevel
    atomically $ writeTVar e (min maxCLevel x)

instance (MonadIO m, MonadReader Git3Env m) => HasStorage m where
  getStorage = do
    e <- ask
    case e of
      Git3Disconnected{} -> throwIO Git3PeerNotConnected
      Git3Connected{..} -> pure peerStorage

class MonadIO m => HasIndexOptions m where
  getIndexBlockSize :: m Natural
  setIndexBlockSize :: Natural -> m ()

instance (MonadIO m, MonadReader Git3Env m) => HasIndexOptions m where
  getIndexBlockSize = asks gitIndexBlockSize >>= readTVarIO

  setIndexBlockSize n = do
    e <- asks gitIndexBlockSize
    atomically $ writeTVar e n

class HasGitRemoteKey m where
  getGitRemoteKey :: m (Maybe GitRemoteKey)
  getGitRepoKey   :: m (Maybe GitRepoKey)
  setGitRepoKey   :: GitRepoKey -> m ()

instance (MonadIO m) => HasGitRemoteKey (Git3 m) where
  getGitRemoteKey =
    ask >>= \case
      Git3Connected{..} -> readTVarIO gitRefLog
      _ -> pure Nothing

  getGitRepoKey = do
    e <- ask
    liftIO $ readTVarIO (gitRepoKey e)

  setGitRepoKey k = do
    e <- ask
    liftIO $ atomically $ writeTVar (gitRepoKey e) (Just k)

getStatePathM :: forall m . (HBS2GitPerks m, HasGitRemoteKey m) => m FilePath
getStatePathM = do
  k <- getGitRemoteKey >>= orThrow RefLogNotSet
  getStatePath (AsBase58 k)

updateRepoKey :: forall m . HBS2GitPerks m => GitRepoKey -> Git3 m ()
updateRepoKey key = do

  setGitRepoKey key

  mf <- getRepoManifest

  let reflog = lastMay [ x
                       | ListVal [SymbolVal "reflog", SignPubKeyLike x]  <- mf
                       ]

  ask >>= \case
        Git3Connected{..} -> atomically $ writeTVar gitRefLog reflog
        _ -> none

getRepoRefMaybe :: forall m . HBS2GitPerks m => Git3 m (Maybe (LWWRef 'HBS2Basic))
getRepoRefMaybe = do
  lwwAPI  <- getClientAPI @LWWRefAPI @UNIX

  pk <- getGitRepoKey >>= orThrow GitRepoRefNotSet

  callRpcWaitMay @RpcLWWRefGet (TimeoutSec 1) lwwAPI (LWWRefKey pk)
    >>= orThrow RpcTimeout

getRepoRefLogCredentials :: forall m . HBS2GitPerks m
                         => Git3 m (PubKey 'Sign 'HBS2Basic, PrivKey 'Sign HBS2Basic)

getRepoRefLogCredentials = do
  -- FIXME: memoize-this
  mf <- getRepoManifest
  rk <- getGitRepoKey >>= orThrow GitRepoRefNotSet

  reflog <- getGitRemoteKey >>= orThrow Git3ReflogNotSet

  creds <- runKeymanClientRO (loadCredentials rk)
             >>= orThrowUser ("not found credentials for"  <+> pretty (AsBase58 rk))

  seed <- [ x | ListVal [SymbolVal "seed", LitIntVal x ] <- mf ]
             & lastMay & orThrow GitRepoManifestMalformed
             <&> fromIntegral @_ @Word64

  let sk = view peerSignSk creds

  (p,s) <- derivedKey @'HBS2Basic @'Sign seed sk

  unless ( p == reflog ) do
    throwIO RefLogCredentialsNotMatched

  pure (p,s)

getRepoManifest :: forall m . HBS2GitPerks m => Git3 m [Syntax C]
getRepoManifest = do

  sto <- getStorage

  LWWRef{..} <- getRepoRefMaybe >>= orThrow GitRepoRefEmpty

  mfref <- readLogThrow (getBlock sto) lwwValue
               <&> headMay
               >>= orThrow GitRepoManifestMalformed

  runExceptT (getTreeContents sto mfref)
     >>= orThrowPassIO
     <&> TE.decodeUtf8With TE.lenientDecode . LBS.toStrict
     <&> parseTop
     >>= orThrow GitRepoManifestMalformed

newtype Git3 (m :: Type -> Type) a = Git3M { fromGit3 :: ReaderT Git3Env m a }
                   deriving newtype ( Applicative
                                    , Functor
                                    , Monad
                                    , MonadIO
                                    , MonadUnliftIO
                                    , MonadReader Git3Env
                                    , MonadTrans
                                    )

type Git3Perks m = ( MonadIO m
                   , MonadUnliftIO m
                   )


instance MonadUnliftIO m => HasClientAPI PeerAPI UNIX (Git3 m) where
  getClientAPI = do
    ask  >>= \case
       Git3Disconnected{} -> throwIO Git3PeerNotConnected
       Git3Connected{..} -> pure peerAPI

instance (MonadUnliftIO m) => HasClientAPI RefLogAPI UNIX (Git3 m) where
  getClientAPI = do
    ask  >>= \case
       Git3Disconnected{} -> throwIO Git3PeerNotConnected
       Git3Connected{..} -> pure reflogAPI


instance (MonadUnliftIO m) => HasClientAPI LWWRefAPI UNIX (Git3 m) where
  getClientAPI = do
    ask  >>= \case
       Git3Disconnected{} -> throwIO Git3PeerNotConnected
       Git3Connected{..} -> pure lwwAPI

nullGit3Env :: MonadIO m => m Git3Env
nullGit3Env = Git3Disconnected
                <$> newTVarIO defSegmentSize
                <*> newTVarIO defCompressionLevel
                <*> newTVarIO defIndexBlockSize
                <*> newTVarIO Nothing

connectedDo :: (MonadIO m) => Git3 m a -> Git3 m a
connectedDo what = do
  env <- ask
  debug $ red "connectedDo"
  case env of
    Git3Disconnected{} -> do
      throwIO Git3PeerNotConnected

    _ -> what

withGit3Env :: Git3Perks m => Git3Env -> Git3 m a -> m a
withGit3Env env a = runReaderT (fromGit3 a) env

runGit3 :: Git3Perks m => Git3Env -> Git3 m b -> m b
runGit3 env action = withGit3Env env action


recover :: Git3 IO a -> Git3 IO a
recover m = fix \again -> do
  catch m $ \case
    Git3PeerNotConnected -> do

      soname <- detectRPC
                  `orDie` "can't locate hbs2-peer rpc"

      flip runContT pure do

        client <- lift $ race (pause @'Seconds 1) (newMessagingUnix False 1.0 soname)
                    >>= orThrowUser ("can't connect to" <+> pretty soname)

        void $ ContT $ withAsync $ runMessagingUnix client

        peer       <- makeServiceCaller @PeerAPI (fromString soname)
        refLogAPI  <- makeServiceCaller @RefLogAPI (fromString soname)
        storageAPI <- makeServiceCaller @StorageAPI (fromString soname)
        lwwAPI     <- makeServiceCaller @LWWRefAPI (fromString soname)

        -- let sto = AnyStorage (StorageClient storageAPI)

        let endpoints = [ Endpoint @UNIX  peer
                        , Endpoint @UNIX  refLogAPI
                        , Endpoint @UNIX  lwwAPI
                        , Endpoint @UNIX  storageAPI
                        ]

        void $ ContT $ withAsync $ liftIO $ runReaderT (runServiceClientMulti endpoints) client

        let sto = AnyStorage (StorageClient storageAPI)

        rk <- lift $ getGitRepoKey >>= orThrow GitRepoRefNotSet

        notice $ yellow $ "REPOKEY" <+> pretty (AsBase58 rk)

        connected <- Git3Connected soname sto peer refLogAPI lwwAPI
                        <$> newTVarIO Nothing
                        <*> newTVarIO Nothing
                        <*> newTVarIO defSegmentSize
                        <*> newTVarIO defCompressionLevel
                        <*> newTVarIO defIndexBlockSize

        liftIO $ withGit3Env connected do

          updateRepoKey rk

          ref <- getGitRemoteKey >>= orThrow GitRepoManifestMalformed

          state <- getStatePath (AsBase58 ref)
          mkdir state

          again

    e -> throwIO e

