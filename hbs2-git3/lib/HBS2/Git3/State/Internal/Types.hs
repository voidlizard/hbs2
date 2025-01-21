{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language UndecidableInstances #-}
module HBS2.Git3.State.Internal.Types
  ( module HBS2.Git3.State.Internal.Types
  , pattern SignPubKeyLike
  ) where

import HBS2.Git3.Prelude
import HBS2.Git3.Config.Local
import HBS2.System.Dir

import Data.Kind
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS

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
  | RefLogNotReady
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


getStatePathM :: forall m . (HBS2GitPerks m, HasGitRemoteKey m) => m FilePath
getStatePathM = do
  k <- getGitRemoteKey >>= orThrow RefLogNotSet
  getStatePath (AsBase58 k)

