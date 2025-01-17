{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language RecordWildCards #-}
{-# Language UndecidableInstances #-}
{-# Language AllowAmbiguousTypes #-}
{-# Language PatternSynonyms #-}
{-# Language FunctionalDependencies #-}
module HBS2.Git3.Prelude
  ( module HBS2.Git3.Prelude
  , module Exported
  , module HBS2.Peer.RPC.Client
  , module HBS2.Peer.RPC.Client.Unix
  , module Codec.Serialise
  , runExceptT
  , pattern SignPubKeyLike
  ) where

import HBS2.Prelude.Plated as Exported
import HBS2.OrDie as Exported
import HBS2.Data.Types.Refs as Exported
import HBS2.Base58 as Exported
import HBS2.Merkle as Exported
import HBS2.Misc.PrettyStuff as Exported
import HBS2.Net.Auth.Credentials
import HBS2.Peer.Proto.RefLog as Exported
import HBS2.Peer.RPC.API.RefLog as Exported
import HBS2.Peer.RPC.API.Peer as Exported
import HBS2.Peer.RPC.API.LWWRef as Exported
import HBS2.Peer.RPC.API.Storage as Exported
import HBS2.Peer.RPC.Client hiding (encode,decode)
import HBS2.Peer.RPC.Client.Unix hiding (encode,decode)
import HBS2.Peer.RPC.Client.StorageClient
import HBS2.Peer.CLI.Detect
import HBS2.Storage as Exported
import HBS2.Storage.Operations.Class as Exported
import HBS2.System.Logger.Simple.ANSI as Exported

import HBS2.Git3.Types as Exported
import HBS2.Git3.State.Types as Exported

import HBS2.System.Dir

import Codec.Compression.Zstd (maxCLevel)
import Codec.Serialise
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader as Exported
import Control.Monad.Trans.Cont as Exported
import Control.Monad.Trans.Maybe as Exported
import Data.Coerce as Exported
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.HashPSQ qualified as HPSQ
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.HashPSQ (HashPSQ)
import Data.Kind
import System.Exit qualified as Q
import System.IO.MMap as Exported
import System.FilePattern as Exported

import GHC.Natural as Exported
import UnliftIO as Exported

data RefLogNotSetException =
  RefLogNotSetException
  deriving stock (Show,Typeable)

instance Exception  RefLogNotSetException

defSegmentSize :: Int
defSegmentSize = 50 * 1024 * 1024

defCompressionLevel :: Int
defCompressionLevel = maxCLevel

defIndexBlockSize :: Natural
defIndexBlockSize = 32 * 1024 * 1024

type HBS2GitPerks m = (MonadUnliftIO  m)


quit :: MonadUnliftIO m => m ()
quit = liftIO Q.exitSuccess


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
    { gitRefLog            :: TVar (Maybe GitRemoteKey)
    , gitPackedSegmentSize :: TVar Int
    , gitCompressionLevel  :: TVar Int
    , gitIndexBlockSize    :: TVar Natural
    }
  | Git3Connected
    { peerSocket  :: FilePath
    , peerStorage :: AnyStorage
    , peerAPI     :: ServiceCaller PeerAPI UNIX
    , reflogAPI   :: ServiceCaller RefLogAPI UNIX
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

class HasGitRemoteKey m where
  getGitRemoteKey :: m (Maybe GitRemoteKey)
  setGitRemoteKey :: GitRemoteKey -> m ()

instance (MonadIO m, MonadReader Git3Env m) => HasGitRemoteKey m where
  getGitRemoteKey = do
    e <- ask
    liftIO $ readTVarIO (gitRefLog e)

  setGitRemoteKey k = do
    e <- ask
    liftIO $ atomically $ writeTVar (gitRefLog e) (Just k)

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

nullGit3Env :: MonadIO m => m Git3Env
nullGit3Env = Git3Disconnected
                <$> newTVarIO Nothing
                <*> newTVarIO defSegmentSize
                <*> newTVarIO defCompressionLevel
                <*> newTVarIO defIndexBlockSize

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


getStatePathM :: forall m . (HBS2GitPerks m, HasGitRemoteKey m) => m FilePath
getStatePathM = do
  k <- getGitRemoteKey >>= orThrow RefLogNotSetException
  getStatePath (AsBase58 k)


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

        peerAPI    <- makeServiceCaller @PeerAPI (fromString soname)
        refLogAPI  <- makeServiceCaller @RefLogAPI (fromString soname)
        storageAPI <- makeServiceCaller @StorageAPI (fromString soname)
        lwwAPI     <- makeServiceCaller @LWWRefAPI (fromString soname)

        -- let sto = AnyStorage (StorageClient storageAPI)

        let endpoints = [ Endpoint @UNIX  peerAPI
                        , Endpoint @UNIX  refLogAPI
                        , Endpoint @UNIX  lwwAPI
                        , Endpoint @UNIX  storageAPI
                        ]

        void $ ContT $ withAsync $ liftIO $ runReaderT (runServiceClientMulti endpoints) client

        ref <- getGitRemoteKey >>= orThrowUser "remote ref not set"

        state <- getStatePath (AsBase58 ref)

        mkdir state

        let sto = AnyStorage (StorageClient storageAPI)

        connected <- Git3Connected soname sto peerAPI refLogAPI
                        <$> newTVarIO (Just ref)
                        <*> newTVarIO defSegmentSize
                        <*> newTVarIO defCompressionLevel
                        <*> newTVarIO defIndexBlockSize

        liftIO $ withGit3Env connected again

    e -> throwIO e


class Cached cache k v | cache -> k, cache -> v where
  isCached :: forall m . MonadIO m => cache -> k -> m Bool
  cached   :: forall m . MonadIO m => cache -> k -> m v -> m v
  uncache  :: forall m . MonadIO m => cache -> k -> m ()


newtype CacheTVH k v = CacheTVH (TVar (HashMap k v))

instance Hashable k => Cached (CacheTVH k v) k v where
  isCached (CacheTVH t) k = readTVarIO  t <&> HM.member k
  uncache (CacheTVH t) k = atomically (modifyTVar t (HM.delete k))
  cached (CacheTVH t) k a = do
    what <- readTVarIO t <&> HM.lookup k
    case what of
      Just x -> pure x
      Nothing -> do
        r <- a
        atomically $ modifyTVar t (HM.insert k r)
        pure r

data CacheFixedHPSQ  k v =
  CacheFixedHPSQ
  { _cacheSize :: Int
  , _theCache  :: TVar (HashPSQ k TimeSpec v)
  }

newCacheFixedHPSQ :: MonadIO m => Int -> m (CacheFixedHPSQ k v)
newCacheFixedHPSQ l = CacheFixedHPSQ l <$> newTVarIO HPSQ.empty

instance (Ord k, Hashable k) => Cached (CacheFixedHPSQ k v) k v where

  isCached CacheFixedHPSQ{..} k = readTVarIO _theCache <&> HPSQ.member k

  uncache CacheFixedHPSQ{..} k = atomically $ modifyTVar _theCache (HPSQ.delete k)

  cached CacheFixedHPSQ{..} k a = do
    w <- readTVarIO _theCache <&> HPSQ.lookup k
    case w of
      Just (_,e) -> pure e
      Nothing -> do
        v <- a

        t <- getTimeCoarse

        atomically do
          s <- readTVar _theCache <&> HPSQ.size

          when (s >= _cacheSize) do
            modifyTVar _theCache HPSQ.deleteMin

          modifyTVar _theCache (HPSQ.insert k t v)

          pure v

