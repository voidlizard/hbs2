{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language RecordWildCards #-}
{-# Language UndecidableInstances #-}
{-# Language AllowAmbiguousTypes #-}
module HBS2.Git3.Prelude
  ( module HBS2.Git3.Prelude
  , module Exported
  , module HBS2.Peer.RPC.Client
  , module HBS2.Peer.RPC.Client.Unix
  , module Codec.Serialise
  , runExceptT
  ) where

import HBS2.Prelude.Plated as Exported
import HBS2.OrDie as Exported
import HBS2.Data.Types.Refs as Exported
import HBS2.Base58 as Exported
import HBS2.Merkle as Exported
import HBS2.Misc.PrettyStuff as Exported
import HBS2.Peer.Proto.RefLog as Exported
import HBS2.Peer.RPC.API.RefLog as Exported
import HBS2.Peer.RPC.API.Peer as Exported
import HBS2.Peer.RPC.Client hiding (encode,decode)
import HBS2.Peer.RPC.Client.Unix hiding (encode,decode)
import HBS2.Storage as Exported
import HBS2.Storage.Operations.Class as Exported
import HBS2.System.Logger.Simple.ANSI as Exported

import HBS2.Git3.Types as Exported

-- TODO: about-to-remove
import DBPipe.SQLite

import Codec.Compression.Zstd (maxCLevel)
import Codec.Serialise
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader as Exported
import Control.Monad.Trans.Cont as Exported
import Control.Monad.Trans.Maybe as Exported
import Data.Coerce as Exported
import Data.HashSet (HashSet(..))
import Data.HashSet qualified as HS
import Data.Kind
import System.Exit qualified as Q
import System.IO.MMap as Exported

import GHC.Natural as Exported
import UnliftIO as Exported


defSegmentSize :: Int
defSegmentSize = 50 * 1024 * 1024

defCompressionLevel :: Int
defCompressionLevel = maxCLevel

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
    { gitRefLog   :: TVar (Maybe GitRemoteKey)
    , gitPackedSegmentSize :: TVar Int
    , gitCompressionLevel  :: TVar Int
    }
  | Git3Connected
    { peerSocket  :: FilePath
    , peerStorage :: AnyStorage
    , peerAPI     :: ServiceCaller PeerAPI UNIX
    , reflogAPI   :: ServiceCaller RefLogAPI UNIX
    , gitRefLog   :: TVar (Maybe GitRemoteKey)
    , gitPackedSegmentSize :: TVar Int
    , gitCompressionLevel  :: TVar Int
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


