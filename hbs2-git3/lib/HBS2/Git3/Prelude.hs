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
  , pattern GitHashLike
  , maxCLevel
  ) where

import HBS2.Prelude.Plated as Exported
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
import HBS2.Data.Types.SignedBox as Exported
import HBS2.Storage as Exported
import HBS2.Storage.Operations.Class as Exported
import HBS2.System.Logger.Simple.ANSI as Exported

import HBS2.Git3.Types as Exported
-- import HBS2.Git3.State.Types as Exported

import HBS2.System.Dir

import Data.Config.Suckless.Syntax

import Codec.Compression.Zstd (maxCLevel)
import Codec.Serialise
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader as Exported
import Control.Monad.Trans.Cont as Exported
import Control.Monad.Trans.Maybe as Exported
import Data.ByteString qualified as BS
import Data.Coerce as Exported
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

quit :: MonadUnliftIO m => m ()
quit = liftIO Q.exitSuccess


pattern GitHashLike:: forall {c} . GitHash -> Syntax c
pattern GitHashLike x <- (
  \case
    StringLike s  -> fromStringMay @GitHash s
    LitIntVal 0   -> Just $ GitHash (BS.replicate 20 0)
    _             -> Nothing
      -> Just x )

