{-# OPTIONS_GHC -fno-warn-orphans #-}
module HBS2.Peer.RPC.Internal.Storage
  ( module HBS2.Peer.RPC.API.Storage
  , module HBS2.Peer.RPC.Class
  )
  where


import HBS2.Actors.Peer.Types
import HBS2.Prelude.Plated
import HBS2.Data.Types.Refs (HashRef(..),RefAlias(..))
import HBS2.Storage
import HBS2.Peer.RPC.Class
import HBS2.Peer.RPC.API.Storage

import HBS2.Net.Proto.Service

import Data.Functor
import Data.ByteString.Lazy ( ByteString )
import Control.Monad.Reader


instance Monad m => HasRpcContext AnyStorage (ReaderT AnyStorage m) where
  getRpcContext = ask

instance (MonadIO m, HasRpcContext AnyStorage m) => HandleMethod m RpcStorageHasBlock where
  type instance Input RpcStorageHasBlock = HashRef
  type instance Output RpcStorageHasBlock = Maybe Integer

  handleMethod href = do
    sto <- getRpcContext @AnyStorage
    liftIO $ hasBlock sto (fromHashRef href)

instance (MonadIO m, HasRpcContext AnyStorage m) => HandleMethod m RpcStorageGetBlock where
  type instance Input RpcStorageGetBlock = HashRef
  type instance Output RpcStorageGetBlock = Maybe ByteString

  handleMethod href = do
    sto <- getRpcContext @AnyStorage
    liftIO $ getBlock sto (fromHashRef href)

instance (MonadIO m, HasRpcContext AnyStorage m) => HandleMethod m RpcStorageEnqueueBlock where
  type instance Input RpcStorageEnqueueBlock = ByteString
  type instance Output RpcStorageEnqueueBlock = Maybe HashRef

  handleMethod lbs = do
    sto <- getRpcContext @AnyStorage
    liftIO $ enqueueBlock sto lbs <&> fmap HashRef

instance (MonadIO m, HasRpcContext AnyStorage m) => HandleMethod m RpcStoragePutBlock where
  type instance Input RpcStoragePutBlock = ByteString
  type instance Output RpcStoragePutBlock = Maybe HashRef

  handleMethod lbs = do
    sto <- getRpcContext @AnyStorage
    liftIO $ putBlock sto lbs <&> fmap HashRef

instance (MonadIO m, HasRpcContext AnyStorage m) => HandleMethod m RpcStorageGetChunk where
  type instance Input RpcStorageGetChunk = (HashRef, Offset, Size)
  type instance Output RpcStorageGetChunk = Maybe ByteString

  handleMethod (h,o,s) = do
    sto <- getRpcContext @AnyStorage
    liftIO $ getChunk sto (fromHashRef h) o s

instance (MonadIO m, HasRpcContext AnyStorage m) => HandleMethod m RpcStorageGetRef where
  type instance Input RpcStorageGetRef = RefAlias
  type instance Output RpcStorageGetRef = Maybe HashRef

  handleMethod ref = do
    sto <- getRpcContext @AnyStorage
    liftIO $ getRef sto ref <&> fmap HashRef

instance (MonadIO m, HasRpcContext AnyStorage m) => HandleMethod m RpcStorageUpdateRef where
  type instance Input RpcStorageUpdateRef = (RefAlias, HashRef)
  type instance Output RpcStorageUpdateRef = ()

  handleMethod (ref, val) = do
    sto <- getRpcContext @AnyStorage
    liftIO $ updateRef sto ref (fromHashRef val)

instance (MonadIO m, HasRpcContext AnyStorage m) => HandleMethod m RpcStorageDelRef where
  type instance Input RpcStorageDelRef = RefAlias
  type instance Output RpcStorageDelRef = ()

  handleMethod ref = do
    sto <- getRpcContext @AnyStorage
    liftIO $ delRef sto ref


