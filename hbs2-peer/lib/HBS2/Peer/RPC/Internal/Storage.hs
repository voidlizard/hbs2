{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language UndecidableInstances #-}
module HBS2.Peer.RPC.Internal.Storage
  ( module HBS2.Peer.RPC.API.Storage
  , module HBS2.Peer.RPC.Class
  )
  where


import HBS2.Prelude.Plated
import HBS2.Actors.Peer.Types
import HBS2.Data.Types.Refs (HashRef(..))
import HBS2.Storage
import HBS2.Peer.RPC.Class
import HBS2.Peer.RPC.API.Storage

import HBS2.Net.Proto.Service
import HBS2.System.Logger.Simple

import Data.Functor

-- type StorageContext m = (MonadIO m, HasStorage m)
type StorageContext m = (MonadIO m, HasStorage m)

instance (StorageContext m) => HandleMethod m RpcStorageHasBlock where

  handleMethod href = do
    debug $ "rpc.storage.hasBlock" <+> pretty href
    sto <- getStorage
    liftIO $ hasBlock sto (fromHashRef href)

instance (StorageContext m) => HandleMethod m RpcStorageGetBlock where

  handleMethod href = do
    debug $ "rpc.storage.getBlock" <+> pretty href
    sto <- getStorage
    liftIO $ getBlock sto (fromHashRef href)

instance (StorageContext m) => HandleMethod m RpcStorageEnqueueBlock where

  handleMethod lbs = do
    debug $ "rpc.storage.enqueueBlock"
    sto <- getStorage
    liftIO $ enqueueBlock sto lbs <&> fmap HashRef

instance (StorageContext m) => HandleMethod m RpcStoragePutBlock where

  handleMethod lbs = do
    debug $ "rpc.storage.putBlock"
    sto <- getStorage
    liftIO $ putBlock sto lbs <&> fmap HashRef

instance (StorageContext m) => HandleMethod m RpcStorageDelBlock where

  handleMethod href = do
    debug $ "rpc.storage.delBlock" <+> pretty href
    sto <- getStorage
    liftIO $ delBlock sto (fromHashRef href)

instance (StorageContext m) => HandleMethod m RpcStorageGetChunk where

  handleMethod (h,o,s) = do
    sto <- getStorage
    liftIO $ getChunk sto (fromHashRef h) o s

instance (StorageContext m) => HandleMethod m RpcStorageGetRef where

  handleMethod ref = do
    debug $ "rpc.storage.getRef" <+> pretty ref
    sto <- getStorage
    liftIO $ getRef sto ref <&> fmap HashRef

instance (StorageContext m) => HandleMethod m RpcStorageUpdateRef where

  handleMethod (ref, val) = do
    debug $ "rpc.storage.updateRef" <+> pretty ref
    sto <- getStorage
    liftIO $ updateRef sto ref (fromHashRef val)

instance (StorageContext m) => HandleMethod m RpcStorageDelRef where

  handleMethod ref = do
    debug $ "rpc.storage.delRef" <+> pretty ref
    sto <- getStorage
    liftIO $ delRef sto ref


