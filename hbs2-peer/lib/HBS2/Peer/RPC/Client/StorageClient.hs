{-# Language AllowAmbiguousTypes #-}
{-# Language UndecidableInstances #-}
module HBS2.Peer.RPC.Client.StorageClient
  ( Storage(..)
  , StorageClient(..)
  ) where

import HBS2.Prelude.Plated

import HBS2.Hash
import HBS2.Data.Types.Refs (HashRef(..),refAlias)
import HBS2.Net.Proto.Service
import HBS2.Storage

import HBS2.Peer.RPC.Internal.Storage()

import HBS2.Peer.RPC.API.Storage

import Data.Functor
import Data.ByteString.Lazy (ByteString)
import Data.Either

import HBS2.System.Logger.Simple

newtype StorageClient e =
  StorageClient { fromStorageClient :: ServiceCaller StorageAPI e }

instance ( MonadIO m
         , HasProtocol e (ServiceProto StorageAPI e)
         )
  => Storage (StorageClient e) HbSync ByteString m where

  putBlock s lbs = liftIO do
    debug $  "CLIENT: putBlock!"
    callService @RpcStoragePutBlock @StorageAPI (fromStorageClient s) lbs
      <&> either (const Nothing) (fmap fromHashRef)

  enqueueBlock s lbs = liftIO do
    debug $  "CLIENT: enqueueBlock!"
    callService @RpcStorageEnqueueBlock @StorageAPI (fromStorageClient s) lbs
      <&> either (const Nothing) (fmap fromHashRef)

  getBlock s key = liftIO do
    callService @RpcStorageGetBlock (fromStorageClient s) (HashRef key)
      <&> fromRight Nothing

  getChunk s k off size = liftIO do
    callService @RpcStorageGetChunk (fromStorageClient s) (HashRef k,  off, size)
      <&> fromRight Nothing

  hasBlock s k = liftIO do
    callService @RpcStorageHasBlock (fromStorageClient s) (HashRef k)
      <&> fromRight Nothing

  delBlock s h = liftIO do
    void $ callService @RpcStorageDelBlock (fromStorageClient s) (HashRef h)

  updateRef s ref v = liftIO do
    void $ callService @RpcStorageUpdateRef (fromStorageClient s) (refAlias ref, HashRef v)

  getRef s ref = liftIO do
    callService @RpcStorageGetRef (fromStorageClient s) (refAlias ref)
      <&> either (const Nothing) (fmap fromHashRef)

  delRef s ref = liftIO do
    void $ callService @RpcStorageDelRef (fromStorageClient s) (refAlias ref)

