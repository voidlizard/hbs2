{-# OPTIONS_GHC -fno-warn-orphans #-}
module HBS2.Peer.RPC.API.Storage where

import HBS2.Actors.Peer
import HBS2.Net.Proto.Service
import HBS2.Net.Messaging.Unix
import HBS2.Peer.RPC.Internal.Types

import Control.Monad.Reader
import Data.ByteString.Lazy (ByteString)
import Codec.Serialise

data RpcStorageHasBlock
data RpcStorageGetBlock
data RpcStorageEnqueueBlock
data RpcStoragePutBlock
data RpcStorageDelBlock
data RpcStorageGetChunk
data RpcStorageGetRef
data RpcStorageUpdateRef
data RpcStorageDelRef

type StorageAPI = '[ RpcStorageHasBlock
                   , RpcStorageGetBlock
                   , RpcStorageEnqueueBlock
                   , RpcStoragePutBlock
                   , RpcStorageDelBlock
                   , RpcStorageGetChunk
                   , RpcStorageGetRef
                   , RpcStorageUpdateRef
                   , RpcStorageDelRef
                   ]

instance HasProtocol UNIX  (ServiceProto StorageAPI UNIX) where
  type instance ProtocolId (ServiceProto StorageAPI UNIX) = 0xDA2374610001
  type instance Encoded UNIX = ByteString
  decode = either (const Nothing) Just . deserialiseOrFail
  encode = serialise


instance (Monad m)
  => HasRpcContext StorageAPI RPC2Context (ResponseM UNIX (ReaderT RPC2Context m)) where
  getRpcContext = lift ask

instance Monad m => HasStorage (ReaderT RPC2Context m) where
  getStorage = asks rpcStorage

