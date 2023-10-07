{-# OPTIONS_GHC -fno-warn-orphans #-}
module HBS2.Peer.RPC.API.Storage where

import HBS2.Actors.Peer
import HBS2.Net.Proto.Service
import HBS2.Net.Messaging.Unix
import HBS2.Peer.RPC.Internal.Types
import HBS2.Storage (Offset,Size)
import HBS2.Data.Types.Refs (HashRef(..),RefAlias(..))

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

type instance Input RpcStorageHasBlock = HashRef
type instance Output RpcStorageHasBlock = Maybe Integer

type instance Input RpcStorageGetBlock = HashRef
type instance Output RpcStorageGetBlock = Maybe ByteString

type instance Input RpcStorageEnqueueBlock = ByteString
type instance Output RpcStorageEnqueueBlock = Maybe HashRef

type instance Input RpcStoragePutBlock = ByteString
type instance Output RpcStoragePutBlock = Maybe HashRef

type instance Input RpcStorageDelBlock = HashRef
type instance Output RpcStorageDelBlock = ()

type instance Input RpcStorageGetChunk = (HashRef, Offset, Size)
type instance Output RpcStorageGetChunk = Maybe ByteString

type instance Input RpcStorageGetRef = RefAlias
type instance Output RpcStorageGetRef = Maybe HashRef

type instance Input RpcStorageUpdateRef = (RefAlias, HashRef)
type instance Output RpcStorageUpdateRef = ()

type instance Input RpcStorageDelRef = RefAlias
type instance Output RpcStorageDelRef = ()

