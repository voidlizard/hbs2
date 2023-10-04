module HBS2.Peer.RPC.API.Storage where

data RpcStorageHasBlock
data RpcStorageGetBlock
data RpcStorageEnqueueBlock
data RpcStoragePutBlock
data RpcStorageGetChunk
data RpcStorageGetRef
data RpcStorageUpdateRef
data RpcStorageDelRef

type StorageAPI = '[ RpcStorageHasBlock
                   , RpcStorageHasBlock
                   , RpcStorageGetBlock
                   , RpcStorageEnqueueBlock
                   , RpcStoragePutBlock
                   , RpcStorageGetChunk
                   , RpcStorageGetRef
                   , RpcStorageUpdateRef
                   , RpcStorageDelRef
                   ]

