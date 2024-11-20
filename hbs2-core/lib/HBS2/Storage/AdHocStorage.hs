{-|
Обёртка над AnyStorage чтобы в рантайме для инстанса Storage подменить
реализацию getBlock. Например на такую, которая будет отправлять ненайденный
блок на закачку.
-}
module HBS2.Storage.AdHocStorage where

import HBS2.Hash
import HBS2.Prelude.Plated
import HBS2.Storage

import Data.ByteString.Lazy qualified as LBS
import Data.Kind

data AdHocStorage (m :: Type -> Type) k b = AdHocStorage
    { adHocStorageAnySto :: AnyStorage
    , adHocStorageGetBlock :: k -> m (Maybe b)
    }

instance
    (MonadIO m)
    => Storage (AdHocStorage m (Hash HbSync) LBS.ByteString) HbSync LBS.ByteString m
    where
    putBlock (AdHocStorage {..}) = putBlock adHocStorageAnySto
    enqueueBlock (AdHocStorage {..}) = enqueueBlock adHocStorageAnySto
    getBlock (AdHocStorage {..}) = adHocStorageGetBlock
    getChunk (AdHocStorage {..}) = getChunk adHocStorageAnySto
    hasBlock (AdHocStorage {..}) = hasBlock adHocStorageAnySto
    updateRef (AdHocStorage {..}) = updateRef adHocStorageAnySto
    getRef (AdHocStorage {..}) = getRef adHocStorageAnySto
    delBlock (AdHocStorage {..}) = delBlock adHocStorageAnySto
    delRef (AdHocStorage {..}) = delRef adHocStorageAnySto
