{-# Language AllowAmbiguousTypes #-}
{-# Language UndecidableInstances #-}
module HBS2.Storage.Operations where

import HBS2.Prelude.Plated
import HBS2.Hash
import HBS2.Storage
import HBS2.Merkle
import HBS2.Data.Types.Refs
import HBS2.Defaults

import Streaming.Prelude qualified as S
import Streaming qualified as S
import Data.Functor
import Data.Function

import Data.Bifunctor
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as B


class (MonadIO m, Storage storage hash block m) => MerkleWriter block hash storage m where
  writeAsMerkle :: storage -> block -> m (Hash hash)

instance (MonadIO m, h ~ HbSync, Storage s h ByteString m) => MerkleWriter ByteString h s m where
  writeAsMerkle sto bs = do

    hashes <- S.each (B.unpack bs)
                & S.chunksOf (fromIntegral defBlockSize)
                & S.mapped (fmap (first B.pack) . S.toList)
                & S.mapM (\blk -> enqueueBlock sto blk >> pure blk)
                & S.map (HashRef . hashObject)
                & S.toList_

    -- FIXME: handle-hardcode
    let pt = toPTree (MaxSize 256) (MaxNum 256) hashes -- FIXME: settings
    makeMerkle 0 pt $ \(_,_,bss) -> void $ putBlock sto bss

