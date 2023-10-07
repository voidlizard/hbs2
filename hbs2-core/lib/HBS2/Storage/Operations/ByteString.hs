{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language AllowAmbiguousTypes #-}
{-# Language UndecidableInstances #-}
module HBS2.Storage.Operations.ByteString
  ( module HBS2.Storage.Operations.Class
  , module HBS2.Storage.Operations.ByteString
  ) where

import HBS2.Prelude.Plated
import HBS2.Hash
import HBS2.Storage
import HBS2.Merkle
import HBS2.Data.Types.Refs

import HBS2.Storage.Operations.Class

import HBS2.Defaults

import Streaming.Prelude qualified as S
import Streaming qualified as S
import Data.Function

import Control.Monad.Except
import Data.Bifunctor
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as LBS


instance (MonadIO m, h ~ HbSync, Storage s h ByteString m) => MerkleWriter ByteString h s m where
  type instance ToBlockW ByteString = ByteString
  writeAsMerkle sto bs = do

    hashes <- S.each (LBS.unpack bs)
                & S.chunksOf (fromIntegral defBlockSize )
                & S.mapped (fmap (first LBS.pack) . S.toList)
                & S.mapM (\blk -> enqueueBlock sto blk >> pure blk)
                -- & S.mapM (\blk -> putBlock sto blk >> pure blk)
                & S.map (HashRef . hashObject)
                & S.toList_

    -- FIXME: handle-hardcode
    let pt = toPTree (MaxSize 256) (MaxNum 256) hashes -- FIXME: settings

    makeMerkle 0 pt $ \(_,_,bss) -> do
      void $ putBlock sto bss

instance ( MonadIO m
         , MonadError OperationError m
         , Storage s HbSync ByteString m
         ) => MerkleReader ByteString s HbSync m where

  newtype instance TreeKey ByteString = SimpleKey (Hash HbSync)
  type instance ToBlockR ByteString = ByteString
  type instance ReadResult ByteString = ByteString

  readFromMerkle sto (SimpleKey h) = do

    pieces <- S.toList_ $ do
      walkMerkle h (lift . getBlock sto) $ \case
        Left{} -> throwError MissedBlockError

        Right (hrr :: [HashRef]) -> do

            forM_ hrr $ \hx -> do
              blk <- lift (getBlock sto (fromHashRef hx))
                        >>= maybe (throwError MissedBlockError) pure
              S.yield blk

    pure $ mconcat pieces


