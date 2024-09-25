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
import Data.Function

import Control.Monad
import Control.Monad.Except
import Control.Exception
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as LBS
-- hbs2-core/lib/HBS2/Net/Auth/Credentials.hs
-- importimport Data.List.Split (chunksOf)


data WriteMerkleIOError =
  WriteMerkleIOError
  deriving (Show,Typeable,Generic)

instance Exception WriteMerkleIOError


instance (MonadIO m, h ~ HbSync, Storage s h ByteString m) => MerkleWriter ByteString h s m where
  type instance ToBlockW ByteString = ByteString
  writeAsMerkle sto bs = do

    hashes <- do
      chu <- S.toList_ (readChunkedBS bs defBlockSize)
      for chu $ \chunk -> do
        enqueueBlock sto chunk
         >>= liftIO . maybe (throwIO WriteMerkleIOError) (pure . HashRef)

    -- FIXME: handle-hardcode
    let pt = toPTree (MaxSize defHashListChunk) (MaxNum defTreeChildNum) hashes -- FIXME: settings

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


readChunkedBS :: (Integral a, Monad m)
              => ByteString
              -> a
              -> S.Stream (S.Of ByteString) m ()

readChunkedBS bs size = foo bs
  where
    foo =
      fix $ \loop leftover -> do
        let (chunk, rest) = LBS.splitAt (fromIntegral size) leftover
        unless (LBS.null chunk) do
          S.yield chunk
          loop rest



