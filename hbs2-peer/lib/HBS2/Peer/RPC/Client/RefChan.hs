{-# Language AllowAmbiguousTypes #-}
module HBS2.Peer.RPC.Client.RefChan where

import HBS2.OrDie
import HBS2.Storage
import HBS2.Storage.Operations.ByteString
import HBS2.Data.Types.SignedBox

import HBS2.Peer.Proto.RefChan
import HBS2.Peer.Prelude
import HBS2.Peer.RPC.API.RefChan
import HBS2.Peer.RPC.API.Storage
import HBS2.Peer.RPC.Client.Internal
import HBS2.Peer.RPC.Client.StorageClient

import Data.Coerce
import Control.Monad.Except
import Control.Monad.Trans.Maybe
import UnliftIO


getRefChanHeadHash :: forall proto m . ( MonadUnliftIO m
                                       , HasClientAPI RefChanAPI proto m
                                       , HasProtocol proto (ServiceProto RefChanAPI proto)
                                       )
                   => PubKey 'Sign 'HBS2Basic
                   -> m (Maybe HashRef)
getRefChanHeadHash puk = do
  api <- getClientAPI @RefChanAPI @proto
  callRpcWaitMay  @RpcRefChanHeadGet (TimeoutSec 1) api puk >>= \case
    Nothing -> throwIO RpcTimeoutError
    Just e  -> pure e


getRefChanHead :: forall proto m . ( MonadUnliftIO m
                                   , HasClientAPI RefChanAPI proto m
                                   , HasClientAPI StorageAPI proto m
                                   , HasProtocol proto (ServiceProto RefChanAPI proto)
                                   , HasProtocol proto (ServiceProto StorageAPI proto)
                                   )
                   => PubKey 'Sign 'HBS2Basic
                   -> m (Maybe (RefChanHeadBlock L4Proto))
getRefChanHead puk = do

  sto <- getClientAPI @StorageAPI @proto <&> AnyStorage . StorageClient

  runMaybeT do
    hx  <- lift (getRefChanHeadHash @proto puk) >>= toMPlus
    lbs <- runExceptT (readFromMerkle sto (SimpleKey (coerce hx)))
              >>= orThrowPassIO

    -- FIXME: error-on-bad-signature
    (_, hdblk) <- unboxSignedBox @(RefChanHeadBlock L4Proto) @'HBS2Basic lbs
                    & toMPlus

    pure hdblk

