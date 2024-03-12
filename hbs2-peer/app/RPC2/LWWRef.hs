{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language UndecidableInstances #-}
module RPC2.LWWRef where


import HBS2.Peer.Prelude

import HBS2.Actors.Peer
import HBS2.Data.Types.SignedBox
import HBS2.Peer.Proto
import HBS2.Peer.Proto.LWWRef
import HBS2.Storage
import HBS2.Net.Messaging.Unix

import PeerTypes

import HBS2.Peer.RPC.Internal.Types
import HBS2.Peer.RPC.API.LWWRef

import Lens.Micro.Platform
import Control.Monad.Reader
import Control.Monad.Trans.Maybe

type LWWRefContext m = (MonadIO m, HasRpcContext LWWRefAPI RPC2Context m)

instance (Monad m)
  => HasRpcContext LWWRefAPI RPC2Context (ResponseM UNIX (ReaderT RPC2Context m)) where
  getRpcContext = lift ask

instance (LWWRefContext m) => HandleMethod m RpcLWWRefGet where

  handleMethod key = do
    co <- getRpcContext @LWWRefAPI
    debug "rpc.LWWRefContext"

    let penv = rpcPeerEnv co
    liftIO $ withPeerM penv $ do
      sto <- getStorage
      runMaybeT do
        rv <- getRef sto key >>= toMPlus
        val <- getBlock sto rv >>= toMPlus
                <&> unboxSignedBox @(LWWRef L4Proto) @L4Proto
                >>= toMPlus

        pure $ snd val

instance LWWRefContext m => HandleMethod m RpcLWWRefFetch where

  handleMethod key = do
    co <- getRpcContext @LWWRefAPI
    debug $ "rpc.LWWRefFetch" <+> pretty key

    let penv = rpcPeerEnv co
    liftIO $ withPeerM penv $ do
      gossip (LWWRefProto1 @L4Proto (LWWProtoGet key))

instance LWWRefContext m => HandleMethod m RpcLWWRefUpdate where

  handleMethod box = do
    -- co <- getRpcContext @LWWRefAPI
    debug "rpc.LWWRefUpdate"
    pure ()


