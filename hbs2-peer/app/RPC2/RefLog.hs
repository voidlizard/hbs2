{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language UndecidableInstances #-}
module RPC2.RefLog
  ( module RPC2.RefLog
  , module HBS2.Peer.RPC.Internal.Types
  ) where

import HBS2.Peer.Prelude

import HBS2.Actors.Peer
import HBS2.Hash
import HBS2.Base58
import HBS2.Data.Types.Refs (HashRef(..))
import HBS2.Events
import HBS2.Peer.Proto
import HBS2.Storage
import HBS2.Net.Messaging.Unix

import PeerTypes
import RefLog (doRefLogBroadCast)

import HBS2.Peer.RPC.Internal.Types
import HBS2.Peer.RPC.API.RefLog

import Lens.Micro.Platform
import Control.Monad.Reader

type RefLogContext m = (MonadIO m, HasRpcContext RefLogAPI RPC2Context m)

instance (Monad m)
  => HasRpcContext RefLogAPI RPC2Context (ResponseM UNIX (ReaderT RPC2Context m)) where
  -- type instance RpcContext RefLogAPI = RPC2Context
  getRpcContext = lift ask

instance (RefLogContext m) => HandleMethod m RpcRefLogGet where

  handleMethod pk = do
    co <- getRpcContext @RefLogAPI
    debug $ "rpc.reflogGet:"  <+> pretty (AsBase58 pk)
                               <+> pretty (hashObject @HbSync (RefLogKey @HBS2Basic pk))

    liftIO $ withPeerM (rpcPeerEnv co) $ do
      let sto = rpcStorage co
      liftIO (getRef sto (RefLogKey @HBS2Basic pk)) <&> fmap HashRef

instance (RefLogContext m) => HandleMethod m RpcRefLogFetch where

  handleMethod pk = do
    co <- getRpcContext @RefLogAPI
    debug $ "rpc.reflogFetch:"  <+> pretty (AsBase58 pk)

    liftIO $ withPeerM (rpcPeerEnv co) $ do
      broadCastMessage (RefLogRequest @L4Proto pk)

instance (RefLogContext m) => HandleMethod m RpcRefLogPost where

  handleMethod msg = do
    co <- getRpcContext @RefLogAPI
    let pk = view refLogId msg
    debug $ "rpc.reflogPost:"  <+> pretty (AsBase58 pk)

    liftIO $ withPeerM (rpcPeerEnv co) $ do
      emit @L4Proto RefLogUpdateEvKey (RefLogUpdateEvData (pk, msg, Nothing))
      doRefLogBroadCast msg


