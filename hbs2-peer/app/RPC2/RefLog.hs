{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language UndecidableInstances #-}
module RPC2.RefLog
  ( module RPC2.RefLog
  , module HBS2.Peer.RPC.Internal.Types
  ) where

import HBS2.Prelude.Plated

import HBS2.Actors.Peer
import HBS2.Hash
import HBS2.Base58
import HBS2.Data.Types.Refs (HashRef(..))
import HBS2.Events
import HBS2.Net.Proto.Definition()
import HBS2.Net.Proto.RefLog
import HBS2.Net.Proto.Service
import HBS2.Storage
import HBS2.Net.Messaging.Unix

import HBS2.System.Logger.Simple
import PeerTypes
import RefLog (doRefLogBroadCast)

import HBS2.Peer.RPC.Internal.Types
import HBS2.Peer.RPC.API.RefLog

import Data.Functor
import Lens.Micro.Platform
import Control.Monad.Reader

type RefLogContext m = (MonadIO m, HasRpcContext RefLogAPI RPC2Context m)

instance (Monad m)
  => HasRpcContext RefLogAPI RPC2Context (ResponseM UNIX (ReaderT RPC2Context m)) where
  -- type instance RpcContext RefLogAPI = RPC2Context
  getRpcContext = lift ask

instance (RefLogContext m) => HandleMethod m RpcRefLogGet where
  type instance Input RpcRefLogGet = PubKey 'Sign HBS2Basic
  type instance Output RpcRefLogGet = Maybe HashRef

  handleMethod pk = do
    co <- getRpcContext @RefLogAPI
    debug $ "rpc2.reflogGet:"  <+> pretty (AsBase58 pk)
                               <+> pretty (hashObject @HbSync (RefLogKey @HBS2Basic pk))

    liftIO $ withPeerM (rpcPeerEnv co) $ do
      let sto = rpcStorage co
      liftIO (getRef sto (RefLogKey @HBS2Basic pk)) <&> fmap HashRef

instance (RefLogContext m) => HandleMethod m RpcRefLogFetch where
  type instance Input RpcRefLogFetch = PubKey  'Sign HBS2Basic
  type instance Output RpcRefLogFetch = ()

  handleMethod pk = do
    co <- getRpcContext @RefLogAPI
    debug $ "rpc2.reflogFetch:"  <+> pretty (AsBase58 pk)

    liftIO $ withPeerM (rpcPeerEnv co) $ do
      broadCastMessage (RefLogRequest @L4Proto pk)

instance (RefLogContext m) => HandleMethod m RpcRefLogPost where
  type instance Input RpcRefLogPost = RefLogUpdate L4Proto
  type instance Output RpcRefLogPost = ()

  handleMethod msg = do
    co <- getRpcContext @RefLogAPI
    let pk = view refLogId msg
    debug $ "rpc2.reflogPost:"  <+> pretty (AsBase58 pk)

    liftIO $ withPeerM (rpcPeerEnv co) $ do
      emit @L4Proto RefLogUpdateEvKey (RefLogUpdateEvData (pk, msg))
      doRefLogBroadCast msg


