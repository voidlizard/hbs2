{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language UndecidableInstances #-}
module RPC2.RefChan
  ( module RPC2.RefChan
  , module HBS2.Peer.RPC.Internal.Types
  ) where

import HBS2.Peer.Prelude

import HBS2.Actors.Peer
import HBS2.Base58
import HBS2.Data.Types.Refs (HashRef(..))
import HBS2.Net.Proto.Service
import HBS2.Peer.Proto.RefChan
import HBS2.Net.Messaging.Unix
import HBS2.Storage

import HBS2.Peer.RPC.API.RefChan
import HBS2.Peer.RPC.Internal.Types

import PeerTypes

import Control.Monad.Reader

type RefChanContext m = (MonadIO m, HasRpcContext RefChanAPI RPC2Context m)

instance (Monad m)
  => HasRpcContext RefChanAPI RPC2Context (ResponseM UNIX (ReaderT RPC2Context m)) where
  -- type instance RpcContext RefChanAPI = RPC2Context
  getRpcContext = lift ask

instance RefChanContext m => HandleMethod m RpcRefChanHeadGet where

  handleMethod puk = do
    co <- getRpcContext @RefChanAPI
    let penv = rpcPeerEnv co
    debug $ "rpc.refchanHeadGet:"  <+> pretty (AsBase58 puk)
    liftIO $ withPeerM penv $ do
      sto <- getStorage
      liftIO $ getRef sto (RefChanHeadKey @'HBS2Basic puk) <&> fmap HashRef

instance (RefChanContext m) => HandleMethod m RpcRefChanHeadFetch where

  handleMethod puk = do
    debug $ "rpc.refchanHeadFetch:"  <+> pretty (AsBase58 puk)
    penv <- rpcPeerEnv <$> getRpcContext @RefChanAPI
    void $ liftIO $ withPeerM penv $ do
      broadCastMessage (RefChanGetHead @L4Proto puk)

instance RefChanContext m => HandleMethod m RpcRefChanFetch where

  handleMethod puk = do
    debug $ "rpc.refchanFetch:"  <+> pretty (AsBase58 puk)
    penv <- rpcPeerEnv <$> getRpcContext @RefChanAPI
    void $ liftIO $ withPeerM penv $ do
      gossip (RefChanRequest @L4Proto puk)

instance RefChanContext m => HandleMethod m RpcRefChanGet where

  handleMethod puk = do
    co <- getRpcContext @RefChanAPI
    let penv = rpcPeerEnv co
    debug $ "rpc.refchanGet:"  <+> pretty (AsBase58 puk)
    liftIO $ withPeerM penv $ do
      sto <- getStorage
      liftIO $ getRef sto (RefChanLogKey @'HBS2Basic puk) <&> fmap HashRef

instance RefChanContext m => HandleMethod m RpcRefChanPropose where

  handleMethod (puk, box) = do
    co <- getRpcContext @RefChanAPI
    debug $ "rpc.refChanNotifyAction" <+>  pretty (AsBase58 puk)
    liftIO $ rpcDoRefChanPropose co (puk, box)


instance RefChanContext m => HandleMethod m RpcRefChanNotify where

  handleMethod (puk, box) = do
    co <- getRpcContext @RefChanAPI
    debug $ "rpc.refChanNotifyAction" <+>  pretty (AsBase58 puk)
    liftIO $ rpcDoRefChanNotify co (puk, box)

instance RefChanContext m => HandleMethod m RpcRefChanHeadPost where

  handleMethod href = do
    co <- getRpcContext @RefChanAPI
    liftIO $ rpcDoRefChanHeadPost co href

