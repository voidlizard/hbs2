{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language UndecidableInstances #-}
module RPC2.RefChan
  ( module RPC2.RefChan
  , module HBS2.Peer.RPC.Internal.Types
  ) where

import HBS2.Prelude.Plated

import HBS2.Actors.Peer
import HBS2.Base58
import HBS2.Data.Types.Refs (HashRef(..))
import HBS2.Net.Proto.Definition()
import HBS2.Net.Proto.Service
import HBS2.Data.Types.SignedBox
import HBS2.Net.Proto.RefChan
import HBS2.Net.Messaging.Unix
import HBS2.Storage

import HBS2.Peer.RPC.API.RefChan
import HBS2.Peer.RPC.Internal.Types

import HBS2.System.Logger.Simple
import PeerTypes

import Data.ByteString qualified as BS
import Data.Functor
import Control.Monad.Reader

type RefChanContext m = (MonadIO m, HasRpcContext RefChanAPI RPC2Context m)

instance (Monad m)
  => HasRpcContext RefChanAPI RPC2Context (ResponseM UNIX (ReaderT RPC2Context m)) where
  -- type instance RpcContext RefChanAPI = RPC2Context
  getRpcContext = lift ask

instance RefChanContext m => HandleMethod m RpcRefChanHeadGet where
  type instance Input RpcRefChanHeadGet = PubKey 'Sign HBS2Basic
  type instance Output RpcRefChanHeadGet = Maybe HashRef

  handleMethod puk = do
    co <- getRpcContext @RefChanAPI
    let penv = rpcPeerEnv co
    debug $ "rpc2.refchanHeadGet:"  <+> pretty (AsBase58 puk)
    liftIO $ withPeerM penv $ do
      sto <- getStorage
      liftIO $ getRef sto (RefChanHeadKey @HBS2Basic puk) <&> fmap HashRef

instance (RefChanContext m) => HandleMethod m RpcRefChanHeadFetch where
  type instance Input RpcRefChanHeadFetch = PubKey 'Sign HBS2Basic
  type instance Output RpcRefChanHeadFetch = ()

  handleMethod puk = do
    debug $ "rpc2.refchanHeadFetch:"  <+> pretty (AsBase58 puk)
    penv <- rpcPeerEnv <$> getRpcContext @RefChanAPI
    void $ liftIO $ withPeerM penv $ do
      broadCastMessage (RefChanGetHead @L4Proto puk)

instance RefChanContext m => HandleMethod m RpcRefChanFetch where
  type instance Input RpcRefChanFetch = PubKey 'Sign HBS2Basic
  type instance Output RpcRefChanFetch = ()

  handleMethod puk = do
    debug $ "rpc2.refchanFetch:"  <+> pretty (AsBase58 puk)
    penv <- rpcPeerEnv <$> getRpcContext @RefChanAPI
    void $ liftIO $ withPeerM penv $ do
      gossip (RefChanRequest @L4Proto puk)

instance RefChanContext m => HandleMethod m RpcRefChanGet where
  type instance Input RpcRefChanGet = PubKey 'Sign HBS2Basic
  type instance Output RpcRefChanGet = Maybe HashRef

  handleMethod puk = do
    co <- getRpcContext @RefChanAPI
    let penv = rpcPeerEnv co
    debug $ "rpc2.refchanGet:"  <+> pretty (AsBase58 puk)
    liftIO $ withPeerM penv $ do
      sto <- getStorage
      liftIO $ getRef sto (RefChanLogKey @HBS2Basic puk) <&> fmap HashRef

instance RefChanContext m => HandleMethod m RpcRefChanPropose where
  type instance Input RpcRefChanPropose = (PubKey 'Sign HBS2Basic, SignedBox BS.ByteString L4Proto)
  type instance Output RpcRefChanPropose = ()

  handleMethod (puk, box) = do
    co <- getRpcContext @RefChanAPI
    debug $ "rpc2.refChanNotifyAction" <+>  pretty (AsBase58 puk)
    liftIO $ rpcDoRefChanPropose co (puk, box)


instance RefChanContext m => HandleMethod m RpcRefChanNotify where
  type instance Input RpcRefChanNotify = (PubKey 'Sign HBS2Basic, SignedBox BS.ByteString L4Proto)
  type instance Output RpcRefChanNotify = ()

  handleMethod (puk, box) = do
    co <- getRpcContext @RefChanAPI
    debug $ "rpc2.refChanNotifyAction" <+>  pretty (AsBase58 puk)
    liftIO $ rpcDoRefChanNotify co (puk, box)

instance RefChanContext m => HandleMethod m RpcRefChanHeadPost where
  type instance Input RpcRefChanHeadPost = HashRef
  type instance Output RpcRefChanHeadPost = ()

  handleMethod href = do
    co <- getRpcContext @RefChanAPI
    liftIO $ rpcDoRefChanHeadPost co href

