module RPC2.RefChan where


import HBS2.Prelude.Plated

import HBS2.Actors.Peer
import HBS2.Hash
import HBS2.Base58
import HBS2.Data.Detect
import HBS2.Data.Types.Refs (HashRef(..))
import HBS2.Net.Proto.Definition()
import HBS2.Net.Proto.Service
import HBS2.Net.Proto.Types
import HBS2.Data.Types.SignedBox
import HBS2.Net.Proto.RefChan
import HBS2.Storage

import HBS2.System.Logger.Simple
import PeerTypes
import RPC2.Types

import Data.ByteString (ByteString)
import Data.Functor
import Lens.Micro.Platform
import Streaming.Prelude qualified as S
import Data.ByteString.Lazy qualified as LBS
import Codec.Serialise

-- NOTE: refchan-head-endpoints
data RpcRefChanHeadGet
data RpcRefChanHeadFetch
data RpcRefChanHeadPost

-- NOTE: refchan-endpoints
data RpcRefChanFetch
data RpcRefChanGet
data RpcRefChanPropose

data RpcRefChanNotify

instance (MonadIO m, HasRpcContext RPC2Context m) => HandleMethod m RpcRefChanHeadGet where
  type instance Input RpcRefChanHeadGet = PubKey 'Sign HBS2Basic
  type instance Output RpcRefChanHeadGet = Maybe HashRef

  handleMethod puk = do
    co <- getRpcContext @RPC2Context
    let penv = rpcPeerEnv co
    debug $ "rpc2.refchanHeadGet:"  <+> pretty (AsBase58 puk)
    liftIO $ withPeerM penv $ do
      sto <- getStorage
      liftIO $ getRef sto (RefChanHeadKey @HBS2Basic puk) <&> fmap HashRef

instance (MonadIO m, HasRpcContext RPC2Context m) => HandleMethod m RpcRefChanHeadFetch where
  type instance Input RpcRefChanHeadFetch = PubKey 'Sign HBS2Basic
  type instance Output RpcRefChanHeadFetch = ()

  handleMethod puk = do
    debug $ "rpc2.refchanHeadFetch:"  <+> pretty (AsBase58 puk)
    penv <- rpcPeerEnv <$> getRpcContext @RPC2Context
    void $ liftIO $ withPeerM penv $ do
      broadCastMessage (RefChanGetHead @L4Proto puk)

instance (MonadIO m, HasRpcContext RPC2Context m) => HandleMethod m RpcRefChanFetch where
  type instance Input RpcRefChanFetch = PubKey 'Sign HBS2Basic
  type instance Output RpcRefChanFetch = ()

  handleMethod puk = do
    debug $ "rpc2.refchanFetch:"  <+> pretty (AsBase58 puk)
    penv <- rpcPeerEnv <$> getRpcContext @RPC2Context
    void $ liftIO $ withPeerM penv $ do
      gossip (RefChanRequest @L4Proto puk)

instance (MonadIO m, HasRpcContext RPC2Context m) => HandleMethod m RpcRefChanGet where
  type instance Input RpcRefChanGet = PubKey 'Sign HBS2Basic
  type instance Output RpcRefChanGet = Maybe HashRef

  handleMethod puk = do
    co <- getRpcContext @RPC2Context
    let penv = rpcPeerEnv co
    debug $ "rpc2.refchanGet:"  <+> pretty (AsBase58 puk)
    liftIO $ withPeerM penv $ do
      sto <- getStorage
      liftIO $ getRef sto (RefChanLogKey @HBS2Basic puk) <&> fmap HashRef


instance (MonadIO m, HasRpcContext RPC2Context m) => HandleMethod m RpcRefChanPropose where
  type instance Input RpcRefChanPropose = (PubKey 'Sign HBS2Basic, SignedBox ByteString L4Proto)
  type instance Output RpcRefChanPropose = ()

  handleMethod (puk, box) = do
    co <- getRpcContext @RPC2Context
    debug $ "rpc2.refChanNotifyAction" <+>  pretty (AsBase58 puk)
    liftIO $ rpcDoRefChanPropose co (puk, box)


instance (MonadIO m, HasRpcContext RPC2Context m) => HandleMethod m RpcRefChanNotify where
  type instance Input RpcRefChanNotify = (PubKey 'Sign HBS2Basic, SignedBox ByteString L4Proto)
  type instance Output RpcRefChanNotify = ()

  handleMethod (puk, box) = do
    co <- getRpcContext @RPC2Context
    debug $ "rpc2.refChanNotifyAction" <+>  pretty (AsBase58 puk)
    liftIO $ rpcDoRefChanNotify co (puk, box)

instance (MonadIO m, HasRpcContext RPC2Context m) => HandleMethod m RpcRefChanHeadPost where
  type instance Input RpcRefChanHeadPost = HashRef
  type instance Output RpcRefChanHeadPost = ()

  handleMethod href = do
    co <- getRpcContext @RPC2Context
    liftIO $ rpcDoRefChanHeadPost co href

-- instance (MonadIO m, HasRpcContext RPC2Context m) => HandleMethod m RpcRefLogFetch where
--   type instance Input RpcRefLogFetch = PubKey  'Sign HBS2Basic
--   type instance Output RpcRefLogFetch = ()

--   handleMethod pk = do
--     co <- getRpcContext @RPC2Context
--     debug $ "rpc2.reflogFetch:"  <+> pretty (AsBase58 pk)

--     liftIO $ withPeerM (rpcPeerEnv co) $ do
--       broadCastMessage (RefLogRequest @L4Proto pk)

-- instance (MonadIO m, HasRpcContext RPC2Context m) => HandleMethod m RpcRefLogPost where
--   type instance Input RpcRefLogPost = RefLogUpdate L4Proto
--   type instance Output RpcRefLogPost = ()

--   handleMethod msg = do
--     co <- getRpcContext @RPC2Context
--     let pk = view refLogId msg
--     debug $ "rpc2.reflogPost:"  <+> pretty (AsBase58 pk)

--     liftIO $ withPeerM (rpcPeerEnv co) $ do
--       emit @L4Proto RefLogUpdateEvKey (RefLogUpdateEvData (pk, msg))
--       doRefLogBroadCast msg


