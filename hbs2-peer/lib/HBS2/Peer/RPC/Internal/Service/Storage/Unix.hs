{-# OPTIONS_GHC -fno-warn-orphans #-}
module HBS2.Peer.RPC.Internal.Service.Storage.Unix where

import HBS2.Prelude.Plated
import HBS2.Actors.Peer
import HBS2.Net.Proto
import HBS2.Net.Proto.Service
import HBS2.Net.Messaging.Unix

import HBS2.Peer.RPC.API.Storage
import HBS2.Peer.RPC.Internal.Storage

import HBS2.System.Logger.Simple


import Data.Config.Suckless.Syntax
import Data.Config.Suckless.KeyValue

import Data.ByteString.Lazy (ByteString)
import Data.Text qualified as Text
import Control.Monad.Reader
import Codec.Serialise
import UnliftIO


instance HasProtocol UNIX  (ServiceProto StorageAPI UNIX) where
  type instance ProtocolId (ServiceProto StorageAPI UNIX) = 0xDA2374610001
  type instance Encoded UNIX = ByteString
  decode = either (const Nothing) Just . deserialiseOrFail
  encode = serialise


sodef :: FilePath
sodef = "/tmp/hbs2-rpc2.storage.socket"

getSocketName :: HasConf m => m FilePath
getSocketName = do
  syn <- getConf

  let soname = lastDef sodef [ Text.unpack n
                             | ListVal @C (Key "rpc2.storage" [SymbolVal "unix", LitStrVal n]) <- syn
                             ]
  pure soname

instance MonadUnliftIO m => (HasDeferred UNIX (ServiceProto StorageAPI UNIX) m) where
  deferred _ m = void $ async m

instance Monad m => HasFabriq UNIX (ReaderT (AnyStorage, MessagingUnix) m) where
  getFabriq = asks (Fabriq . snd)

instance Monad m => HasOwnPeer UNIX (ReaderT (AnyStorage, MessagingUnix) m) where
  ownPeer = asks ( msgUnixSelf . snd )

-- FIXME: fix-this-ugly-shit
instance Monad m
  => HasRpcContext AnyStorage (ResponseM UNIX (ReaderT (AnyStorage, MessagingUnix) m)) where
  getRpcContext = lift $ asks fst


runService :: ( HasConf m
              , MonadUnliftIO m
              , HasDeferred UNIX (ServiceProto StorageAPI UNIX) m
              )
           => AnyStorage -> m ()
runService sto = do

  soname <- getSocketName

  notice $ "RPC2 Storage Service started" <+> pretty soname

  server <- newMessagingUnixOpts [MUFork] True 1.0 soname
  m1 <- async $ runMessagingUnix server
  link m1

  flip runReaderT (sto, server) do
    runProto @UNIX
      [ makeResponse (makeServer @StorageAPI)
      ]



