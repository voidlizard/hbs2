{-# OPTIONS_GHC -fno-warn-orphans #-}
module RPC2.Service.Unix
  ( module RPC2.Service.Unix
  , module HBS2.Net.Proto.Service
  ) where

import HBS2.Prelude.Plated
import HBS2.Actors.Peer
import HBS2.Net.Proto
import HBS2.Net.Proto.Service
import HBS2.Net.Messaging.Unix

import HBS2.System.Logger.Simple

import RPC2.API

import Data.Config.Suckless.Syntax
import Data.Config.Suckless.KeyValue

import Data.Text qualified as Text
import Control.Monad.Reader
import UnliftIO
import Data.ByteString.Lazy (ByteString)
import Codec.Serialise

instance HasProtocol UNIX  (ServiceProto RPC2 UNIX) where
  type instance ProtocolId (ServiceProto RPC2 UNIX) = 0xDA2374610000
  type instance Encoded UNIX = ByteString
  decode = either (const Nothing) Just . deserialiseOrFail
  encode = serialise

instance Monad m => HasRpcContext RPC2Context (ReaderT RPC2Context m) where
  getRpcContext = ask

-- FIXME: fix-this-ugly-shit
instance (Monad m, HasRpcContext RPC2Context m) => HasRpcContext RPC2Context (ResponseM UNIX (ReaderT MessagingUnix m)) where
  getRpcContext = lift $ lift getRpcContext

instance MonadUnliftIO m => (HasDeferred UNIX (ServiceProto RPC2 UNIX) (ReaderT RPC2Context m)) where
  deferred _ m = void $ async m

instance (MonadUnliftIO m) =>
  HasDeferred  UNIX (ServiceProto RPC2 UNIX) (ResponseM UNIX m) where
  deferred _ m = do
    -- FIXME: this-might-be-ok-for-rpc
    --   никаких конвейров и прочих модных
    --   штук, которые реализованы в PeerM
    --   можно прикрутить какой-то глоальный
    --   пул процессов?
    --   О! Конвейр, буде он понадобится,
    --   можно запихнуть прямо в MessagingUnix
    void $ async m

instance Monad m => HasConf (ReaderT RPC2Context m) where
  getConf = asks rpcConfig

sodef :: FilePath
sodef = "/tmp/hbs2-rpc2.socket"

getSocketName :: HasConf m => m FilePath
getSocketName = do
  syn <- getConf

  let soname = lastDef sodef [ Text.unpack n
                             | ListVal @C (Key "rpc2" [SymbolVal "unix", LitStrVal n]) <- syn
                             ]
  pure soname

runService :: ( HasConf m
              , MonadUnliftIO m
              , HasRpcContext RPC2Context m
              , HasDeferred UNIX (ServiceProto RPC2 UNIX) m
              ) => m ()
runService = do

  soname <- getSocketName

  notice $ "RPC2 Service started" <+> pretty soname

  server <- newMessagingUnixOpts [MUFork] True 1.0 soname
  m1 <- async $ runMessagingUnix server
  link m1

  flip runReaderT server do
    runProto @UNIX
      [ makeResponse (makeServer @RPC2)
      ]


