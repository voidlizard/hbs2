{-# Language TemplateHaskell #-}
module CLI.Common where

import HBS2.Net.Messaging.Unix
import HBS2.Net.Proto.Service

import PeerConfig

import RPC2.Client.Unix
import RPC2.Service.Unix (getSocketName)
import RPC2.API

import Control.Applicative
import Control.Monad.Reader
import Data.Maybe
import Lens.Micro.Platform
import UnliftIO

data RPCOpt =
  RPCOpt
  { _rpcOptConf :: Maybe FilePath
  , _rpcOptAddr :: Maybe String
  }

makeLenses 'RPCOpt

withMyRPC :: forall api m . (MonadUnliftIO m, api ~ RPC2)
          => RPCOpt
          -> (ServiceCaller api UNIX -> m ())
          -> m ()

withMyRPC o m = do
  conf  <- peerConfigRead (view rpcOptConf o)
  soConf <- runReaderT getSocketName conf
  let soOpt = view rpcOptAddr o
  let soname = fromJust $ soOpt <|> Just soConf
  withRPC2 @RPC2  @UNIX soname m


