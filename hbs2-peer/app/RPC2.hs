{-# Language TemplateHaskell #-}
module RPC2 where

import HBS2.Prelude
import HBS2.Clock
import HBS2.Net.Proto.Service
import HBS2.Net.Messaging.Unix
import HBS2.Net.Proto.Types

import HBS2.System.Logger.Simple

import Data.Config.Suckless.KeyValue()

import RPC2.Service.Unix as RPC2
import RPC2.API
import PeerConfig

import Data.Maybe
import Control.Applicative
import Lens.Micro.Platform
import Control.Monad.Reader
import UnliftIO

data RPCOpt =
  RPCOpt
  { _rpcOptConf :: Maybe FilePath
  , _rpcOptAddr :: Maybe String
  }

makeLenses 'RPCOpt


withRPC2 :: forall e m . (e ~ UNIX, HasProtocol e (ServiceProto RPC2 e), MonadUnliftIO m)
         => RPCOpt
         -> ( ServiceCaller RPC2 e -> m () )
         -> m ()

withRPC2 o action = do
  conf  <- peerConfigRead (view rpcOptConf o)
  soConf <- runReaderT RPC2.getSocketName conf
  let soOpt = view rpcOptAddr o
  let soname = fromJust $ soOpt <|> Just soConf

  debug $ "withRPC2" <+> pretty soname

  client1 <- newMessagingUnix False 1.0 soname

  m1 <- async $ runMessagingUnix client1
  -- link m1

  caller <- makeServiceCaller @RPC2 @UNIX (fromString soname)
  p2 <- liftIO $ async $ runReaderT (runServiceClient @RPC2 @UNIX caller) client1

  action caller

  pause @'Seconds 0.05
  cancel p2

  void $ waitAnyCatchCancel [m1, p2]

