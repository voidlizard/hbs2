module HBS2.Peer.RPC.Client.Unix where

import HBS2.Prelude.Plated

import HBS2.Clock
import HBS2.Net.Messaging.Unix
import HBS2.Net.Proto
import HBS2.Net.Proto.Service

import HBS2.Peer.RPC.Internal.Service.Storage.Unix()

import HBS2.System.Logger.Simple

import Data.Kind
import Control.Monad.Reader
import UnliftIO

withRPC2 :: forall (api :: [Type]) e m . ( e ~ UNIX
                             , HasProtocol e (ServiceProto api e)
                             , MonadUnliftIO m
                             -- FIXME: remove-this-debug-shit
                             , MonadUnliftIO m
                             )
         => FilePath
         -> ( ServiceCaller api e -> m () )
         -> m ()

withRPC2 soname action = do

  debug $ "withRPC2" <+> pretty soname

  client1 <- newMessagingUnix False 1.0 soname

  m1 <- async $ runMessagingUnix client1
  -- link m1

  caller <- makeServiceCaller @api @UNIX (fromString soname)
  p2 <- liftIO $ async $ runReaderT (runServiceClient @api @e caller) client1

  action caller

  pause @'Seconds 0.05
  cancel p2

  void $ waitAnyCatchCancel [m1, p2]

