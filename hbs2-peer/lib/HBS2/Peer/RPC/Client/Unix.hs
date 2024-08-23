module HBS2.Peer.RPC.Client.Unix
  ( module HBS2.Peer.RPC.Client.Unix
  , module HBS2.Net.Proto.Service
  , module HBS2.Net.Messaging.Unix
  ) where

import HBS2.Prelude.Plated

import HBS2.Clock
import HBS2.Net.Messaging.Unix
import HBS2.Net.Proto.Service

-- FIXME: to-remove-code
import HBS2.Peer.RPC.API.Storage()

import HBS2.System.Logger.Simple

import Data.Kind
import Control.Monad.Reader
import UnliftIO
import Control.Monad.Trans.Cont

withRPC2 :: forall (api :: [Type]) e m r . ( e ~ UNIX
                             , HasProtocol e (ServiceProto api e)
                             , MonadUnliftIO m
                             )
         => FilePath
         -> ( ServiceCaller api e -> m r )
         -> m r

withRPC2 soname action = do

  flip runContT pure do

    trace $ "withRPC2" <+> pretty soname

    client1 <- newMessagingUnix False 1.0 soname

    m1 <- ContT $ withAsync (runMessagingUnix client1)
    -- link m1

    caller <- makeServiceCaller @api @UNIX (fromString soname)
    p2 <- ContT $ withAsync (liftIO $ runReaderT (runServiceClient @api @e caller) client1)

    r <- lift $ action caller

    pause @'Seconds 0.05
    cancel p2

    void $ waitAnyCatchCancel [m1, p2]

    pure r

