{-# Language UndecidableInstances #-}
{-# Language RecordWildCards #-}
module HBS2.Net.Messaging.UDP where

import HBS2.Prelude
import HBS2.OrDie
import HBS2.Defaults
import HBS2.Net.IP.Addr
import HBS2.Net.Messaging

import Data.Function
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Cont
import Control.Concurrent.STM.TQueue qualified as Q0
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified  as LBS
import Data.List qualified as L
import Data.Maybe
import Data.Text qualified as Text
import Lens.Micro.Platform
import Network.Socket
import Network.Socket.ByteString
import Network.Multicast

import UnliftIO

-- One address - one peer - one messaging
data MessagingUDP =
  MessagingUDP
  { listenAddr :: SockAddr
  , sink       :: TQueue (From L4Proto, ByteString)
  , inbox      :: TQueue (To L4Proto, ByteString)
  , sock       :: TVar (Maybe Socket)
  , mcast      :: Bool
  }

getOwnPeer :: MessagingUDP -> Peer L4Proto
getOwnPeer mess = PeerL4 UDP (listenAddr mess)

newMessagingUDPMulticast :: MonadUnliftIO m => String -> m (Maybe MessagingUDP)
newMessagingUDPMulticast s = runMaybeT $ do

  (host, port)  <- MaybeT $ pure $ getHostPort (Text.pack s)

  so <- liftIO (try @_ @SomeException $ multicastReceiver host port)
          >>= toMPlus

  liftIO $ setSocketOption so ReuseAddr 1

  a <- liftIO $ getSocketName so

  liftIO $ MessagingUDP a <$> Q0.newTQueueIO
                          <*> Q0.newTQueueIO
                          <*> newTVarIO (Just so)
                          <*> pure True

isUDPSocketClosed :: MonadUnliftIO m => MessagingUDP -> m Bool
isUDPSocketClosed MessagingUDP{..} = readTVarIO sock <&> isNothing

newMessagingUDP :: (MonadUnliftIO m) => Bool -> Maybe String -> m (Maybe MessagingUDP)
newMessagingUDP reuse saddr =
  case saddr of
    Just s -> do
      runMaybeT $ do
        l  <- MaybeT $ liftIO $ parseAddrUDP (Text.pack s) <&> listToMaybe . sorted
        let a = addrAddress l
        so <- liftIO $ socket (addrFamily l) (addrSocketType l) (addrProtocol l)

        liftIO $ withFdSocket so setCloseOnExecIfNeeded

        when reuse $ do
          liftIO $ setSocketOption so ReuseAddr 1

        liftIO $ MessagingUDP a <$> Q0.newTQueueIO
                                <*> Q0.newTQueueIO
                                <*> newTVarIO (Just so)
                                <*> pure False


    Nothing -> do
        so <- liftIO $ socket AF_INET Datagram defaultProtocol
        sa <- liftIO $ getSocketName so

        liftIO $ Just <$> ( MessagingUDP sa <$> Q0.newTQueueIO
                                            <*> Q0.newTQueueIO
                                            <*> newTVarIO (Just so)
                                            <*> pure False
                          )

  where
    sorted = L.sortBy ( compare @Integer `on` proto)
    proto x = case addrAddress x of
      SockAddrInet{}  -> 0
      SockAddrInet6{} -> 1
      SockAddrUnix{}  -> 2


-- FIXME: stopping

runMessagingUDP :: MonadUnliftIO m => MessagingUDP -> m ()
runMessagingUDP MessagingUDP{..} =  void $ flip runContT pure do

  let addr = listenAddr
  so <- liftIO (readTVarIO sock) >>= orThrowUser "UDP socket is not ready"

  void $ ContT $ bracket (pure (Just so)) $ \case
            Just so -> liftIO do
              close so >> atomically (writeTVar sock Nothing)

            Nothing -> pure ()

  unless mcast $ do
    liftIO $ bind so addr

  w <- ContT $ withAsync do
        forever $ liftIO do
          (msg, from) <- recvFrom so defMaxDatagram
          liftIO $ atomically $
            Q0.writeTQueue sink (From (PeerL4 UDP from), LBS.fromStrict msg)

  link w

  waitCatch w >>= either throwIO (const $ pure ())

instance Messaging MessagingUDP L4Proto ByteString where

  sendTo bus (To whom) _ msg = liftIO do
    -- atomically $ Q0.writeTQueue (inbox bus) (To whom, msg)
    mso <- readTVarIO (sock bus)
    for_ mso $ \so -> do
      sendAllTo so (LBS.toStrict msg) (view sockAddr whom)

  receive bus _ = liftIO do
    -- so <- readTVarIO (sock bus)
    -- (msg, from) <- recvFrom so defMaxDatagram
    -- pure [(From (PeerUDP from), LBS.fromStrict msg)]

    liftIO $ atomically $ Q0.readTQueue (sink bus) <&> L.singleton

