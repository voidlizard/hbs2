{-# Language UndecidableInstances #-}
module HBS2.Net.Messaging.UDP where

import HBS2.Clock
import HBS2.Defaults
import HBS2.Net.IP.Addr
import HBS2.Net.Messaging
import HBS2.Net.Proto
import HBS2.Prelude.Plated

import HBS2.System.Logger.Simple

import Data.Function
import Control.Exception
import Control.Monad.Trans.Maybe
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue qualified as Q0
import Control.Monad
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified  as LBS
import Data.Functor
import Data.List qualified as L
import Data.Maybe
-- import Data.Text (Text)
import Data.Text qualified as Text
import Lens.Micro.Platform
import Network.Socket
import Network.Socket.ByteString
import Network.Multicast

import Control.Monad.Trans.Resource


-- One address - one peer - one messaging
data MessagingUDP =
  MessagingUDP
  { listenAddr :: SockAddr
  , sink       :: TQueue (From L4Proto, ByteString)
  , inbox      :: TQueue (To L4Proto, ByteString)
  , sock       :: TVar Socket
  , mcast      :: Bool
  }


getOwnPeer :: MessagingUDP -> Peer L4Proto
getOwnPeer mess = PeerL4 UDP (listenAddr mess)

newMessagingUDPMulticast :: MonadResource m => String -> m (Maybe MessagingUDP)
newMessagingUDPMulticast s = runMaybeT $ do

  (host, port)  <- MaybeT $ pure $ getHostPort (Text.pack s)

  so <- liftIO $ multicastReceiver host port

  _ <- register $ close so

  liftIO $ setSocketOption so ReuseAddr 1

  a <- liftIO $ getSocketName so

  liftIO $ MessagingUDP a <$> Q0.newTQueueIO
                          <*> Q0.newTQueueIO
                          <*> newTVarIO so
                          <*> pure True


newMessagingUDP :: (MonadIO m, MonadResource m) => Bool -> Maybe String -> m (Maybe MessagingUDP)
newMessagingUDP reuse saddr =
  case saddr of
    Just s -> do

      runMaybeT $ do
        l  <- MaybeT $ liftIO $ parseAddrUDP (Text.pack s) <&> listToMaybe . sorted
        let a = addrAddress l
        so <- liftIO $ socket (addrFamily l) (addrSocketType l) (addrProtocol l)

        _ <- register $ close so

        when reuse $ do
          liftIO $ setSocketOption so ReuseAddr 1

        liftIO $ MessagingUDP a <$> Q0.newTQueueIO
                                <*> Q0.newTQueueIO
                                <*> newTVarIO so
                                <*> pure False


    Nothing -> do
        so <- liftIO $ socket AF_INET Datagram defaultProtocol

        _ <- register $ close so

        sa <- liftIO $ getSocketName so

        liftIO $ Just <$> ( MessagingUDP sa <$> Q0.newTQueueIO
                                            <*> Q0.newTQueueIO
                                            <*> newTVarIO so
                                            <*> pure False
                          )

  where
    sorted = L.sortBy ( compare `on` proto)
    proto x = case addrAddress x of
      SockAddrInet{}  -> 0
      SockAddrInet6{} -> 1
      SockAddrUnix{}  -> 2


udpWorker :: MessagingUDP -> TVar Socket -> IO ()
udpWorker env tso = do

  so <- readTVarIO tso

  rcvLoop <- async $ forever $ do
    -- so <- readTVarIO tso
    -- pause ( 10 :: Timeout 'Seconds )
    (msg, from) <- recvFrom so defMaxDatagram
  -- liftIO $ print $ "recv:" <+> pretty (BS.length msg)
  -- FIXME: ASAP-check-addr-type
    liftIO $ atomically $ Q0.writeTQueue (sink env) (From (PeerL4 UDP from), LBS.fromStrict msg)

  sndLoop <- async $ forever $ do
    pause ( 10 :: Timeout 'Seconds )
    -- (To whom, msg) <- atomically $ Q0.readTQueue (inbox env)
    -- print "YAY!"
    -- sendAllTo so (LBS.toStrict msg) (view sockAddr whom)

    -- (msg, from) <- recvFrom so defMaxDatagram
  -- liftIO $ print $ "recv:" <+> pretty (BS.length msg)
    -- atomically $ Q.writeTBQueue (sink env) (From (PeerUDP from), LBS.fromStrict msg)

  void $ waitAnyCatchCancel [rcvLoop,sndLoop]

-- FIXME: stopping

runMessagingUDP :: MonadIO m => MessagingUDP -> m ()
runMessagingUDP udpMess = liftIO $ do
  let addr = listenAddr udpMess
  so <- readTVarIO (sock udpMess)

  unless (mcast udpMess) $ do
    bind so addr

  w <- async $ udpWorker udpMess (sock udpMess)
  waitCatch w >>= either throwIO (const $ pure ())

instance Messaging MessagingUDP L4Proto ByteString where

  sendTo bus (To whom) _ msg = liftIO do
    -- atomically $ Q0.writeTQueue (inbox bus) (To whom, msg)
    so <- readTVarIO (sock bus)
    sendAllTo so (LBS.toStrict msg) (view sockAddr whom)

  receive bus _ = liftIO do
    -- so <- readTVarIO (sock bus)
    -- (msg, from) <- recvFrom so defMaxDatagram
    -- pure [(From (PeerUDP from), LBS.fromStrict msg)]

    liftIO $ atomically $ Q0.readTQueue (sink bus) <&> L.singleton

