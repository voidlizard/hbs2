{-# Language TemplateHaskell #-}
module HBS2.Net.Messaging.UDP where

import HBS2.Clock
import HBS2.Defaults
import HBS2.Net.IP.Addr
import HBS2.Net.Messaging
import HBS2.Net.Proto
import HBS2.Prelude.Plated

import Data.Foldable
import Data.Function
import Control.Exception
import Control.Monad.Trans.Maybe
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TBQueue qualified as Q
import Control.Concurrent.STM.TQueue qualified as Q0
import Control.Monad
import Data.ByteString.Lazy (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified  as LBS
import Data.Functor
import Data.Hashable
import Data.List qualified as L
import Data.Maybe
-- import Data.Text (Text)
import Data.Text qualified as Text
import Lens.Micro.Platform
import Network.Socket
import Network.Socket.ByteString
import Network.Multicast
import Prettyprinter

data UDP

-- FIXME: #ASAP change SockAddr to PeerAddr !!!
instance HasPeer UDP where
  newtype instance Peer UDP =
    PeerUDP
    { _sockAddr :: SockAddr
    }
    deriving stock (Eq,Ord,Show,Generic)


instance AddrPriority (Peer UDP) where
  addrPriority (PeerUDP sa) = addrPriority sa

instance Hashable (Peer UDP) where
  hashWithSalt salt p = case _sockAddr p of
    SockAddrInet  pn h     -> hashWithSalt salt (4, fromIntegral pn, h)
    SockAddrInet6 pn _ h _ -> hashWithSalt salt (6, fromIntegral pn, h)
    SockAddrUnix s         -> hashWithSalt salt ("unix", s)

instance Pretty (Peer UDP) where
  pretty p = pretty (_sockAddr p)

makeLenses 'PeerUDP


instance MonadIO m => IsPeerAddr UDP m where
  type instance PeerAddr UDP = IPAddrPort UDP
  toPeerAddr p = pure $ fromString $ show $ pretty p

  fromPeerAddr iap = do
    ai <- liftIO $ parseAddr $ fromString (show (pretty iap))
    pure $ PeerUDP $ addrAddress (head ai) -- FIXME: errors?!

-- One address - one peer - one messaging
data MessagingUDP =
  MessagingUDP
  { listenAddr :: SockAddr
  , sink       :: TBQueue (From UDP, ByteString)
  , inbox      :: TQueue (To UDP, ByteString)
  , sock       :: TVar Socket
  , mcast      :: Bool
  }


getOwnPeer :: MessagingUDP -> Peer UDP
getOwnPeer mess = PeerUDP (listenAddr mess)

newMessagingUDPMulticast :: MonadIO m => String -> m (Maybe MessagingUDP)
newMessagingUDPMulticast s = runMaybeT $ do

  (host, port)  <- MaybeT $ pure $ getHostPort (Text.pack s)

  so <- liftIO $ multicastReceiver host port

  liftIO $ setSocketOption so ReuseAddr 1

  a <- liftIO $ getSocketName so

  liftIO $ MessagingUDP a <$> Q.newTBQueueIO defMessageQueueSize
                          <*> Q0.newTQueueIO
                          <*> newTVarIO so
                          <*> pure True

newMessagingUDP :: MonadIO m => Bool -> Maybe String -> m (Maybe MessagingUDP)
newMessagingUDP reuse saddr =
  case saddr of
    Just s -> do

      runMaybeT $ do
        l  <- MaybeT $ liftIO $ parseAddr (Text.pack s) <&> listToMaybe . sorted
        let a = addrAddress l
        so <- liftIO $ socket (addrFamily l) (addrSocketType l) (addrProtocol l)

        when reuse $ do
          liftIO $ setSocketOption so ReuseAddr 1

        liftIO $ MessagingUDP a <$> Q.newTBQueueIO defMessageQueueSize
                                <*> Q0.newTQueueIO
                                <*> newTVarIO so
                                <*> pure False


    Nothing -> do
        so <- liftIO $ socket AF_INET Datagram defaultProtocol
        sa <- liftIO $ getSocketName so

        liftIO $ Just <$> ( MessagingUDP sa <$> Q.newTBQueueIO defMessageQueueSize
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
    pause ( 10 :: Timeout 'Seconds )
    -- (msg, from) <- recvFrom so defMaxDatagram
  -- liftIO $ print $ "recv:" <+> pretty (BS.length msg)
    -- atomically $ Q.writeTBQueue (sink env) (From (PeerUDP from), LBS.fromStrict msg)

  sndLoop <- async $ forever $ do
    pause ( 10 :: Timeout 'Seconds )
    -- (To whom, msg) <- atomically $ Q0.readTQueue (inbox env)
    -- print "YAY!"
    -- sendAllTo so (LBS.toStrict msg) (view sockAddr whom)

    -- (msg, from) <- recvFrom so defMaxDatagram
  -- liftIO $ print $ "recv:" <+> pretty (BS.length msg)
    -- atomically $ Q.writeTBQueue (sink env) (From (PeerUDP from), LBS.fromStrict msg)

  mapM_ wait [rcvLoop,sndLoop]

-- FIXME: stopping

runMessagingUDP :: MonadIO m => MessagingUDP -> m ()
runMessagingUDP udpMess = liftIO $ do
  let addr = listenAddr udpMess
  so <- readTVarIO (sock udpMess)

  unless (mcast udpMess) $ do
    bind so addr

  w <- async $ udpWorker udpMess (sock udpMess)
  waitCatch w >>= either throwIO (const $ pure ())

instance Messaging MessagingUDP UDP ByteString where

  sendTo bus (To whom) _ msg = liftIO do
    -- atomically $ Q0.writeTQueue (inbox bus) (To whom, msg)
    so <- readTVarIO (sock bus)
    sendAllTo so (LBS.toStrict msg) (view sockAddr whom)

  receive bus _ = liftIO do
    so <- readTVarIO (sock bus)
    (msg, from) <- recvFrom so defMaxDatagram
    pure [(From (PeerUDP from), LBS.fromStrict msg)]

    -- liftIO $ atomically
    -- $ Q.readTBQueue (sink bus) <&> L.singleton

