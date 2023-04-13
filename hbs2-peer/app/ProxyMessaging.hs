{-# Language TemplateHaskell #-}
module ProxyMessaging
  ( ProxyMessaging
  , newProxyMessaging
  , runProxyMessaging
  ) where

import HBS2.Prelude.Plated
import HBS2.Net.Messaging
import HBS2.Clock
import HBS2.Net.Proto.Types
import HBS2.Net.Messaging.UDP
import HBS2.Net.Messaging.TCP

import HBS2.System.Logger.Simple

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.List qualified as L
import Lens.Micro.Platform
import Control.Monad

-- TODO: protocol-encryption-goes-here

data ProxyMessaging  =
  ProxyMessaging
  { _proxyUDP :: MessagingUDP
  , _proxyTCP  :: Maybe MessagingTCP
  , _proxyAnswers :: TQueue (From L4Proto, ByteString)
  }

makeLenses 'ProxyMessaging

newProxyMessaging :: forall m . MonadIO m
                  => MessagingUDP
                  -> Maybe MessagingTCP
                  -> m ProxyMessaging

newProxyMessaging u t = liftIO do
  ProxyMessaging u t
    <$> newTQueueIO

runProxyMessaging :: forall m . MonadIO  m
                  => ProxyMessaging
                  -> m ()

runProxyMessaging env = liftIO do

  let udp = view proxyUDP env
  let answ = view proxyAnswers env
  let udpPeer = getOwnPeer udp

  u <- async $ forever do
          msgs <- receive  udp (To udpPeer)
          atomically $ do
            forM_ msgs $ writeTQueue answ

  t <- async $ maybe1 (view proxyTCP env) none $ \tcp -> do
          forever do
            msgs <- receive  tcp (To $ view tcpOwnPeer tcp)
            atomically $ do
              forM_ msgs $ writeTQueue answ

  liftIO $ mapM_ waitCatch [u,t]

instance Messaging ProxyMessaging L4Proto ByteString where

  sendTo bus t@(To whom) f m = do
    -- sendTo (view proxyUDP bus)  t f m
    -- trace $ "PROXY: SEND" <+> pretty whom
    let udp = view proxyUDP bus
    case view sockType whom of
      UDP -> sendTo udp  t f m
      TCP -> maybe1 (view proxyTCP bus) none $ \tcp -> do
               sendTo tcp t f m

  receive bus _ = liftIO do
    -- trace "PROXY: RECEIVE"
    -- receive (view proxyUDP bus) w
    let answ = view proxyAnswers bus
    atomically $ do
      r <- readTQueue answ
      rs <- flushTQueue answ
      pure (r:rs)

