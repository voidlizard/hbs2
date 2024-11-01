{-# Language TemplateHaskell #-}
module Main where

import HBS2.Prelude.Plated
import HBS2.Net.Proto
import HBS2.Net.Messaging.UDP
import HBS2.Actors.Peer
import HBS2.Misc.PrettyStuff
import HBS2.OrDie

import Control.Monad.Reader
import Control.Monad.Trans.Cont
import Control.Concurrent.STM (retry)
import Data.ByteString.Lazy (ByteString)
import Prettyprinter
import System.IO (hPrint)
import Lens.Micro.Platform

import Codec.Serialise
-- import Control.Concurrent.Async

import UnliftIO
import UnliftIO.Async
import UnliftIO.STM

type UDP = L4Proto

debug :: (MonadIO m) => Doc ann -> m ()
debug p = liftIO $ hPrint stderr p

data PingPong e = Ping Int
                | Pong Int
                deriving stock (Eq,Generic,Show,Read)


instance Serialise (PingPong e)


instance HasProtocol UDP (PingPong UDP) where
  type instance ProtocolId (PingPong UDP) = 1
  type instance Encoded UDP = ByteString
  decode = either (const Nothing) Just . deserialiseOrFail
  encode = serialise

pingPongHandler :: forall e m  . ( MonadIO m
                                 , Response e (PingPong e) m
                                 , HasProtocol e (PingPong e)
                                 )
                => TVar Int
                -> Int
                -> PingPong e
                -> m ()

pingPongHandler tv n = \case

    Ping c -> debug ("Ping" <+> pretty c) >> response (Pong @e c)

    Pong c | c < n -> do
      debug ("Pong" <+> pretty c)
      liftIO $ atomically $ writeTVar tv c
      response (Ping @e (succ c))

           | otherwise -> pure ()

data PPEnv =
  PPEnv
  { _ppSelf :: Peer UDP
  , _ppFab  :: Fabriq UDP
  }

makeLenses 'PPEnv

newtype PingPongM m a = PingPongM { fromPingPong :: ReaderT PPEnv m a }
                        deriving newtype ( Functor
                                         , Applicative
                                         , Monad
                                         , MonadIO
                                         , MonadReader PPEnv
                                         , MonadTrans
                                         )

runPingPong :: (MonadIO m, PeerMessaging UDP) => MessagingUDP -> PingPongM m a -> m a
runPingPong udp m = runReaderT (fromPingPong m) (PPEnv (getOwnPeer udp) (Fabriq udp))

instance Monad m => HasFabriq UDP (PingPongM m) where
  getFabriq = asks (view ppFab)

instance Monad m => HasOwnPeer UDP (PingPongM m) where
  ownPeer = asks (view ppSelf)

instance HasTimeLimits UDP (PingPong UDP) IO where
  tryLockForPeriod _ _ = pure True

main :: IO ()
main = do
  liftIO $ hSetBuffering stdout LineBuffering
  liftIO $ hSetBuffering stderr LineBuffering

  let tries = 1000

  replicateM_ 10 do

    udp1 <- newMessagingUDP False (Just "127.0.0.1:10001") `orDie` "Can't start listener on 10001"
    udp2 <- newMessagingUDP False (Just "127.0.0.1:10002") `orDie` "Can't start listener on 10002"

    void $ flip runContT pure do

      m1 <- ContT $ withAsync $ runMessagingUDP udp1
      m2 <- ContT $ withAsync $ runMessagingUDP udp2

      tping1 <- newTVarIO 0
      tping2 <- newTVarIO 0

      pause @'Seconds 0.01

      p1 <- ContT $ withAsync $ runPingPong udp1 do
                      request (getOwnPeer udp2) (Ping @UDP 0)
                      runProto @UDP
                        [ makeResponse (pingPongHandler tping1 tries)
                        ]

      p2 <- ContT $ withAsync $ runPingPong udp2 do
                      -- request (getOwnPeer udp1) (Ping @UDP 0)
                      runProto @UDP
                        [ makeResponse (pingPongHandler tping2 tries)
                        ]

      r <- liftIO $ race (pause @'Seconds 2) do
        atomically do
          r1 <- readTVar tping1
          r2 <- readTVar tping2
          if (max r1 r2) >= (tries-1) then pure () else retry

      let done = either (const "fail") (const "okay") r

      v1 <- readTVarIO tping1
      v2 <- readTVarIO tping2

      liftIO $ hPrint stdout $ pretty "finished" <+> pretty done <+> pretty (max v1 v2)

      mapM_ cancel [m1,m2,p1,p2]

      c1 <- liftIO $ isUDPSocketClosed udp1
      c2 <- liftIO $ isUDPSocketClosed udp2

      liftIO $ hPrint stdout $ pretty "socket1 closed" <+> pretty c1
      liftIO $ hPrint stdout $ pretty "socket2 closed" <+> pretty c2


