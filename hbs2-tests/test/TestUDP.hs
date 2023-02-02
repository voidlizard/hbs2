{-# Language TemplateHaskell #-}
module Main where

import HBS2.Prelude.Plated
import HBS2.Net.Proto
import HBS2.Net.Messaging.UDP
import HBS2.Actors.Peer
import HBS2.OrDie

import Control.Monad.Reader
import Data.ByteString.Lazy (ByteString)
import Prettyprinter
import System.IO
import Lens.Micro.Platform

import Codec.Serialise
import Control.Concurrent.Async

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
                => PingPong e
                -> m ()

pingPongHandler = \case

    Ping c -> debug ("Ping" <+> pretty c) >> response (Pong @e c)

    Pong c | c < 100000 -> debug ("Pong" <+> pretty c) >> response (Ping @e (succ c))
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


main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  udp1 <- newMessagingUDP False (Just "127.0.0.1:10001") `orDie` "Can't start listener on 10001"
  udp2 <- newMessagingUDP False (Just "127.0.0.1:10002") `orDie` "Can't start listener on 10002"

  m1 <- async $ runMessagingUDP udp1
  m2 <- async $ runMessagingUDP udp2

  p1 <- async $ runPingPong udp1 do
                  request (getOwnPeer udp2) (Ping @UDP (-10000))
                  runProto @UDP
                    [ makeResponse pingPongHandler
                    ]

  p2 <- async $ runPingPong udp2 do
                  request (getOwnPeer udp1) (Ping @UDP 0)
                  runProto @UDP
                    [ makeResponse pingPongHandler
                    ]

  mapM_ wait [p1,p2,m1,m2]


