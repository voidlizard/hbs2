{-# Language TemplateHaskell #-}
module Main where

import HBS2.Prelude.Plated
import HBS2.Clock
import HBS2.Net.Proto
import HBS2.Net.Messaging.UDP
import HBS2.Net.Messaging.TCP
import HBS2.Actors.Peer
import HBS2.OrDie

import HBS2.System.Logger.Simple

import HBS2.Net.Messaging.Encrypted.ByPass

import Control.Monad.Reader
import Data.ByteString.Lazy (ByteString)
import Prettyprinter
import System.IO
import Lens.Micro.Platform
import Crypto.Saltine.Core.Sign

import Data.Int
import Codec.Serialise
-- import Control.Concurrent.Async
import System.Random
import Text.InterpolatedString.Perl6 (qc)

import Control.Monad.Trans.Resource
import UnliftIO.Async

type UDP = L4Proto

data PingPong e = Ping Int
                | Pong Int
                | Poke
                deriving stock (Eq,Generic,Show,Read)


instance Serialise (PingPong e)

instance HasProtocol UDP (PingPong UDP) where
  type instance ProtocolId (PingPong UDP) = 1307114574
  type instance Encoded UDP = ByteString
  decode = either (const Nothing) Just . deserialiseOrFail
  encode = serialise

pingPongHandler :: forall e m  . ( MonadIO m
                                 , Response e (PingPong e) m
                                 , HasProtocol e (PingPong e)
                                 , HasOwnPeer e m
                                 , Pretty (Peer e)
                                 )
                => Int
                -> PingPong e
                -> m ()

pingPongHandler n = \case

    Poke -> pure ()

    Ping c -> do
      self <- ownPeer @e
      debug ("Ping" <+> pretty self <+> pretty c) >> response (Pong @e c)

    Pong c | c < n -> do
      self <- ownPeer @e
      debug ("Pong" <+> pretty self <+> pretty c) >> response (Ping @e (succ c))

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
                                         , MonadUnliftIO
                                         , MonadReader PPEnv
                                         , MonadTrans
                                         )

runPingPong :: (MonadIO m) => Peer UDP -> Fabriq UDP -> PingPongM m a -> m a
runPingPong pip udp m = runReaderT (fromPingPong m) (PPEnv pip udp)

instance Monad m => HasFabriq UDP (PingPongM m) where
  getFabriq = asks (view ppFab)

instance Monad m => HasOwnPeer UDP (PingPongM m) where
  ownPeer = asks (view ppSelf)

instance HasTimeLimits UDP (PingPong UDP) IO where
  tryLockForPeriod _ _ = pure True

tracePrefix :: SetLoggerEntry
tracePrefix  = logPrefix "[trace] "

debugPrefix :: SetLoggerEntry
debugPrefix  = logPrefix "[debug] "

errorPrefix :: SetLoggerEntry
errorPrefix  = logPrefix "[error] "

warnPrefix :: SetLoggerEntry
warnPrefix   = logPrefix "[warn] "

noticePrefix :: SetLoggerEntry
noticePrefix = logPrefix "[RT] "


testUDP  :: IO ()
testUDP = runResourceT do

  let o = byPassDef

  udp1 <- newMessagingUDP False (Just "127.0.0.1:10001") `orDie` "Can't start listener on 10001"
  udp2 <- newMessagingUDP False (Just "127.0.0.1:10002") `orDie` "Can't start listener on 10002"

  Keypair s1 p1 <- liftIO newKeypair
  pass1 <- newByPassMessaging @UDP o udp1 (getOwnPeer udp1) p1 s1

  Keypair s2 p2 <- liftIO newKeypair
  pass2 <- newByPassMessaging @UDP o udp2 (getOwnPeer udp2) p2 s2

  m1 <- async $ runMessagingUDP udp1
  m2 <- async $ runMessagingUDP udp2

  let own1 = getOwnPeer udp1
  let own2 = getOwnPeer udp2

  peer1 <- async $ runPingPong own1 (Fabriq pass1) do
                  proto <- async $ runProto @UDP
                    [ makeResponse (pingPongHandler 10)
                    ]
                  link proto
                  forever (pause @'Seconds 10)


  peer2 <- async $ runPingPong own2 (Fabriq pass2) do
                  proto <- async $ runProto @UDP
                    [ makeResponse (pingPongHandler 10)
                    ]
                  link proto
                  request (getOwnPeer udp1) (Ping @UDP 0)
                  forever (pause @'Seconds 10)

  mapM_ wait [peer1,peer2,m1,m2]


testTCP :: IO ()
testTCP = runResourceT do

  let o = byPassDef

  pn1 <- liftIO $ randomIO @Int8 <&> ((11000 +) . fromIntegral)
  pn2 <- liftIO $ randomIO @Int8 <&> ((11000 +). fromIntegral)

  let addr1 = fromString [qc|tcp://127.0.0.1:{pn1}|]
  let addr2 = fromString [qc|tcp://127.0.0.1:{pn2}|]

  debug $ "ADDR1" <+> pretty addr1
  debug $ "ADDR2" <+> pretty addr2

  me1 <- newMessagingTCP addr1
  me2 <- newMessagingTCP addr2

  m1 <- async $ runMessagingTCP me1
  m2 <- async $ runMessagingTCP me2

  let peer1 = view tcpOwnPeer me1
  let peer2 = view tcpOwnPeer me2


  Keypair s1 p1 <- liftIO newKeypair
  pass1 <- newByPassMessaging o me1 peer1 p1 s1

  Keypair s2 p2 <- liftIO newKeypair
  pass2 <- newByPassMessaging o me2 peer2 p2 s2

  peerThread1 <- async $ runPingPong peer1 (Fabriq pass1) do
                  proto <- async $ runProto @L4Proto
                    [ makeResponse (pingPongHandler 10)
                    ]
                  link proto

                  request peer2 (Poke @L4Proto)

                  pause @'Seconds 1

                  request peer2 (Ping @L4Proto 0)

                  forever (pause @Seconds 10)


  peerThread2 <- async $ runPingPong peer2 (Fabriq pass2) do
                  proto <- async $ runProto @L4Proto
                    [ makeResponse (pingPongHandler 10)
                    ]
                  link proto
                  forever (pause @Seconds 10)

  waiter <- async $ do
              pause @'Seconds 3

  void $ waitAnyCatchCancel [peerThread1,peerThread2,m1,m2,waiter]

  stat1 <- getStat pass1
  stat2 <- getStat pass2

  debug "testUDP done"

  liftIO $ print $ "peer1 stats" <> line <> indent 4 (pretty stat1) <> line
  liftIO $ print $ "peer2 stats" <> line <> indent 4 (pretty stat2) <> line


main :: IO ()
main = do

  liftIO $ hSetBuffering stdout LineBuffering
  liftIO $ hSetBuffering stderr LineBuffering

  setLogging @DEBUG  debugPrefix
  setLogging @INFO   defLog
  setLogging @ERROR  errorPrefix
  setLogging @WARN   warnPrefix
  setLogging @NOTICE noticePrefix
  setLogging @TRACE  tracePrefix

  testTCP


