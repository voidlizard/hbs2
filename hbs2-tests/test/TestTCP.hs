{-# Language TemplateHaskell #-}
module Main where

import HBS2.Prelude.Plated
import HBS2.Net.Proto.Types
import HBS2.Clock
import HBS2.Net.Messaging.TCP
import HBS2.Actors.Peer

import HBS2.System.Logger.Simple

import System.IO
import Control.Monad.Reader
import Control.Monad.Writer hiding (listen)

import Test.Tasty.HUnit

import Data.ByteString.Lazy (ByteString)
import Lens.Micro.Platform
import Codec.Serialise

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

testPeerAddr :: IO ()
testPeerAddr = do

  let p1 = fromStringMay @(PeerAddr L4Proto) "192.168.1.2:5551"
  let p2 = fromStringMay @(PeerAddr L4Proto) "udp://192.168.1.2:5551"
  let p3 = fromStringMay @(PeerAddr L4Proto) "tcp://192.168.1.2:5551"

  debug $ "parsed udp addr:" <+> pretty p1
  debug $ "parsed udp addr:" <+> pretty p2
  debug $ "parsed tcp addr:" <+> pretty p3

  assertEqual "udp address check 1" p2 p2
  assertBool "tcp and udp are different" (p1 /= p3)

  case p1 of
    (Just (L4Address UDP _)) -> pure ()
    _ -> assertFailure "p1 is not UDP"

  case p2 of
    (Just (L4Address UDP _)) -> pure ()
    _ -> assertFailure "p1 is not UDP"

  case p3 of
    (Just (L4Address TCP _)) -> pure ()
    _ -> assertFailure "p3 is not TCP"

  peerUDP0 <- fromPeerAddr @L4Proto "192.168.1.1:5551"
  peerUDP1 <- fromPeerAddr @L4Proto "udp://192.168.1.1:5551"
  peerTCP <- fromPeerAddr @L4Proto "tcp://192.168.1.1:3001"

  debug $ "peerUDP0" <+> pretty peerUDP0
  debug $ "peerUDP1" <+> pretty peerUDP1
  debug $ "peerTCP" <+> pretty peerTCP

  pure ()


data PingPong e = Ping Int
                | Pong Int
                deriving stock (Eq,Generic,Show,Read)


instance Serialise (PingPong e)

instance HasProtocol L4Proto (PingPong L4Proto) where
  type instance ProtocolId (PingPong L4Proto) = 1
  type instance Encoded L4Proto = ByteString
  decode = either (const Nothing) Just . deserialiseOrFail
  encode = serialise

testCmd :: forall a ann b m . ( Pretty a
                               , Pretty b
                               , MonadIO m
                               )
         => a -> Doc ann -> b -> m ()

testCmd p1 s p2 = do
  notice $ brackets (pretty p1)
           <+> s
           <+> parens (pretty p2)

pingPongHandler :: forall e m  . ( MonadIO m
                                 , Response e (PingPong e) m
                                 , HasProtocol e (PingPong e)
                                 , HasOwnPeer e m
                                 , Pretty (Peer e)
                                 )
                => Int
                -> PingPong e
                -> m ()

pingPongHandler n req = do

  that <- thatPeer (Proxy @(PingPong e))
  own <- ownPeer @e

  case req of

    Ping c -> do
      testCmd own (">>> RECV PING" <+> pretty c) that

      when ( c <= n ) do
        testCmd own ("<<< SEND PONG" <+> pretty (succ c)) that
        response (Pong @e (succ c))

    Pong c -> do
      testCmd own (">>> RECV PONG" <+> pretty c) that
      testCmd own (">>> SEND PING BACK" <+> pretty (succ c)) that

      response (Ping @e c)


data PPEnv =
  PPEnv
  { _ppSelf :: Peer L4Proto
  , _ppFab  :: Fabriq L4Proto
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

runPingPong :: (MonadIO m, PeerMessaging L4Proto) => MessagingTCP-> PingPongM m a -> m a
runPingPong tcp m = runReaderT (fromPingPong m) (PPEnv (view tcpOwnPeer tcp) (Fabriq tcp))

instance Monad m => HasFabriq L4Proto (PingPongM m) where
  getFabriq = asks (view ppFab)

instance Monad m => HasOwnPeer L4Proto (PingPongM m) where
  ownPeer = asks (view ppSelf)

instance HasTimeLimits L4Proto (PingPong L4Proto) IO where
  tryLockForPeriod _ _ = pure True


main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  setLogging @DEBUG  debugPrefix
  setLogging @INFO   defLog
  setLogging @ERROR  errorPrefix
  setLogging @WARN   warnPrefix
  setLogging @NOTICE noticePrefix
  setLogging @TRACE  tracePrefix

  testPeerAddr

  let pa1 = fromString "tcp://127.0.0.1:3001"
  let pa2 = fromString "tcp://127.0.0.1:3002"

  let pa3 = fromString "tcp://127.0.0.1:3003"
  pip3 <- fromPeerAddr pa3

  -- let pa3 = fromSockAddr @'TCP $ fromString "tcp://127.0.0.1:3003"

  env1 <- newMessagingTCP pa1
  env2 <- newMessagingTCP pa2

  p1 <- fromPeerAddr pa1
  p2 <- fromPeerAddr pa2

  peer1  <- async do
              runMessagingTCP env1

  peer2  <- async do
              runMessagingTCP env2

  pause @'Seconds 1

  let runPeers m = snd <$> runWriterT m
  let run m = do
        x <- liftIO $ async m
        tell [x]

  pause @'Seconds 1

  setLoggingOff @TRACE

  pp1 <- async $ runPingPong env1 do
                  testCmd (view tcpOwnPeer env1) ("!!! SEND PING" <+> pretty 1) (view tcpOwnPeer env2)
                  request (view tcpOwnPeer env2) (Ping @L4Proto 1)
                  runProto @L4Proto
                    [ makeResponse (pingPongHandler 3)
                    ]

  pp2 <- async $ runPingPong env2 do
                    -- request (view tcpOwnPeer env1) (Ping @L4Proto 1)
                    runProto @L4Proto
                        [ makeResponse (pingPongHandler 3)
                        ]

  pause @'Seconds 1

  testCmd "!!!" "reverse test" "!!!"

  runPingPong env2 do
    testCmd (view tcpOwnPeer env2) ("!!! SEND PING" <+> pretty 1) (view tcpOwnPeer env1)
    request (view tcpOwnPeer env1) (Ping @L4Proto 1)
    pure ()

  forever do
    runPingPong env2 do
      testCmd (view tcpOwnPeer env1) ("!!! SEND PING" <+> pretty 1) pip3
      request pip3 (Ping @L4Proto 1)
      pure ()
    pause @'Seconds 2

  -- waiter <- async $ pause @'Seconds 60

  mapM_ wait [pp1,pp2]
  -- void $ waitAnyCatchCancel $ [peer1,peer2] <> conn <> [pp1,pp2]

  setLoggingOff @DEBUG
  setLoggingOff @INFO
  setLoggingOff @ERROR
  setLoggingOff @WARN
  setLoggingOff @NOTICE
  setLoggingOff @TRACE


