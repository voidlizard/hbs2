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
import Control.Concurrent.Async
import Lens.Micro.Platform
import Codec.Serialise
import System.Environment

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

pingPongHandler :: forall e m proto  . ( MonadIO m
                                       , Response e (PingPong e) m
                                       , HasProtocol e (PingPong e)
                                       , HasOwnPeer e m
                                       , HasDeferred proto e m
                                       , Pretty (Peer e)
                                       , proto ~ PingPong e
                                       )
                => Int
                -> PingPong e
                -> m ()

pingPongHandler _ req = do

  that <- thatPeer @proto
  own <- ownPeer @e

  case req of

    Ping c -> do
      testCmd own ("RECV PING <<<" <+> pretty c) that

      deferred @proto do
        pause @'Seconds 1
        testCmd own ("SEND PONG >>>" <+> pretty (succ c)) that
        response (Pong @e (succ c))

    Pong c -> do
      testCmd own ("RECV PONG <<<" <+> pretty c) that

      deferred @proto  do
        pause @'Seconds 1
        testCmd own ("SEND PING >>>" <+> pretty (succ c)) that
        response (Ping @e c)

data PPEnv =
  PPEnv
  { _ppSelf :: Peer L4Proto
  , _ppFab  :: Fabriq L4Proto
  }

makeLenses 'PPEnv

newtype PingPongM e m a = PingPongM { fromPingPong :: ReaderT PPEnv m a }
                          deriving newtype ( Functor
                                           , Applicative
                                           , Monad
                                           , MonadIO
                                           , MonadReader PPEnv
                                           , MonadTrans
                                           )

runPingPong :: (MonadIO m, PeerMessaging L4Proto) => Peer L4Proto -> Fabriq L4Proto -> PingPongM L4Proto m a -> m a
runPingPong peer tcp m = runReaderT (fromPingPong m) (PPEnv peer tcp)

instance Monad m => HasFabriq L4Proto (PingPongM L4Proto m) where
  getFabriq = asks (view ppFab)

instance Monad m => HasOwnPeer L4Proto (PingPongM L4Proto m) where
  ownPeer = asks (view ppSelf)

instance HasTimeLimits L4Proto (PingPong L4Proto) IO where
  tryLockForPeriod _ _ = pure True


instance HasDeferred (PingPong L4Proto) L4Proto  (ResponseM L4Proto (PingPongM L4Proto IO)) where
  deferred m = do
    self <- lift $ asks (view ppSelf)
    bus  <- lift $ asks (view ppFab)
    who <- thatPeer @(PingPong L4Proto)
    void $ liftIO $ async $ runPingPong self bus (runResponseM who m)

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

  args <- getArgs >>= \case
           [self,remote] -> pure (self,remote)
           _             -> error "bad args"

  let self = fromString (fst args) -- "tcp://127.0.0.1:3001"
  remote <- fromPeerAddr $ fromString (snd args) :: IO (Peer L4Proto)

  tcp <- newMessagingTCP self

  peer  <- async do
              runMessagingTCP tcp

  -- setLoggingOff @TRACE

  pp1 <- async $ runPingPong (view tcpOwnPeer tcp) (Fabriq tcp) do
                  testCmd (view tcpOwnPeer tcp) ("!!! SEND PING" <+> pretty 1) remote
                  request remote (Ping @L4Proto 1)
                  runProto @L4Proto
                    [ makeResponse (pingPongHandler 100)
                    ]

  void $ waitAnyCatchCancel [pp1,peer]

  setLoggingOff @DEBUG
  setLoggingOff @INFO
  setLoggingOff @ERROR
  setLoggingOff @WARN
  setLoggingOff @NOTICE
  setLoggingOff @TRACE


