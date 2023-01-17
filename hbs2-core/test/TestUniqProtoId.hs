{-# Language TypeFamilyDependencies  #-}
{-# Language UndecidableInstances #-}
module TestUniqProtoId where

import HBS2.Prelude
import HBS2.Prelude.Plated
import HBS2.Clock

import HasProtocol
import FakeMessaging

import Test.Tasty.HUnit

import Data.ByteString.Lazy (ByteString)
import Control.Concurrent.Async
import Codec.Serialise hiding (encode,decode)

import System.IO

import Control.Concurrent.STM.TQueue qualified as Q
-- import Control.Concurrent.STM.TQueue ()
import Control.Concurrent.STM

import Prettyprinter hiding (pipe)

debug :: (MonadIO m) => Doc ann -> m ()
debug p = liftIO $ hPrint stderr p


dump :: MonadIO m => TQueue a -> a -> m ()
dump q x = liftIO $ atomically $ Q.writeTQueue q x

data PingPong e = Ping Int
                | Pong Int
                deriving stock (Eq,Generic,Show,Read)

data PeekPoke e = Peek Int
                | Poke Int
                | Nop
                deriving stock (Eq,Generic,Show,Read)


instance Serialise (PingPong e)

instance Serialise (PeekPoke e)

instance HasProtocol Fake (PingPong Fake) where
  type instance ProtocolId (PingPong Fake) = 1
  type instance Encoded Fake = ByteString
  decode = either (const Nothing) Just . deserialiseOrFail
  encode = serialise

instance HasProtocol Fake (PeekPoke Fake) where
  type instance ProtocolId (PeekPoke Fake)  = 2
  type instance Encoded Fake = ByteString
  decode = either (const Nothing) Just . deserialiseOrFail
  encode = serialise

pingPongHandler :: forall e m  . ( MonadIO m
                                 , Response e (PingPong e) m
                                 , HasProtocol e (PingPong e)
                                 )
                => TQueue (PingPong e)
                -> PingPong e
                -> m ()

pingPongHandler q =
  \case

    Ping c -> dump q (Ping c) >> response (Pong @e c)

    Pong c | c < 100   -> dump q (Pong c) >> response (Ping @e (succ c))
           | otherwise -> dump q (Pong c)

peekPokeHandler :: forall e m  . ( MonadIO m
                                 , Response e (PeekPoke e) m
                                 , HasProtocol e (PeekPoke e)
                                 )
                => TQueue (PeekPoke e)
                -> PeekPoke e
                -> m ()

peekPokeHandler q =
  \case
    Peek c -> dump q (Peek c) >> response (Poke @e (succ c))
    Poke c -> dump q (Poke c) >> response (Nop @e)
    Nop    -> dump q Nop

testUniqProtoId :: IO ()
testUniqProtoId = do

  hSetBuffering stderr LineBuffering

  qpg0 <- Q.newTQueueIO :: IO (TQueue (PingPong Fake))
  qpp0 <- Q.newTQueueIO :: IO (TQueue (PeekPoke Fake))

  qpg1 <- Q.newTQueueIO :: IO (TQueue (PingPong Fake))
  qpp1 <- Q.newTQueueIO :: IO (TQueue (PeekPoke Fake))

  fake <- newFakeP2P True

  let peer0 = FakePeer 0
  let peer1 = FakePeer 1

  env0 <- newEnv peer0 fake
  env1 <- newEnv peer1 fake

  race (pause (0.25 :: Timeout 'Seconds)) $ do

    runEngineM env0 $ do
      request peer1 (Ping @Fake 0)

    runEngineM env1 $ do
      request peer0 (Peek @Fake 0)

    pip1 <- async $
      runPeer env0
        [ makeResponse (pingPongHandler qpg0)
        , makeResponse (peekPokeHandler qpp0)
        ]

    pip2 <- async $
      runPeer env1
        [ makeResponse (pingPongHandler qpg1)
        , makeResponse (peekPokeHandler qpp1)
        ]

    pause (0.10 :: Timeout 'Seconds)

    debug "stopping threads"

    mapM_ cancel [pip1, pip2]

    void $ waitAnyCatchCancel [pip1, pip2]

  ping0 <- atomically $ Q.flushTQueue qpg0
  ping1 <- atomically $ Q.flushTQueue qpg1
  p0    <- atomically $ Q.flushTQueue qpp0
  p1    <- atomically $ Q.flushTQueue qpp1

  assertEqual "ping0" ping0 [ Pong i | i <- [0..100] ]
  assertEqual "ping1" ping1 [ Ping i | i <- [0..100] ]
  assertEqual "p0"    p0 [ Peek 0, Nop ]
  assertEqual "p1"    p1 [ Poke 1 ]

  debug "we're done"

