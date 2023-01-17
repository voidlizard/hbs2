{-# Language TypeFamilyDependencies  #-}
{-# Language UndecidableInstances #-}
module TestUniqProtoId where

import HasProtocol
import FakeMessaging

import GHC.TypeLits
import Data.Proxy
import Data.Map qualified as Map
import Control.Monad.Reader

import Control.Concurrent.Async
import Data.Foldable
import Safe

import Prettyprinter hiding (pipe)


data AnyMessage e = AnyMessage Integer (Encoded e)

data EngineEnv e = forall bus . (Messaging bus e (AnyMessage e))  =>
  EngineEnv
  { peer   :: Maybe (Peer e)
  , self   :: Peer e
  , bus    :: bus
  }

data AnyProtocol e m = forall p  . ( HasProtocol e p
                                   , KnownNat (ProtocolId p)
                                   , Response e p m
                                   ) =>
  AnyProtocol
  { myProtoId   :: Integer
  , protoDecode :: Encoded e -> Maybe p
  , protoEncode :: p -> Encoded e
  , handle      :: p -> m ()
  }

makeResponse :: forall e p  m . ( MonadIO m
                                , Response e p m
                                , HasProtocol e p
                                )
            => (p ->  m ()) -> AnyProtocol e m

makeResponse h = AnyProtocol { myProtoId  = natVal (Proxy @(ProtocolId p))
                             , protoDecode = decode
                             , protoEncode = encode
                             , handle = h
                             }


newtype EngineM e m a = EngineM { fromEngine :: ReaderT (EngineEnv e) m a }
                        deriving ( Functor
                                 , Applicative
                                 , Monad
                                 , MonadTrans
                                 , MonadIO
                                 , MonadReader (EngineEnv e)
                                )

runEngineM :: EngineEnv e -> EngineM e m a -> m a
runEngineM e f = runReaderT (fromEngine f) e


instance (MonadIO m, HasProtocol e p) => Request e p (EngineM e m) where
  request p msg = do
    let proto = protoId @e @p (Proxy @p)
    ask >>= \case
      EngineEnv { self = s, bus = b} -> do
        liftIO $ sendTo b (To p) (From s) (AnyMessage proto (encode msg))

instance (MonadIO m, HasProtocol e p) => Response e p (EngineM e m) where
  response resp = do
    env <- ask
    let proto = protoId @e @p (Proxy @p)
    case env of
      (EngineEnv { peer = Just p
                 , bus = b
                 , self = s
                 } ) -> do
                  liftIO $ sendTo b (To p) (From s) (AnyMessage proto (encode resp))
      _ -> pure ()




newEnv :: forall e bus m . (Monad m, Messaging bus e (AnyMessage e)) => Peer e -> bus -> m (EngineEnv e)
newEnv p pipe = pure $ EngineEnv Nothing p pipe


runPeer :: MonadIO m => EngineEnv e -> [AnyProtocol e (EngineM e m)] -> m a
runPeer env@(EngineEnv {self = me, bus = pipe}) hh = do

  let resp = [ (pid, a) | a@AnyProtocol { myProtoId = pid } <- hh ]

  let disp = Map.fromList resp

  runEngineM env $ do

    forever $ do
      messages <- receive pipe (To me)

      for_ messages $ \(From pip, AnyMessage n msg) -> do

        local (\e -> e { peer = Just pip  } ) $ do

          case Map.lookup n disp of
            Just (AnyProtocol {protoDecode = decoder, handle = h}) -> maybe (pure ()) h (decoder msg)
            Nothing -> pure ()

data PingPong e = Ping Int
                | Pong Int
                deriving stock (Show,Read)

data PeekPoke e = Peek Int
                | Poke Int
                | Nop
                deriving stock (Show,Read)


instance HasProtocol Fake (PingPong Fake) where
  type instance ProtocolId (PingPong Fake) = 1
  type instance Encoded Fake = String
  decode = readMay
  encode = show

instance HasProtocol Fake (PeekPoke Fake) where
  type instance ProtocolId (PeekPoke Fake)  = 2
  type instance Encoded Fake = String
  decode = readMay
  encode = show


pingPongHandler :: forall e m  . (MonadIO m, Response e (PingPong e) m, HasProtocol e (PingPong e)) => PingPong e -> m ()
pingPongHandler  =
  \case
    Ping c -> liftIO (print $ "effect: PING" <+> pretty c) >> response (Pong @e c)
    Pong c -> liftIO (print $ "effect: PONG" <+> pretty c) >> response (Ping @e (succ c))

peekPokeHandler :: forall e m  . (MonadIO m, Response e (PeekPoke e) m, HasProtocol e (PeekPoke e)) => PeekPoke e -> m ()
peekPokeHandler =
  \case
    Peek c -> liftIO (print $ "effect: Peek" <+> pretty c) >> response (Poke @e c)
    Poke c -> liftIO (print $ "effect: Poke" <+> pretty c) >> response (Nop @e)
    Nop    -> liftIO (print $ pretty "effect: Nop") >> response (Peek @e 1)


testUniqiProtoId :: IO ()
testUniqiProtoId = do


  fake <- newFakeP2P True

  let peer0 = FakePeer 0
  let peer1 = FakePeer 1

  env0 <- newEnv peer0 fake
  env1 <- newEnv peer1 fake

  runEngineM env0 $ do
    request peer1 (Ping @Fake 0)

  runEngineM env1 $ do
    request peer0 (Peek @Fake 0)

  pip1 <- async $
    runPeer env0
      [ makeResponse pingPongHandler
      , makeResponse peekPokeHandler
      ]

  pip2 <- async $
    runPeer env1
      [ makeResponse pingPongHandler
      , makeResponse peekPokeHandler
      ]

  void $ waitAnyCatchCancel [pip1, pip2]


