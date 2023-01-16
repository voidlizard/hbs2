{-# Language TypeFamilyDependencies  #-}
{-# Language FunctionalDependencies #-}
{-# Language AllowAmbiguousTypes  #-}
-- {-# Language #-}
-- {-# Language QuantifiedConstraints #-}
module TestUniqProtoId where

import HBS2.Clock

import HasProtocol

import Data.Kind
import GHC.TypeLits
import Data.Proxy
import Data.Map qualified as Map
import Data.Map (Map)
import Control.Monad.Reader
import Data.ByteString (ByteString)
import Lens.Micro.Platform

import Data.Foldable
import Data.List qualified as List
import Data.Cache qualified as Cache
import Data.Cache (Cache)
import Control.Concurrent.STM.TChan as Chan
import Control.Concurrent.STM
import Data.Hashable
import Data.Maybe
import Safe

import Prettyprinter

newtype From a = From (Peer a)

newtype To a = To (Peer a)

class HasPeer proto => Messaging bus proto msg  | bus -> proto, bus -> msg where

  sendTo  :: MonadIO m => bus -> To proto -> From proto -> msg -> m ()
  receive :: MonadIO m => bus -> To proto -> m [(From proto, msg)]


data AnyMessage = AnyMessage Integer String

data EngineEnv p = forall bus . (Messaging bus p AnyMessage)  =>
  EngineEnv
  { peer   :: Maybe (Peer p)
  , self   :: Peer p
  , bus    :: bus
  }

-- makeLenses 'EngineEnv

data FakeP2P proto msg =
  FakeP2P
  {
    blocking :: Bool
  , fakeP2p  :: Cache (Peer proto) (TChan (From proto,msg))
  }

newFakeP2P :: Bool -> IO (FakeP2P peer msg)
newFakeP2P block = FakeP2P block <$> Cache.newCache Nothing

instance ( (HasPeer proto, Hashable (Peer proto))
         ) => Messaging (FakeP2P proto msg) proto msg where

  sendTo bus (To whom) who msg = liftIO do
    chan <- Cache.fetchWithCache (fakeP2p bus) whom $ const newTChanIO
    atomically $ Chan.writeTChan chan (who, msg)

  receive bus (To me) = liftIO do
    readChan =<< Cache.fetchWithCache (fakeP2p bus) me (const newTChanIO)

    where
      readChan | blocking bus = atomically . (List.singleton <$>) . Chan.readTChan
               | otherwise    = atomically . (maybeToList <$>) . Chan.tryReadTChan



data AnyProtocol e m = forall p a . ( HasProtocol p a
                                    , KnownNat (ProtocolId a)
                                    , Response p a m
                                    , e ~ Encoded p
                                    ) =>
  AnyProtocol
  { getProtoId  :: Integer
  , protoDecode :: Encoded p -> Maybe a
  , protoEncode :: a -> Encoded p
  , handle      :: a -> m ()
  }



class Response e p (m :: Type -> Type) where
  response :: p -> m ()

class Request e p (m :: Type -> Type) where
  request :: Peer e -> p -> m ()

makeResponse :: forall a p t m . ( MonadIO m
                                 , Response a p (t m)
                                 , HasProtocol a p
                                 )
            => (p -> t m ()) -> AnyProtocol (Encoded a) (t m)

makeResponse h = AnyProtocol { getProtoId  = natVal (Proxy @(ProtocolId p))
                             , protoDecode = decode @a
                             , protoEncode = encode @a
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


instance (MonadIO m, HasProtocol e p, Encoded e ~ String, Show (Peer e)) => Request e p (EngineM e m) where
  request p msg = do
    let proto = protoId @e @p (Proxy @p)
    ask >>= \case
      EngineEnv { self = s, bus = b} -> do
        liftIO $ sendTo b (To p) (From s) (AnyMessage proto (encode msg))

instance (MonadIO m, HasProtocol e p, Encoded e ~ String, Show (Peer e)) => Response e p (EngineM e m) where
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


data PingPong  = Ping Int
               | Pong Int
               deriving stock (Show,Read)


data Fake

instance HasPeer Fake where
  newtype instance Peer Fake = FakePeer Int
                               deriving newtype (Hashable)
                               deriving stock (Eq,Show)

instance HasProtocol Fake PingPong where
  type instance ProtocolId PingPong = 1
  type instance Encoded Fake = String
  decode = readMay
  encode = show

data PeekPoke  = Peek Int
               | Poke Int
               | Nop
               deriving stock (Show,Read)


instance HasProtocol Fake PeekPoke where
  type instance ProtocolId PeekPoke = 2
  type instance Encoded Fake = String
  decode = readMay
  encode = show



pingPongHandler :: forall a m  . (MonadIO m, Response a PingPong  m, HasProtocol a PingPong) => PingPong -> m ()
pingPongHandler =
  \case
    Ping c -> liftIO (print $ "effect: PING" <+> pretty c) >> response @a @PingPong (Pong c)
    Pong c -> liftIO (print $ "effect: PONG" <+> pretty c) >> response @a @PingPong (Ping (succ c))


peekPokeHandler :: forall a m  . (MonadIO m, Response a PeekPoke m, HasProtocol a PeekPoke) => PeekPoke -> m ()
peekPokeHandler =
  \case
    Peek c -> liftIO (print $ "effect: Peek" <+> pretty c) >> response @a @PeekPoke(Poke c)
    Poke c -> liftIO (print $ "effect: Poke" <+> pretty c) >> response @a @PeekPoke Nop
    Nop    -> liftIO (print $ pretty "effect: Nop") >> response @a @PeekPoke (Peek 1)

testUniqiProtoId :: IO ()
testUniqiProtoId = do

  fake <- newFakeP2P True

  let env = EngineEnv @Fake Nothing (FakePeer 0) fake

  let resp = [ (protoId @Fake (Proxy @PingPong), makeResponse pingPongHandler)
             , (protoId @Fake (Proxy @PeekPoke), makeResponse peekPokeHandler)
             ]

  let decoders = Map.fromList resp :: Map Integer (AnyProtocol (Encoded Fake) (EngineM Fake IO))

  forever $ do

    runEngineM env $ do

      request (FakePeer 0) (Ping 0)
      request (FakePeer 0) (Peek 1)

      messages <- receive fake (To (FakePeer 0))

      for_ messages $ \(From pip, AnyMessage n msg) -> do

        local (\e -> e { peer = Just pip  } ) $ do

          -- FIXME: dispatcher!
          case Map.lookup n decoders of
            Just (AnyProtocol {protoDecode = decoder, handle = h}) -> maybe (pure ()) h (decoder msg)
            Nothing -> pure ()

      pause ( 0.25 :: Timeout 'Seconds)

