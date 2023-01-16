{-# Language TypeFamilyDependencies  #-}
{-# Language FunctionalDependencies #-}
{-# Language AllowAmbiguousTypes  #-}
{-# Language TemplateHaskell #-}
-- {-# Language QuantifiedConstraints #-}
module TestUniqProtoId where

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

newtype From a = From (Peer a)

newtype To a = To (Peer a)

class HasPeer proto => Messaging bus proto msg  | bus -> proto, bus -> msg where

  sendTo  :: MonadIO m => bus -> To proto -> From proto -> msg -> m ()
  receive :: MonadIO m => bus -> To proto -> m [(From proto, msg)]


data AnyMessage = AnyMessage Integer String

data EngineEnv = forall p bus . (Messaging bus p AnyMessage)  =>
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


data PingPong  = Ping Int
               | Pong Int
               deriving stock (Show,Read)

type family Encoding a :: Type

data Fake


instance HasPeer Fake where
  newtype instance Peer Fake = FakePeer Int
                               deriving newtype (Hashable)
                               deriving stock (Eq)

instance HasProtocol Fake PingPong where
  type instance ProtocolId PingPong = 1
  type instance Encoded Fake = String
  decode = readMay
  encode = show

class Response e p (m :: Type -> Type) where
  response :: p -> m ()

makeResponse :: forall a p t m . ( MonadIO m
                                 , Response a p (t m)
                                 , HasProtocol a p
                                 , KnownNat (ProtocolId p)
                                 )
            => (p -> t m ()) -> AnyProtocol (Encoded a) (t m)

makeResponse h = AnyProtocol { getProtoId  = natVal (Proxy @(ProtocolId p))
                             , protoDecode = decode @a
                             , protoEncode = encode @a
                             , handle = h
                             }


newtype EngineM m a = EngineM { fromEngine :: ReaderT EngineEnv m a }
                      deriving ( Functor
                               , Applicative
                               , Monad
                               , MonadTrans
                               , MonadIO
                               , MonadReader EngineEnv
                               )

runEngineM :: EngineEnv -> EngineM m a -> m a
runEngineM e f = runReaderT (fromEngine f) e

instance (MonadIO m, HasProtocol e p, Encoded e ~ String) => Response e p (EngineM m) where
  response resp = do
    env <- ask
    case env of
      (EngineEnv { peer = Just p
                 , bus = b
                 , self = s
                 } ) -> do
                  liftIO $ sendTo b (To p) (From s) (AnyMessage 1 (encode resp))
      _ -> pure ()

pingPongHandler :: forall a m  . (MonadIO m, Response a PingPong  m, HasProtocol a PingPong) => PingPong -> m ()
pingPongHandler =
  \case
    Ping c -> liftIO (print "effect: PING") >> response @a @PingPong (Pong c)
    Pong _ -> liftIO (print "effect: PONG")

testUniqiProtoId :: IO ()
testUniqiProtoId = do

  fake <- newFakeP2P True

  let env = EngineEnv @Fake Nothing (FakePeer 0) fake

  sendTo fake (To (FakePeer 0)) (From (FakePeer 0)) (AnyMessage 1 (encode (Ping 0)))

  let pingpong = makeResponse pingPongHandler

  let resp = [ (1, pingpong) ]

  let decoders = Map.fromList resp :: Map Integer (AnyProtocol (Encoded Fake) (EngineM IO))

  -- TODO: GET MESSAGE
  -- TODO: GET RECIPIENT
  -- TODO: GET PROTO-ID FROM MESSAGE

  messages <- receive fake (To (FakePeer 0))

  runEngineM env $ do

    for_ messages $ \(From peer, AnyMessage n msg) -> do

      local (\(EngineEnv _ s b) -> EngineEnv undefined s b) $ do

        -- FIXME: dispatcher!
        case Map.lookup n decoders of
          Just (AnyProtocol {protoDecode = decoder, handle = h}) -> maybe (pure ()) h (decoder msg)
          Nothing -> pure ()




