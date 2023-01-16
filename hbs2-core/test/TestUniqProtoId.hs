{-# Language TypeFamilyDependencies  #-}
{-# Language FunctionalDependencies #-}
{-# Language AllowAmbiguousTypes  #-}
module TestUniqProtoId where

import HasProtocol

import Data.Kind
import GHC.TypeLits
import Data.Proxy
import Data.Map qualified as Map
import Data.Map (Map)
import Control.Monad.Reader
import Data.ByteString (ByteString)

import Data.List qualified as List
import Data.Cache qualified as Cache
import Data.Cache (Cache)
import Control.Concurrent.STM.TChan as Chan
import Control.Concurrent.STM
import Data.Hashable
import Data.Maybe


newtype From a = From (Peer a)

newtype To a = To (Peer a)

class HasPeer proto => Messaging bus proto msg  | bus -> proto, bus -> msg where

  sendTo  :: MonadIO m => bus -> To proto -> From proto -> msg -> m ()
  receive :: MonadIO m => bus -> To proto -> m [(From proto, msg)]


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

type family Encoding a :: Type

data Fake


instance HasPeer Fake where
  newtype instance Peer Fake = FakePeer Int
                               deriving newtype (Hashable)
                               deriving stock (Eq)

instance HasProtocol Fake PingPong where
  type instance ProtocolId PingPong = 1
  type instance Encoded Fake = String
  decode = undefined
  encode = undefined

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

data AnyMessage = AnyMessage Integer ByteString

data EngineEnv = forall p bus . (HasPeer p, Messaging bus p AnyMessage)  =>
  EngineEnv
  { peer   :: Maybe (Peer p)
  , bus    :: bus
  }

newtype EngineM m a = EngineM { fromEngine :: ReaderT EngineEnv m a }
                      deriving ( Functor, Applicative, Monad, MonadTrans, MonadIO )

runEngineM :: EngineEnv -> EngineM m a -> m a
runEngineM e f = runReaderT (fromEngine f) e

instance (Monad m, HasProtocol e p) => Response e p (EngineM m) where
  response resp = do
    -- TODO: get bus
    -- TODO: encode
    -- TODO: sendTo
    undefined

pingPongHandler :: forall a  m . (MonadIO m, Response a PingPong  m) => PingPong -> m ()
pingPongHandler =
  \case
    Ping c -> liftIO (print "effect: PING") >> response @a @PingPong (Pong c)
    Pong _ -> liftIO (print "effect: PONG")

testUniqiProtoId :: IO ()
testUniqiProtoId = do

  fake <- newFakeP2P True

  let env = EngineEnv @Fake Nothing fake

  let pingpong = makeResponse pingPongHandler

  let decoders = mempty :: Map Integer (AnyProtocol String (EngineM IO))
  let dec = Map.insert 1 pingpong decoders

  -- TODO: GET MESSAGE
  -- TODO: GET RECIPIENT
  -- TODO: GET PROTO-ID FROM MESSAGE

  let message = "" :: Encoded Fake

  -- FIXME: dispatcher!
  case Map.lookup 1 dec of
    Just (AnyProtocol {protoDecode = decoder, handle = h}) -> maybe (pure ()) (runEngineM env . h) (decoder message)
    Nothing -> pure ()

  pure ()

