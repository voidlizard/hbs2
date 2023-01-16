{-# Language TypeFamilyDependencies  #-}
{-# Language AllowAmbiguousTypes  #-}
module TestUniqProtoId where

import HasProtocol

import Data.Kind
import GHC.TypeLits
import Data.Proxy
import Data.Map qualified as Map
import Data.Map (Map)
import Control.Monad.Reader

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
  type instance Peer Fake = Int

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

data EngineEnv = forall p . HasPeer p =>
  EngineEnv
  { peer :: Maybe (Peer p)
  }

newtype EngineM m a = EngineM { fromEngine :: ReaderT EngineEnv m a }
                      deriving ( Functor, Applicative, Monad, MonadTrans, MonadIO )

runEngineM :: EngineEnv -> EngineM m a -> m a
runEngineM e f = runReaderT (fromEngine f) e

instance (Monad m, HasProtocol e p) => Response e p (EngineM m) where
  response _ = do
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

  let env = EngineEnv @Fake Nothing

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

