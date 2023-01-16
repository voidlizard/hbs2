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
import Control.Monad.Identity

data ProtocolA = ProtocolA

data ProtocolB = ProtocolB

instance HasProtocol ProtocolA where
  type instance ProtocolId ProtocolA = 1
  type instance Encoded ProtocolA = String
  decode = undefined
  encode = undefined


instance HasProtocol ProtocolB where
  type instance ProtocolId ProtocolB = 2
  type instance Encoded ProtocolB = String
  decode = undefined
  encode = undefined

-- class Response p (m :: Type -> Type) where
--   answer :: p -> m ()

data AnyProtocol m = forall a . ( HasProtocol a
                                , KnownNat (ProtocolId a)
                                , Response a m
                                ) =>
  AnyProtocol
  { getProtoId  :: Integer
  , protoDecode :: String -> Maybe a
  , protoEncode :: a -> String
  , handle      :: a -> m ()
  }


data PingPong = Ping Int
              | Pong Int


instance HasProtocol PingPong where
  type instance ProtocolId PingPong = 3
  type instance Encoded PingPong = PingPong
  decode = undefined
  encode = undefined


class Response p (m :: Type -> Type) where
  answer :: p -> m ()

anyProtocol :: forall p  m   . ( MonadIO m
                               , Response p m
                               , HasProtocol p
                               , KnownNat (ProtocolId p)
                               )
            => (p -> m ()) -> AnyProtocol m

anyProtocol h = AnyProtocol { getProtoId  = natVal (Proxy @(ProtocolId p))
                            , protoDecode = decode @p
                            , protoEncode = encode @p
                            , handle = h
                            }


newtype EngineM m a = EngineM { fromEngine :: ReaderT () m a }
                      deriving ( Functor, Applicative, Monad, MonadTrans, MonadIO )

runEngineM :: EngineM m a -> m a
runEngineM f = runReaderT (fromEngine f) ()

instance (Monad m, HasProtocol p) => Response p m where
  answer = undefined


testUniqiProtoId :: IO ()
testUniqiProtoId = do

  let decoders = mempty :: Map Integer (AnyProtocol (EngineM IO))

  -- TODO: GET MESSAGE
  -- TODO: GET RECIPIENT
  -- TODO: GET PROTO-ID FROM MESSAGE

  let pingpong = anyProtocol @PingPong @(EngineM IO)
        \case
          Ping c -> lift (print "effect: PING") >> answer (Pong c)
          Pong _ -> lift (print "effect: PONG")


  -- FIXME: dispatcher!
  case Map.lookup 3 decoders of
    Just (AnyProtocol {protoDecode = decoder, handle = h}) -> maybe (pure ()) (runEngineM . h) (decoder "AAA")
    Nothing -> pure ()

  -- let qq = natVal (Proxy @(ProtocolId ProtocolA))

  pure ()



