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
  type instance ProtocolId PingPong = 1
  type instance Encoded PingPong = PingPong
  decode = undefined
  encode = undefined


class Response p (m :: Type -> Type) where
  answer :: p -> m ()

makeProtocol :: forall p  m  . ( MonadIO m
                               , Response p (EngineM m)
                               , HasProtocol p
                               , KnownNat (ProtocolId p)
                               )
            => (p -> EngineM m ()) -> AnyProtocol (EngineM m)

makeProtocol h = AnyProtocol { getProtoId  = natVal (Proxy @(ProtocolId p))
                             , protoDecode = decode @p
                             , protoEncode = encode @p
                             , handle = h
                             }

newtype EngineM m a = EngineM { fromEngine :: ReaderT () m a }
                      deriving ( Functor, Applicative, Monad, MonadTrans, MonadIO )

runEngineM :: EngineM m a -> m a
runEngineM f = runReaderT (fromEngine f) ()

instance (Monad m, HasProtocol p) => Response p (EngineM m) where
  answer _ = do
    -- TODO: get bus
    -- TODO: encode
    -- TODO: sendTo
    undefined

testUniqiProtoId :: IO ()
testUniqiProtoId = do

  let decoders = mempty :: Map Integer (AnyProtocol (EngineM IO))

  -- TODO: GET MESSAGE
  -- TODO: GET RECIPIENT
  -- TODO: GET PROTO-ID FROM MESSAGE

  let pingpong = makeProtocol @PingPong @IO
        \case
          Ping c -> lift (print "effect: PING") >> answer (Pong c)
          Pong _ -> lift (print "effect: PONG")

  -- FIXME: dispatcher!
  case Map.lookup 3 decoders of
    Just (AnyProtocol {protoDecode = decoder, handle = h}) -> maybe (pure ()) (runEngineM . h) (decoder "AAA")
    Nothing -> pure ()

  pure ()



