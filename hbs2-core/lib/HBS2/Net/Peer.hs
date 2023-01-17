{-# Language TemplateHaskell #-}
{-# Language UndecidableInstances #-}
module HBS2.Net.Peer where

import HBS2.Prelude
import HBS2.Prelude.Plated
import HBS2.Net.Proto
import HBS2.Net.Messaging
import HBS2.Clock

import Lens.Micro.Platform
import Data.ByteString.Lazy ( ByteString )
import Data.Foldable
import Control.Monad.Reader
import Data.Map qualified as Map
import GHC.TypeLits
import Control.Monad.Trans.Maybe

import Codec.Serialise hiding (encode,decode)

data AnyMessage e = AnyMessage Integer (Encoded e)
                    deriving stock (Generic)

instance Serialise (Encoded e) => Serialise (AnyMessage e)

data EngineEnv e = forall bus . ( Messaging bus e ByteString
                                , Serialise (Encoded e)
                                )  =>
  EngineEnv
  { _peer   :: Maybe (Peer e)
  , _self   :: Peer e
  , bus     :: bus
  }

makeLenses 'EngineEnv

data AnyProtocol e m = forall p  . ( HasProtocol e p
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
                                 , MonadIO
                                 , MonadReader (EngineEnv e)
                                )

runEngineM :: EngineEnv e -> EngineM e m a -> m a
runEngineM e f = runReaderT (fromEngine f) e


instance (MonadIO m, HasProtocol e p) => Request e p (EngineM e m) where
  request p msg = do
    let proto = protoId @e @p (Proxy @p)
    ask >>= \case
      EngineEnv { _self = s, bus = b} -> do
        let bs = serialise (AnyMessage @e proto (encode msg))
        liftIO $ sendTo b (To p) (From s) bs

instance (MonadIO m, HasProtocol e p) => Response e p (EngineM e m) where
  response resp = do
    env <- ask
    let proto = protoId @e @p (Proxy @p)
    case env of
      (EngineEnv { _peer = Just p
                 , _self = s
                 , bus = b
                 } ) -> do
                  let bs = serialise (AnyMessage @e proto (encode resp))
                  liftIO $ sendTo b (To p) (From s) bs
      _ -> pure ()


newEnv :: forall e bus m . ( Monad m
                           , Messaging bus e ByteString
                           , Serialise (Encoded e)
                           )
       => Peer e
       -> bus
       -> m (EngineEnv e)

newEnv p pipe = pure $ EngineEnv Nothing p pipe


runPeer :: forall e m a . ( MonadIO m
                          )
        => EngineEnv e
        -> [AnyProtocol e (EngineM e m)]
        -> m a

runPeer env@(EngineEnv {bus = pipe}) hh = do

  let me = env ^. self

  let resp = [ (pid, a) | a@AnyProtocol { myProtoId = pid } <- hh ]

  let disp = Map.fromList resp

  runEngineM env $ do

    forever $ do
      messages <- receive pipe (To me)

      for_ messages $ \(From pip, bs) -> do

        case deserialiseOrFail @(AnyMessage e) bs of

          Left _-> pure () -- liftIO $ print "failed to deserialise"

          Right (AnyMessage n msg) -> do

            local (set peer (Just pip))  do

              case Map.lookup n disp of
                Nothing -> pure ()

                Just (AnyProtocol { protoDecode = decoder
                                  , handle = h
                                  }) -> maybe (pure ()) h (decoder msg)

