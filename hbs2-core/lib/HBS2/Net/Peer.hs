{-# Language TemplateHaskell #-}
{-# Language UndecidableInstances #-}
module HBS2.Net.Peer where

import HBS2.Prelude
import HBS2.Net.Proto
import HBS2.Net.Messaging

import Lens.Micro.Platform
import Data.ByteString.Lazy ( ByteString )
import Data.Foldable
import Control.Monad.Reader
import Data.Map qualified as Map
import Data.Proxy
import GHC.TypeLits
import Codec.Serialise qualified as S

data AnyMessage e = AnyMessage Integer (Encoded e)

data EngineEnv e = forall bus . (Messaging bus e (AnyMessage e))  =>
  EngineEnv
  { _peer   :: Maybe (Peer e)
  , _self   :: Peer e
  , bus    :: bus
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
        liftIO $ sendTo b (To p) (From s) (AnyMessage proto (encode msg))

instance (MonadIO m, HasProtocol e p) => Response e p (EngineM e m) where
  response resp = do
    env <- ask
    let proto = protoId @e @p (Proxy @p)
    case env of
      (EngineEnv { _peer = Just p
                 , _self = s
                 , bus = b
                 } ) -> do
                  liftIO $ sendTo b (To p) (From s) (AnyMessage proto (encode resp))
      _ -> pure ()


newEnv :: forall e bus m . ( Monad m
                           , Messaging bus e (AnyMessage e)
                           )
       => Peer e
       -> bus
       -> m (EngineEnv e)

newEnv p pipe = pure $ EngineEnv Nothing p pipe


runPeer :: MonadIO m => EngineEnv e -> [AnyProtocol e (EngineM e m)] -> m a
runPeer env@(EngineEnv {bus = pipe}) hh = do

  let me = env ^. self

  let resp = [ (pid, a) | a@AnyProtocol { myProtoId = pid } <- hh ]

  let disp = Map.fromList resp

  runEngineM env $ do

    forever $ do
      messages <- receive pipe (To me)

      for_ messages $ \(From pip, AnyMessage n msg) -> local (set peer (Just pip))  do

        case Map.lookup n disp of
          Nothing -> pure ()

          Just (AnyProtocol { protoDecode = decoder
                            , handle = h
                            }) -> maybe (pure ()) h (decoder msg)

