{-# Language TemplateHaskell #-}
{-# Language UndecidableInstances #-}
module HBS2.Net.Peer where

import HBS2.Prelude
import HBS2.Prelude.Plated
import HBS2.Net.Proto
import HBS2.Net.Messaging
import HBS2.Clock
import HBS2.Actors
import HBS2.Defaults

import Lens.Micro.Platform
import Data.ByteString.Lazy ( ByteString )
import Data.Foldable
import Control.Monad.Reader
import Data.Map qualified as Map
import GHC.TypeLits
import Control.Monad.Trans.Maybe
import Control.Concurrent.Async
import Data.Kind

import Codec.Serialise hiding (encode,decode)

data AnyMessage e = AnyMessage Integer (Encoded e)
                    deriving stock (Generic)

instance Serialise (Encoded e) => Serialise (AnyMessage e)


data EngineEnv e  = forall bus  . ( Messaging bus e ByteString
                                  , Serialise (Encoded e)
                                  )  =>
  EngineEnv
  { _peer   :: Maybe (Peer e)
  , _self   :: Peer e
  , bus     :: bus
  , defer   :: Pipeline IO ()
  }


newtype EngineM e m a = EngineM { fromEngine :: ReaderT (EngineEnv e) m a }
                        deriving newtype ( Functor
                                         , Applicative
                                         , Monad
                                         , MonadIO
                                         , MonadReader (EngineEnv e)
                                         , MonadTrans
                                         )

data ResponseEnv e =
  ResponseEnv
  { _engine   :: EngineEnv e
  , _respPeer :: Peer e
  }

newtype ResponseM e m a = ResponseM { fromResponse :: ReaderT (ResponseEnv e) m a }
                          deriving newtype ( Functor
                                           , Applicative
                                           , Monad
                                           , MonadIO
                                           , MonadReader (ResponseEnv e)
                                           , MonadTrans
                                           )


makeLenses 'EngineEnv

makeLenses 'ResponseEnv

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


runEngineM :: EngineEnv e -> EngineM e m a -> m a
runEngineM e f = runReaderT (fromEngine f) e

runResponseM :: Monad m => EngineEnv e -> Peer e -> ResponseM e m a -> EngineM e m a
runResponseM eng p f = lift $ runReaderT (fromResponse f) (ResponseEnv eng p)

instance (MonadIO m, HasProtocol e p) => Request e p (EngineM e m) where
  request p msg = do
    let proto = protoId @e @p (Proxy @p)
    ask >>= \case
      EngineEnv { _self = s, bus = b} -> do
        let bs = serialise (AnyMessage @e proto (encode msg))
        liftIO $ sendTo b (To p) (From s) bs

instance (HasProtocol e p, Serialise (Encoded e)) => Response e p (ResponseM e IO) where

  thatPeer _ = asks (view respPeer)

  deferred _ m = do
    e@(EngineEnv { defer = d }) <- asks (view engine)
    p <- asks (view respPeer)
    addJob d (runEngineM e (runResponseM e p m))

  response resp = do
    env <- ask
    let p = env ^. respPeer
    let s   = env ^. (engine . self)
    let proto = protoId @e @p (Proxy @p)
    let bs = serialise (AnyMessage @e proto (encode resp))

    -- TODO: wrap somehow
    case env ^. engine of
      EngineEnv { bus = b } -> liftIO $ sendTo b (To p) (From s) bs

newEnv :: forall e bus m . ( Monad m
                           , MonadIO m
                           , Messaging bus e ByteString
                           , Serialise (Encoded e)
                           )
       => Peer e
       -> bus
       -> m (EngineEnv e)

newEnv p pipe = do
  de <- liftIO $ newPipeline defProtoPipelineSize
  pure $ EngineEnv Nothing p pipe de

runPeer :: forall e m a . ( MonadIO m
                          )
        => EngineEnv e
        -> [AnyProtocol e (ResponseM e m)]
        -> m a

runPeer env@(EngineEnv {bus = pipe, defer = d}) hh = do

  let me = env ^. self

  let resp = [ (pid, a) | a@AnyProtocol { myProtoId = pid } <- hh ]

  let disp = Map.fromList resp

  void $ liftIO $ async $ runPipeline d

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
                                  }) -> maybe (pure ()) (runResponseM env pip . h) (decoder msg)

