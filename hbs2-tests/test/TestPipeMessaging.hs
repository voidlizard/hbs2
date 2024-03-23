{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE NumericUnderscores #-}
module Main where

import HBS2.Prelude.Plated

import HBS2.Net.Messaging
import HBS2.Net.Messaging.Pipe
import HBS2.Net.Proto.Service
import HBS2.Actors.Peer

import HBS2.System.Logger.Simple.ANSI

import Data.ByteString.Lazy (ByteString)
import System.Posix.IO
import UnliftIO
import Control.Monad.Trans.Cont
import Control.Monad.Reader
import Codec.Serialise
import Data.Fixed

import System.TimeIt

-- protocol's data
data Ping =
    Ping Int
  | Pong Int
  deriving stock (Eq,Show,Generic)

instance Pretty Ping where
  pretty = viaShow

instance Serialise Ping

-- API definition
type MyServiceMethods1 = '[ Ping ]

-- API endpoint definition
type instance Input Ping = Ping
type instance Output Ping = Maybe Ping

-- API handler
instance MonadIO m => HandleMethod m Ping where
  handleMethod = \case
    Ping n -> pure (Just (Pong n))
    Pong _ -> pure Nothing

-- Codec for protocol
instance HasProtocol PIPE (ServiceProto MyServiceMethods1 PIPE) where
  type instance ProtocolId (ServiceProto MyServiceMethods1 PIPE) = 0xDEADF00D1
  type instance Encoded PIPE = ByteString
  decode = either (const Nothing) Just . deserialiseOrFail
  encode = serialise

-- Some "deferred" implementation for our monad
--   note -- plain asyncs may cause to resource leak
instance (MonadUnliftIO  m, HasProtocol PIPE (ServiceProto api PIPE))
  => HasDeferred (ServiceProto api PIPE) PIPE m where
  deferred m = void (async m)

mainLoop :: IO ()
mainLoop = do

  flip runContT pure do

    -- pipe for server
    (i1,o1) <- liftIO $ createPipe
              >>= \(i,o) -> (,) <$> fdToHandle i <*> fdToHandle o

    -- pipe for client
    (i2,o2) <- liftIO $ createPipe
              >>= \(i,o) -> (,) <$> fdToHandle i <*> fdToHandle o

    -- interwire client and server by  pipes
    server <- newMessagingPipe (i2,o1)
    client <- newMessagingPipe (i1,o2)

    -- run messaging workers
    void $ ContT $ withAsync $ runMessagingPipe server
    void $ ContT $ withAsync $ runMessagingPipe client

    -- make server protocol responder
    void $ ContT $ withAsync $ flip runReaderT server do
                    runProto @PIPE
                      [ makeResponse (makeServer @MyServiceMethods1)
                      ]

    -- make client's "caller"
    caller <- lift $ makeServiceCaller @MyServiceMethods1 @PIPE (localPeer client)

    -- make client's endpoint worker
    void $ ContT $ withAsync $ runReaderT (runServiceClient caller) client

    let n = 20_000

    (a, _) <- timeItT do
      for_ [1..n] $ \i -> do
        void $ callService @Ping caller (Ping i)

    debug $ "sent" <+> pretty n <+> "messages in" <+> pretty (realToFrac a :: Fixed E3) <> "sec"
          <> line
          <> "rps:" <+> pretty (realToFrac n / realToFrac a :: Fixed E2)

main :: IO ()
main = do

  setLogging @DEBUG defLog
  mainLoop
    `finally` do
      setLoggingOff @DEBUG


