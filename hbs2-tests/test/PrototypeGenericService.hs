{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}

module Main where

import HBS2.Actors.Peer
import HBS2.Clock
import HBS2.Net.Messaging.Unix
import HBS2.Net.Proto
import HBS2.Prelude.Plated
-- import HBS2.Net.Proto.Definition
import HBS2.Net.Proto.Service

import HBS2.System.Logger.Simple

import Codec.Serialise
import Control.Monad.Reader
import Data.ByteString.Lazy (ByteString)
import System.FilePath.Posix
-- import System.IO
-- import System.IO.Temp
import UnliftIO.Async
import Data.List

import UnliftIO
import Test.Tasty.HUnit

data Method1
data Method2

type MyServiceMethods1 = '[ Method1, Method2 ]

instance HasProtocol UNIX  (ServiceProto MyServiceMethods1 UNIX) where
  type instance ProtocolId (ServiceProto MyServiceMethods1 UNIX) = 0xd79349a1bffb70c4
  type instance Encoded UNIX = ByteString
  decode = either (const Nothing) Just . deserialiseOrFail
  encode = serialise


-- instance (MonadIO m, HasProtocol UNIX (ServiceProto MyServiceMethods1 UNIX)) => HasTimeLimits UNIX (ServiceProto MyServiceMethods1 UNIX) m where
--   tryLockForPeriod _ _ = pure True

instance MonadIO m => HandleMethod m Method1 where
  type instance Input Method1 = String
  type instance Output Method1 = String
  handleMethod n = do
    debug $ "SERVICE1. METHOD1" <+> pretty n
    case n of
      "JOPA"   -> pure "KITA"
      "PECHEN" -> pure "TRESKI"
      _        -> pure "X3"

instance MonadIO m => HandleMethod m Method2 where
  type instance Input Method2 = ()
  type instance Output Method2 = ()
  handleMethod _ = pure ()


instance (HasProtocol UNIX (ServiceProto api UNIX), MonadUnliftIO m)
  => HasDeferred UNIX (ServiceProto api UNIX) m where
  deferred _ m = void (async m)

main :: IO ()
main = do

  setLogging @DEBUG  (logPrefix "[debug] ")
  setLogging @INFO   (logPrefix "")
  setLogging @ERROR  (logPrefix "[err] ")
  setLogging @WARN   (logPrefix "[warn] ")
  setLogging @NOTICE (logPrefix "[notice] ")
  setLogging @TRACE  (logPrefix "[trace] ")

  withSystemTempDirectory "test-unix-socket" $ \tmp -> do

    let soname = tmp </> "unix.socket"

    server <- newMessagingUnixOpts [MUFork] True 1.0 soname
    client1 <- newMessagingUnix False 1.0 soname

    m1 <- async $ runMessagingUnix server

    pause @'Seconds 0.10

    m2 <- async $ runMessagingUnix client1

    p1 <- async $ flip runReaderT server do
                    runProto @UNIX
                      [ makeResponse (makeServer @MyServiceMethods1)
                      ]

    caller <- makeServiceCaller @MyServiceMethods1 @UNIX (msgUnixSelf server)

    p2 <- async $ runReaderT (runServiceClient caller) client1

    link p1
    link p2

    results <- forConcurrently ["JOPA", "PECHEN", "WTF?"] $ \r -> do
                answ <- callService @Method1 caller r
                pure (r, answ)

    debug $ "GOT RESPONSES (Method1): " <+> viaShow results

    assertBool "assert1" (sortOn fst results == [("JOPA",Right "KITA"),("PECHEN",Right "TRESKI"),("WTF?",Right "X3")] )

    r2 <- callService @Method2 caller ()

    debug $ "GOT RESPONSE (Method2): " <+> viaShow r2

    assertBool "assert2" (r2 == Right ())

    cancel p1
    pause @'Seconds 0.10

    waitAnyCatchCancel [p1,p2,m1,m2]

  setLoggingOff @DEBUG
  setLoggingOff @INFO
  setLoggingOff @ERROR
  setLoggingOff @WARN
  setLoggingOff @NOTICE
  setLoggingOff @TRACE


