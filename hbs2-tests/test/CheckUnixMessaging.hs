{-# LANGUAGE ImportQualifiedPost #-}

-- May develop an run it with command:
-- ```
-- nix develop -c ghcid -c "cabal repl" hbs2-tests:test-unix-messaging -r=Main.main
-- ```

module Main where

import Codec.Serialise
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Reader hiding (reader)
import Control.Monad.Trans.Cont
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy (ByteString)
import Data.Hashable
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Time
import Lens.Micro.Platform
import Network.Socket
import Network.Socket.ByteString hiding (sendTo)
import Network.Socket.ByteString.Lazy qualified as SL
import UnliftIO
import UnliftIO.Async
import UnliftIO.Concurrent

import HBS2.OrDie
import HBS2.Actors.Peer
import HBS2.Base58
import HBS2.Data.Types.Refs
import HBS2.Merkle
import HBS2.Net.Messaging.Unix
import HBS2.Net.Proto
import HBS2.Net.Proto.Service
import HBS2.Net.Proto.Service hiding (decode, encode)
import HBS2.Prelude.Plated
import HBS2.System.Logger.Simple.ANSI
import HBS2.Storage

soname = "/tmp/hbs2-dev.sock"

data EchoH

type DevAPI = '[EchoH]

instance HasProtocol UNIX (ServiceProto DevAPI UNIX) where
    type ProtocolId (ServiceProto DevAPI UNIX) = 0xDE50000
    type Encoded UNIX = ByteString
    decode = either (const Nothing) Just . deserialiseOrFail
    encode = serialise

type instance Input EchoH = Text
type instance Output EchoH = Either Text Text

sayt :: (MonadIO m) => Text -> m ()
sayt = liftIO . BS8.putStrLn . TE.encodeUtf8

instance (MonadIO m) => HandleMethod m EchoH where
    handleMethod msg = do
        now <- liftIO getCurrentTime
        -- threadDelay (10 ^ 5)
        let resp = (cs . show) now <> " " <> msg
        -- sayt $ "Got request: " <> resp
        pure $ Right $ resp

instance
    (MonadUnliftIO m)
    => HasDeferred (ServiceProto DevAPI UNIX) UNIX m
    where
    deferred m = void (async m)

withServer :: (() -> IO r) -> IO r
withServer = runContT do
    server <- newMessagingUnixOpts [] True 0.10 soname
    (link <=< ContT . withAsync) do
        runMessagingUnix server
    (link <=< ContT . withAsync) do
        flip runReaderT server do
            runProto @UNIX
                [ makeResponse (makeServer @DevAPI)
                ]

withClient :: (ServiceCaller DevAPI UNIX -> IO r) -> IO r
withClient = runContT do
    client <- newMessagingUnixOpts [] False 0.15 soname
    (link <=< ContT . withAsync) do
        runMessagingUnix client
    caller <- makeServiceCaller @DevAPI @UNIX (fromString soname)
    (link <=< ContT . withAsync) do
        liftIO $ runReaderT (runServiceClient @DevAPI @UNIX caller) client
    pure caller

main :: IO ()
main = do

  setLogging @ERROR  $ toStderr . logPrefix "[error] "
  setLogging @WARN   $ toStderr . logPrefix "[warn] "
  setLogging @NOTICE $ toStdout . logPrefix ""
  setLogging @DEBUG  $ toStderr . logPrefix "[debug] "

  totfuck <- newTVarIO 0

  flip runContT pure do
    void $ ContT withServer
    -- pause @'Seconds 1
    s <- replicateM 16 $ lift $ async do
      void $ flip runContT pure do
        caller <- ContT withClient
        tsucc <- newTVarIO 0
        tfail <- newTVarIO 0
        for_ [1..1000] $ \i -> do
          lift (callRpcWaitMay @EchoH (TimeoutSec 2) caller ((cs . show) i))
            >>= \case
                Just (Right _) -> atomically $ modifyTVar tsucc succ
                e              -> atomically (modifyTVar tfail succ) >> err (viaShow e)

        ok   <- readTVarIO tsucc
        fuck <- readTVarIO tfail
        atomically $ modifyTVar totfuck (+fuck)
        notice $ "Finished:" <+> "succeed" <+> pretty ok <+> "failed" <+> pretty fuck

    mapM_ wait s

  tf <- readTVarIO totfuck

  notice $ "total errors" <+> pretty tf

  setLoggingOff @ERROR
  setLoggingOff @WARN
  setLoggingOff @NOTICE
  setLoggingOff @DEBUG


