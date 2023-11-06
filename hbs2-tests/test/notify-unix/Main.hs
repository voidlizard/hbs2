module Main where

import HBS2.Prelude.Plated
import HBS2.Clock hiding (sec)
import HBS2.Net.Proto
import HBS2.Net.Messaging.Unix
import HBS2.Net.Proto.Notify
import HBS2.Actors.Peer
-- import HBS2.OrDie

import HBS2.System.Logger.Simple

-- import Codec.Serialise
import Control.Monad.Reader
-- import Control.Monad.Trans.Resource
import Data.ByteString.Lazy (ByteString)
-- import Lens.Micro.Platform
-- import Prettyprinter
import System.FilePath.Posix
-- import System.IO
-- import System.IO.Temp
-- import UnliftIO.Async
-- import UnliftIO qualified as UIO
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import UnliftIO
import Codec.Serialise

data Tick = Tick Int
            deriving stock Generic

instance Serialise Tick


instance HasProtocol UNIX  (NotifyProto Tick UNIX) where
  type instance ProtocolId (NotifyProto Tick UNIX) = 0xd71049a1bffb70c4
  type instance Encoded UNIX = ByteString
  decode = either (const Nothing) Just . deserialiseOrFail
  encode = serialise

instance  (MonadUnliftIO m, HasProtocol UNIX (NotifyProto ev e)) => HasDeferred UNIX (NotifyProto ev e) m where
  deferred _ m = void $ async m

data WhatTick = Odd | Even
     deriving stock (Generic,Eq)

instance Hashable WhatTick
instance Serialise WhatTick

newtype instance NotifyKey Tick  =
  TickNotifyKey WhatTick
  deriving (Generic)
  deriving newtype (Hashable,Eq)

newtype instance NotifyData Tick =
  TickNotifyData Int
  deriving Generic



instance Serialise (NotifyKey Tick)
instance Serialise (NotifyData Tick)


runTickTack :: MonadIO m => r -> ReaderT r m a -> m a
runTickTack s m = runReaderT m s

main :: IO ()
main = do

  setLogging @DEBUG  (logPrefix "[debug] ")
  setLogging @INFO   (logPrefix "")
  setLogging @ERROR  (logPrefix "[err] ")
  setLogging @WARN   (logPrefix "[warn] ")
  setLogging @NOTICE (logPrefix "[notice] ")
  setLogging @TRACE  (logPrefix "[trace] ")

  liftIO $ hSetBuffering stdout LineBuffering
  liftIO $ hSetBuffering stderr LineBuffering

  withSystemTempDirectory "test-unix-socket" $ \tmp -> do

    let soname = tmp </> "unix.socket"

    server  <- newMessagingUnix True 1.0 soname
    client1 <- newMessagingUnix False 1.0 soname

    m1 <- async $ runMessagingUnix server
    m2 <- async $ runMessagingUnix client1

    src <- newSomeNotifySource @Tick

    -- запускаем "часы"
    emitter <- async $ do
                 sec <- newTVarIO 0
                 forever do
                   sn <- atomically $ stateTVar sec (\s -> (s, succ s))
                   if even sn then do
                     emitNotify src (TickNotifyKey Even, TickNotifyData sn)
                   else
                     emitNotify src (TickNotifyKey Odd, TickNotifyData sn)
                   debug "SERVER: TICK!"
                   pause @'Seconds 1

    --  запускаем сервер
    p1 <- async $ liftIO $ runTickTack server do
                  env1 <- newNotifyEnvServer @Tick src
                  w <- async $ runNotifyWorkerServer env1
                  link w
                  runProto @UNIX
                    [ makeResponse (makeNotifyServer env1)
                    ]

    sink <- newNotifySink

    --  запускаем клиента
    p2 <- async $ runTickTack client1 $ do
                    void $ asyncLinked $ runNotifyWorkerClient sink
                    runProto @UNIX
                      [ makeResponse (makeNotifyClient @Tick sink)
                      ]

    s1 <- asyncLinked $ runNotifySink sink (TickNotifyKey Even) $ \(TickNotifyData td) -> do
      debug $ "CLIENT1:" <+> viaShow td

    s2 <- asyncLinked $ runNotifySink sink (TickNotifyKey Odd) $ \(TickNotifyData td) -> do
      debug $ "CLIENT2:" <+> viaShow td

    s3 <- async $ runNotifySink sink (TickNotifyKey Odd) $ \(TickNotifyData td) -> do
      debug $ "CLIENT3:" <+> viaShow td

    void $ async do
      pause @'Seconds 10
      cancelWith s3 (toException (userError "Fuck you!"))

    void $ waitAnyCatchCancel [p1,p2,m1,m2,s1,s2]

