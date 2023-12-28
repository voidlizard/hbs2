{-# Language TemplateHaskell #-}
module Main where

import HBS2.Prelude.Plated
import HBS2.Clock
import HBS2.Net.Proto
import HBS2.Net.Messaging.Unix
import HBS2.Actors.Peer
import HBS2.OrDie

import HBS2.System.Logger.Simple

import Codec.Serialise
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Data.ByteString.Lazy (ByteString)
import Lens.Micro.Platform
import Prettyprinter
import System.FilePath.Posix
import System.IO
import System.IO.Temp
import UnliftIO.Async
import UnliftIO qualified as UIO
import UnliftIO (TVar)

data PingPong e = Ping Int
                | Pong Int
                deriving stock (Eq,Generic,Show,Read)


instance Serialise (PingPong e)


instance HasProtocol UNIX  (PingPong UNIX) where
  type instance ProtocolId (PingPong UNIX) = 1
  type instance Encoded UNIX = ByteString
  decode = either (const Nothing) Just . deserialiseOrFail
  encode = serialise

pingPongHandlerS :: forall e m  . ( MonadIO m
                                  , Response e (PingPong e) m
                                  , HasProtocol e (PingPong e)
                                  , Pretty (Peer e)
                                  )
                => TVar [(Peer e, PingPong e)]
                -> Int
                -> PingPong e
                -> m ()

pingPongHandlerS tv n msg = do

    that <- thatPeer @(PingPong e)

    UIO.atomically $ UIO.modifyTVar tv ((that,msg):)

    case msg of

      Ping c -> do
        debug ("S: Ping" <+> pretty c <+> "from" <+> pretty that ) >> response (Pong @e c)

      Pong _ -> pure ()

pingPongHandler1 :: forall e m  . ( MonadIO m
                                 , Response e (PingPong e) m
                                 , HasProtocol e (PingPong e)
                                 )
                => TVar [PingPong e]
                -> Int
                -> PingPong e
                -> m ()

pingPongHandler1 t n msg = do

  UIO.atomically $ UIO.modifyTVar t (msg:)

  case msg of

    Ping c -> pure ()
    Pong c -> pure ()

    -- Pong c | c < n -> debug ("C1: Pong" <+> pretty c) >> response (Ping @e (succ c))
    --        | otherwise -> pure ()


pingPongHandler2 :: forall e m  . ( MonadIO m
                                 , Response e (PingPong e) m
                                 , HasProtocol e (PingPong e)
                                 )
                => TVar [PingPong e]
                -> Int
                -> PingPong e
                -> m ()

pingPongHandler2 t n msg = do

    UIO.atomically $ UIO.modifyTVar t (msg:)

    case msg of

      Ping c -> pure ()
      Pong c -> pure ()

      -- Pong c | c < n -> debug ("C2: Pong" <+> pretty c) >> response (Ping @e (succ c))
      --        | otherwise -> pure ()


data PPEnv =
  PPEnv
  { _ppSelf :: Peer UNIX
  , _ppFab  :: Fabriq UNIX
  }

makeLenses 'PPEnv

newtype PingPongM m a = PingPongM { fromPingPong :: ReaderT PPEnv m a }
                        deriving newtype ( Functor
                                         , Applicative
                                         , Monad
                                         , MonadIO
                                         , MonadUnliftIO
                                         , MonadReader PPEnv
                                         , MonadTrans
                                         )

runPingPong :: (MonadIO m, PeerMessaging UNIX) => MessagingUnix -> PingPongM m a -> m a
runPingPong tran m = runReaderT (fromPingPong m) (PPEnv (msgUnixSelf tran) (Fabriq tran))

instance Monad m => HasFabriq UNIX (PingPongM m) where
  getFabriq = asks (view ppFab)

instance Monad m => HasOwnPeer UNIX (PingPongM m) where
  ownPeer = asks (view ppSelf)

instance HasTimeLimits UNIX (PingPong UNIX) IO where
  tryLockForPeriod _ _ = pure True

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

    server <- newMessagingUnix True 1.0 soname

    client1 <- newMessagingUnix False 1.0 soname
    client2 <- newMessagingUnix False 1.0 soname

    m1 <- async $ runMessagingUnix server
    m2 <- async $ runMessagingUnix client1
    m3 <- async $ runMessagingUnix client2

    trs <- UIO.newTVarIO []
    tr1 <- UIO.newTVarIO []
    tr2 <- UIO.newTVarIO []

    p1 <- async $ runPingPong server do
                    runProto @UNIX
                      [ makeResponse (pingPongHandlerS trs 2)
                      ]

    -- p2 <- async $ pause @'Seconds 300
    p2 <- async $ runPingPong  client1 do
                    -- pause @'Seconds 0.25
                    -- request (msgUnixSelf server) (Ping @UNIX 0)
                    l <- async $ runProto @UNIX
                      [ makeResponse (pingPongHandler1 tr1 10)
                      ]
                    link l
                    forM_ [1..10] $ \n-> request (msgUnixSelf server) (Ping @UNIX n)
                    wait l

    -- p3 <- async $ pause @'Seconds 300
    p3 <- async $ do
      runPingPong  client2 do
                    l <- async $ runProto @UNIX
                      [ makeResponse (pingPongHandler2 tr2 200)
                      ]
                    link l
                    forM_ (take 10 [10000000..]) $ \n-> request (msgUnixSelf server) (Ping @UNIX n)
                    wait l

    -- p4 <- async do
    pause @'Seconds 10
    UIO.readTVarIO trs >>= print . vcat . fmap (\(a,b) -> pretty (a, show b))
    UIO.readTVarIO tr1 >>= print
    UIO.readTVarIO tr2 >>= print

    cancel m1

    (_,r) <- liftIO $ waitAnyCatchCancel [m1,m2,m3,p1,p2,p3]

    debug (viaShow r)



