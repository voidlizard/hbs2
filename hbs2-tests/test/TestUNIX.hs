{-# Language TemplateHaskell #-}
module Main where

import HBS2.Prelude.Plated
import HBS2.Clock
import HBS2.Net.Proto
import HBS2.Net.Messaging.Unix
import HBS2.Actors.Peer
import HBS2.OrDie

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


debug :: (MonadIO m) => Doc ann -> m ()
debug p = liftIO $ hPrint stderr p


data PingPong e = Ping Int
                | Pong Int
                deriving stock (Eq,Generic,Show,Read)


instance Serialise (PingPong e)


instance HasProtocol UNIX  (PingPong UNIX) where
  type instance ProtocolId (PingPong UNIX) = 1
  type instance Encoded UNIX = ByteString
  decode = either (const Nothing) Just . deserialiseOrFail
  encode = serialise

pingPongHandler :: forall e m  . ( MonadIO m
                                 , Response e (PingPong e) m
                                 , HasProtocol e (PingPong e)
                                 )
                => Int
                -> PingPong e
                -> m ()

pingPongHandler n = \case

    Ping c -> debug ("Ping" <+> pretty c) >> response (Pong @e c)

    Pong c | c < n -> debug ("Pong" <+> pretty c) >> response (Ping @e (succ c))
           | otherwise -> pure ()

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
  liftIO $ hSetBuffering stdout LineBuffering
  liftIO $ hSetBuffering stderr LineBuffering

  withSystemTempDirectory "test-unix-socket" $ \tmp -> do

    let soname = tmp </> "unix.socket"

    server <- newMessagingUnix True 1.0 soname

    client <- newMessagingUnix False 1.0 soname

    m1 <- async $ runMessagingUnix server
    m2 <- async $ runMessagingUnix client

    p1 <- async $ runPingPong server do
                    runProto @UNIX
                      [ makeResponse (pingPongHandler 100000)
                      ]

    p2 <- async $ runPingPong  client do
                    request (msgUnixSelf server) (Ping @UNIX 0)
                    runProto @UNIX
                      [ makeResponse (pingPongHandler 100000)
                      ]

    (_,r) <- liftIO $ waitAnyCatchCancel [m1,m2,p1,p2]

    debug (viaShow r)



