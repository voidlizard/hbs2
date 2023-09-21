module HBS2.Net.Messaging.Unix where

import HBS2.Prelude.Plated
import HBS2.Net.Proto.Types
import HBS2.Net.Messaging
import HBS2.Clock

import HBS2.System.Logger.Simple

import Control.Monad.Trans.Resource
import Control.Monad
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Function
import Data.Functor
import Data.Hashable
import Data.List qualified as List
import Network.ByteOrder hiding (ByteString)
import Network.Socket
import Network.Socket.ByteString
import Control.Concurrent.STM.TQueue (flushTQueue)
import Data.Set (Set)
import Data.Set qualified as Set
import UnliftIO

data UNIX = UNIX
            deriving (Eq,Ord,Show,Generic)

{- HLINT ignore "Use newtype instead of data" -}
data MessagingUnixOpts =
    MUWatchdog Int
  | MUFork
  deriving (Eq,Ord,Show,Generic,Data)

-- FIXME: use-bounded-queues
data MessagingUnix =
  MessagingUnix
  { msgUnixSockPath    :: FilePath
  , msgUnixServer      :: Bool
  , msgUnixRetryTime   :: Timeout 'Seconds
  , msgUnixSelf        :: Peer UNIX
  , msgUnixOpts        :: Set MessagingUnixOpts
  , msgUnixInbox       :: TQueue ByteString
  , msgUnixRecv        :: TQueue (From UNIX, ByteString)
  , msgUnixLast        :: TVar TimeSpec
  , msgUnixAccepts     :: TVar Int
  }



newMessagingUnix :: MonadIO m
                 => Bool
                 -> Timeout 'Seconds
                 -> FilePath
                 -> m MessagingUnix

newMessagingUnix server tsec path = do
  newMessagingUnixOpts mempty server tsec path

newMessagingUnixOpts :: MonadIO m
                 => [MessagingUnixOpts]
                 -> Bool
                 -> Timeout 'Seconds
                 -> FilePath
                 -> m MessagingUnix

newMessagingUnixOpts opts server tsec path = do
  let sa  = SockAddrUnix path
  now <- getTimeCoarse
  MessagingUnix path
                server
                tsec
                (PeerUNIX sa)
                (Set.fromList opts)
                <$> liftIO newTQueueIO
                <*> liftIO newTQueueIO
                <*> liftIO (newTVarIO now)
                <*> liftIO (newTVarIO 0)

instance HasPeer UNIX where
  newtype instance Peer UNIX = PeerUNIX {fromPeerUnix :: SockAddr}
    deriving stock (Eq,Ord,Show,Generic)
    deriving newtype (Pretty)

instance IsString (Peer UNIX) where
  fromString p  = PeerUNIX (SockAddrUnix p)

-- FIXME: fix-code-dup?
instance Hashable (Peer UNIX) where
  hashWithSalt salt p = case fromPeerUnix p of
    SockAddrInet  pn h     -> hashWithSalt salt (4, fromIntegral pn, h)
    SockAddrInet6 pn _ h _ -> hashWithSalt salt (6, fromIntegral pn, h)
    SockAddrUnix s         -> hashWithSalt salt ("unix", s)


data ReadTimeoutException = ReadTimeoutException deriving (Show, Typeable)

instance Exception ReadTimeoutException


runMessagingUnix :: MonadUnliftIO m => MessagingUnix -> m ()
runMessagingUnix env = do

  if msgUnixServer env then
    runServer
  else
    runClient

  where

    runServer = forever $ handleAny cleanupAndRetry $ runResourceT do

      t0 <- getTimeCoarse
      atomically $ writeTVar (msgUnixLast env) t0

      sock <- liftIO $ socket AF_UNIX Stream defaultProtocol

      void $ allocate (pure sock) (`shutdown` ShutdownBoth)

      liftIO $ bind sock $ SockAddrUnix (msgUnixSockPath env)
      liftIO $ listen sock 1

      let doFork  = Set.member MUFork (msgUnixOpts env)

      let withSession | doFork    = void . async . runResourceT
                      | otherwise = void . runResourceT

      watchdog <- async $ do

        let mwd = headMay [ n | MUWatchdog n <- Set.toList (msgUnixOpts env)  ]

        maybe1 mwd (forever (pause @'Seconds 60)) $ \wd -> do

          forever do

            pause $ TimeoutSec $ realToFrac $ min (wd `div` 2) 1

            now  <- getTimeCoarse
            seen <- readTVarIO (msgUnixLast env)
            acc  <- readTVarIO (msgUnixAccepts env)

            trace $ "watchdog" <+> pretty now <+> pretty seen <+> pretty acc

            let diff = toNanoSeconds $ TimeoutTS (now - seen)

            when ( acc > 0 && diff >= toNanoSeconds (TimeoutSec $ realToFrac wd) ) do
              throwIO ReadTimeoutException

      run <- async $ forever $ runResourceT do
              (so, sa) <- liftIO $ accept sock

              atomically $ modifyTVar (msgUnixAccepts env) succ

              withSession do

                void $ allocate (pure so) close

                writer <- async $ forever do
                  msg <- liftIO . atomically $ readTQueue (msgUnixInbox env)
                  let len = fromIntegral $ LBS.length msg :: Int
                  liftIO $ sendAll so $ bytestring32 (fromIntegral len)
                  liftIO $ sendAll so $ LBS.toStrict msg

                void $ allocate (pure writer) cancel

                link writer

                fix \next -> do
                  -- FIXME: timeout-hardcode
                  frameLen <- liftIO $ recv so 4 <&> word32 <&> fromIntegral
                  frame    <- liftIO $ recv so frameLen
                  atomically $ writeTQueue (msgUnixRecv env) (From (PeerUNIX sa), LBS.fromStrict frame)
                  now <- getTimeCoarse
                  atomically $ writeTVar (msgUnixLast env) now
                  next

      (_, r) <- waitAnyCatchCancel [run, watchdog]

      case r of
        Left e  -> throwIO e
        Right{} -> pure ()


    runClient = liftIO $ forever $ handleAny logAndRetry $ runResourceT do

      sock <- liftIO $ socket AF_UNIX Stream defaultProtocol

      void $ allocate (pure sock) close

      let sa   = SockAddrUnix (msgUnixSockPath env)

      let attemptConnect = do
            result <- liftIO $ try $ connect sock $ SockAddrUnix (msgUnixSockPath env)
            case result of
              Right _ -> return ()
              Left (e :: SomeException) -> do
                pause (msgUnixRetryTime env)
                warn $ "MessagingUnix. failed to connect" <+> pretty sa <+> viaShow e
                attemptConnect

      attemptConnect

      reader <- async $ forever do
                  -- Read response from server
                  frameLen <- liftIO $ recv sock 4 <&> word32 <&> fromIntegral
                  frame    <- liftIO $ recv sock frameLen
                  atomically $ writeTQueue (msgUnixRecv env) (From (PeerUNIX sa), LBS.fromStrict frame)

      forever do
        msg <- liftIO . atomically $ readTQueue (msgUnixInbox env)
        let len = fromIntegral $ LBS.length msg :: Int
        liftIO $ sendAll sock $ bytestring32 (fromIntegral len)
        liftIO $ sendAll sock $ LBS.toStrict msg

      void $ waitAnyCatchCancel [reader]

    cleanupAndRetry e = liftIO do
      warn $ "MessagingUnix. client seems gone. restaring server" <+> pretty (msgUnixSelf env)
      err (viaShow e)
      atomically $ writeTVar (msgUnixAccepts env) 0
      liftIO $ atomically $ void $ flushTQueue (msgUnixInbox env)
      liftIO $ atomically $ void $ flushTQueue (msgUnixRecv env)
      pause (msgUnixRetryTime env)

    logAndRetry :: SomeException -> IO ()
    logAndRetry e = do
      warn $ "MessagingUnix. runClient failed, probably server is gone. Retrying:" <+> pretty (msgUnixSelf env)
      err (viaShow e)
      pause (msgUnixRetryTime env)


instance Messaging MessagingUnix UNIX ByteString where

  sendTo bus (To _) _ msg = liftIO do
    atomically $ writeTQueue (msgUnixInbox bus) msg

  receive bus _ = liftIO do
    atomically $ readTQueue (msgUnixRecv bus) <&> List.singleton

