{-# Language TemplateHaskell #-}
{-# Language UndecidableInstances #-}
module HBS2.Net.Messaging.Unix
  ( module HBS2.Net.Messaging.Unix
  , module HBS2.Net.Messaging
  , module HBS2.Net.Proto.Types
  , SocketClosedException
  ) where

import HBS2.Prelude.Plated
import HBS2.Net.Proto.Types
import HBS2.Actors.Peer.Types
import HBS2.Net.Messaging
import HBS2.Net.Messaging.Stream
import HBS2.Clock

import HBS2.System.Logger.Simple

import Control.Monad
import Control.Monad.Reader hiding (reader)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Hashable
import Data.HashMap.Strict qualified as HashMap
import Data.HashMap.Strict (HashMap)
import Network.ByteOrder hiding (ByteString)
import Network.Socket
import Network.Socket.ByteString hiding (sendTo)
import Network.Socket.ByteString.Lazy qualified as SL
import Control.Concurrent.STM.TQueue (flushTQueue)
import Data.Set (Set)
import Data.Set qualified as Set
import Lens.Micro.Platform
import Control.Monad.Trans.Cont
import UnliftIO

import Streaming.Prelude qualified as S

import Control.Concurrent (myThreadId)

data UNIX = UNIX
            deriving (Eq,Ord,Show,Generic)

type PeerUnixAddr = String

instance HasPeer UNIX where
  newtype instance Peer UNIX = PeerUNIX { _fromPeerUnix :: PeerUnixAddr}
    deriving stock (Eq,Ord,Show,Generic)
    deriving newtype (Pretty)


instance IsString (Peer UNIX) where
  fromString = PeerUNIX

instance Hashable (Peer UNIX) where
  hashWithSalt salt (PeerUNIX p) = hashWithSalt salt p

{- HLINT ignore "Use newtype instead of data" -}
data MessagingUnixOpts =
    MUWatchdog Int
  | MUNoFork
  | MUDontRetry
  | MUKeepAlive Int
  deriving (Eq,Ord,Show,Generic,Data)

-- FIXME: use-bounded-queues
data MessagingUnix =
  MessagingUnix
  { msgUnixSockPath    :: FilePath
  , msgUnixServer      :: Bool
  , msgUnixRetryTime   :: Timeout 'Seconds
  , msgUnixSelf        :: Peer UNIX
  , msgUnixOpts        :: Set MessagingUnixOpts
  , msgUnixSendTo      :: TVar (HashMap (Peer UNIX) (TQueue ByteString))
  , msgUnixRecv        :: TQueue (From UNIX, ByteString)
  , msgUnixLast        :: TVar TimeSpec
  , msgUnixAccepts     :: TVar Int
  , msgSockets         :: TVar (HashMap (Peer UNIX) Socket)
  }

makeLenses 'PeerUNIX

newMessagingUnix :: MonadIO m
                 => Bool
                 -> Timeout 'Seconds
                 -> FilePath
                 -> m MessagingUnix

newMessagingUnix server tsec path = do
  newMessagingUnixOpts mempty server tsec path

newMessagingUnixOpts :: (MonadIO m)
                 => [MessagingUnixOpts]
                 -> Bool
                 -> Timeout 'Seconds
                 -> FilePath
                 -> m MessagingUnix

newMessagingUnixOpts opts server tsec path = do
  now <- getTimeCoarse
  MessagingUnix path
                server
                tsec
                (PeerUNIX path)
                (Set.fromList opts)
                <$> liftIO (newTVarIO mempty)
                <*> liftIO newTQueueIO
                <*> liftIO (newTVarIO now)
                <*> liftIO (newTVarIO 0)
                <*> liftIO (newTVarIO mempty)


data ReadTimeoutException = ReadTimeoutException deriving (Show, Typeable)

instance Exception ReadTimeoutException

data UnixMessagingStopped = UnixMessagingStopped deriving (Show,Typeable)

instance Exception UnixMessagingStopped

runMessagingUnix :: MonadUnliftIO m => MessagingUnix -> m ()
runMessagingUnix env = do

  if msgUnixServer env then
    liftIO runServer
  else
    runClient

  where

    runServer = forever $ handleAny cleanupAndRetry $ flip runContT pure $ do

      t0 <- getTimeCoarse
      atomically $ writeTVar (msgUnixLast env) t0

      forked <- newTVarIO (mempty :: [Async ()])

      let fork w = do
            l <- async w
            atomically $ modifyTVar forked (l :)

      let doFork  = not $ Set.member MUNoFork (msgUnixOpts env)

      let withSession | doFork    = void . liftIO . fork
                      | otherwise = void . liftIO

      -- watchdog <- liftIO $ async runWatchDog

      let openSock = liftIO $ socket AF_UNIX Stream defaultProtocol
      let closeSock = liftIO . close

      sock <- ContT $ bracket openSock closeSock

      _ <- ContT $ bracket (pure forked)  $ \clients -> do
             readTVarIO clients >>= mapM_ cancel

      liftIO $ bind sock $ SockAddrUnix (msgUnixSockPath env)
      liftIO $ listen sock 5

      forever do
        (so, sa) <- liftIO $ accept sock

        withSession $ flip runContT void do

          peerNum <- atomically $ do
            n <- readTVar (msgUnixAccepts env)
            modifyTVar    (msgUnixAccepts env) succ
            pure n

          seen <- getTimeCoarse >>= newTVarIO

          let that = if doFork then
                       msgUnixSelf env & over fromPeerUnix (<> "#" <> show peerNum)
                     else
                       msgUnixSelf env

          let writer = liftIO $ async $ pause @'Seconds 0.001 >> forever do
                mq <- atomically $ readTVar (msgUnixSendTo env) <&> HashMap.lookup that

                maybe1 mq none $ \q -> do
                  msg <- liftIO . atomically $ readTQueue q

                  let len = fromIntegral $ LBS.length msg :: Int
                  let bs = bytestring32 (fromIntegral len)

                  liftIO $ sendAll so $ bytestring32 (fromIntegral len)

                  -- debug $ "sendAll" <+> pretty len <+> pretty (LBS.length msg) <+> viaShow bs

                  liftIO $ SL.sendAll so msg

          void $ ContT $ bracket ( pure so ) closeSock

          void $ ContT $ bracket (createQueues env that) dropQueuesFor

          void $ ContT $ bracket ( debug $ "Client thread started" <+> pretty that )
                                 ( \_ -> debug $ "Client thread finished" <+> pretty that  )

          void $ ContT $ bracket writer cancel

          fix \next -> do

            let mq = Just (msgUnixRecv env)

            -- frameLen <- liftIO $ recv so 4 <&> word32 <&> fromIntegral
            frameLen <- liftIO $ readFromSocket so 4 <&> LBS.toStrict <&> word32 <&> fromIntegral

            -- debug $ "frameLen" <+> pretty frameLen

            if frameLen == 0 then do
              -- answer to watchdog message
              liftIO $ sendAll so $ bytestring32 0
            else do
              frame    <- liftIO $ readFromSocket so frameLen --  <&> LBS.toStrict
              maybe1 mq none $ \q -> do
                atomically $ writeTQueue q (From that, frame)

            now <- getTimeCoarse
            -- TODO: to-remove-global-watchdog
            atomically $ writeTVar (msgUnixLast env) now
            atomically $ writeTVar seen now
            next


    clientLoop m = fix \next -> do
      m
      if not (MUDontRetry `elem` msgUnixOpts env) then do
        trace "LOOP!"
        next
      else do
        trace "LOOP EXIT"

    handleClient | MUDontRetry `elem` msgUnixOpts env  = \_ w -> handleAny throwStopped w
                 | otherwise =  handleAny

    throwStopped _ = throwIO UnixMessagingStopped

    runClient = liftIO $ clientLoop $ handleClient logAndRetry $ flip runContT pure $ do

      let sa   = SockAddrUnix (msgUnixSockPath env)
      let p  = msgUnixSockPath env
      let who = PeerUNIX p
      tseen <- getTimeCoarse >>= newTVarIO

      let openSock = liftIO $ socket AF_UNIX Stream defaultProtocol
      let closeSock = close

      sock <- ContT $ bracket openSock closeSock

      void $ ContT $ bracket (createQueues env who) dropQueuesFor


      let attemptConnect = do
            result <- liftIO $ try $ connect sock $ SockAddrUnix (msgUnixSockPath env)
            case result of
              Right _ -> none
              Left (e :: SomeException) -> do
                pause (msgUnixRetryTime env)
                warn $ "MessagingUnix. failed to connect" <+> pretty sa <+> viaShow e
                pause @'Seconds 2.5
                attemptConnect

      attemptConnect

      reader <- ContT $ liftIO . withAsync do
            forever do
              let q = msgUnixRecv env

            -- Read response from server
              frameLen <- liftIO $ readFromSocket sock 4 <&> LBS.toStrict <&> word32 <&> fromIntegral

              getTimeCoarse >>= (atomically . writeTVar tseen)

              frame    <- liftIO $ readFromSocket sock frameLen

              -- when (frameLen > 0) do
                -- сообщения кому? **МНЕ**
                -- сообщения от кого? от **КОГО-ТО**
              atomically $ writeTQueue q (From who, frame)

      watchdog <- ContT $ liftIO . withAsync do
            let mwd = headMay [ n | MUWatchdog n <- Set.toList (msgUnixOpts env) ]
            case mwd of
              Nothing -> forever (pause @'Seconds 600)
              Just n  -> forever do

                pause (TimeoutSec (realToFrac n))

                now <- getTimeCoarse
                seen <- readTVarIO tseen

                let diff = toNanoSeconds $ TimeoutTS (now - seen)

                trace $ "I'm a watchdog!" <+> pretty diff

                when ( diff > toNanoSeconds (TimeoutSec $ realToFrac n) ) do
                  trace "watchdog fired!"
                  throwIO ReadTimeoutException


      writer <- ContT $ liftIO . withAsync  do

                  let mwd = headMay [ n | MUWatchdog n <- Set.toList (msgUnixOpts env) ]

                  let withWD m = case mwd of
                        Nothing -> m
                        Just n -> do
                          let nwait = max 1 (realToFrac n * 0.7)
                          e <- race (pause (TimeoutSec nwait)) m
                          case e of
                            Right{} -> pure ()
                            Left{}  -> do
                              liftIO $ sendAll sock $ bytestring32 0
                              -- liftIO $ SL.sendAll sock ""

                  forever do

                    -- Мы клиент. Шлём кому? **ЕМУ**, на том конце трубы.
                    -- У нас один контрагент, имя сокета (файла) == адрес пира.
                    -- Как в TCP порт сервиса (а отвечает тот с другого порта)
                    mq <- atomically $ readTVar (msgUnixSendTo env) <&> HashMap.lookup who

                    maybe1 mq none $ \q -> do
                      -- если WD установлен, то просыпаемся, скажем, wd/2 и
                      -- шлём пустую строку серверу
                      withWD do
                        msg <- liftIO $ atomically $ readTQueue q
                        let len = fromIntegral $ LBS.length msg :: Int
                        liftIO $ SL.sendAll sock $ ( LBS.fromStrict (bytestring32 (fromIntegral len)) <> msg)

      r <- waitAnyCatchCancel [reader, writer, watchdog]

      case snd r of
        Right{} -> pure ()
        Left e  -> throwIO e

    cleanupAndRetry e = liftIO do
      warn $ "MessagingUnix. client seems gone. restaring server" <+> pretty (msgUnixSelf env)
      err (viaShow e)
      atomically $ writeTVar (msgUnixAccepts env) 0

      liftIO $ atomically $ void $ flushTQueue (msgUnixRecv env)

      dropQueues

      pause (msgUnixRetryTime env)


    logAndRetry :: SomeException -> IO ()
    logAndRetry e = do
      warn $ "MessagingUnix. runClient failed, probably server is gone. Retrying:" <+> pretty (msgUnixSelf env)
      err (viaShow e)
      dropQueues
      pause (msgUnixRetryTime env)

    dropQueues :: MonadIO m => m ()
    dropQueues = do
      void $ liftIO $ atomically $ flushTQueue (msgUnixRecv env)
      liftIO $ atomically $ modifyTVar (msgUnixSendTo env) mempty
      -- мы не дропаем обратную очередь (принятые сообщения), потому,
      -- что нет смысла. она живёт столько, сколько живёт клиент
      -- очередь отправки мы удаляем, потому, что этого клиента
      -- мы больше никогда не увидим, ведь они разделяются на уровне
      -- сокетов и больше никак.

    dropQueuesFor :: MonadIO m => Peer UNIX -> m ()
    dropQueuesFor who = liftIO do
      atomically do
        modifyTVar (msgUnixSendTo env) (HashMap.delete who)
        -- modifyTVar (msgUnixRecvFrom env) (HashMap.delete who)

createQueues :: MonadIO m => MessagingUnix -> Peer UNIX -> m (Peer UNIX)
createQueues env who = liftIO do
  atomically $ do

    sHere <- readTVar (msgUnixSendTo env) <&> HashMap.member who

    if sHere then do
      pure False
    else do
      sendToQ   <- newTQueue
      modifyTVar (msgUnixSendTo env) (HashMap.insert who sendToQ)
      pure True

  pure who

instance Messaging MessagingUnix UNIX ByteString where

  sendTo bus (To who) (From me) msg = liftIO do

    -- createQueues bus who

    -- FIXME: handle-no-queue-for-rcpt-situation-1

    mq <- atomically $ readTVar (msgUnixSendTo bus) <&> HashMap.lookup who

    maybe1 mq none $ \q -> do
      atomically $ writeTQueue q msg

  receive bus _ = liftIO do

    let q = msgUnixRecv bus
    atomically $ peekTQueue q >> flushTQueue q

instance (Monad m, Messaging MessagingUnix UNIX (Encoded UNIX)) => HasFabriq UNIX (ReaderT MessagingUnix m) where
  getFabriq = asks Fabriq

instance Monad m => HasOwnPeer UNIX (ReaderT MessagingUnix m) where
  ownPeer = asks msgUnixSelf

