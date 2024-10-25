{-# Language TemplateHaskell #-}
{-# Language UndecidableInstances #-}
module HBS2.Net.Messaging.Unix
  ( module HBS2.Net.Messaging.Unix
  , module HBS2.Net.Messaging
  , module HBS2.Net.Proto.Types
  , SocketClosedException
  ) where

import HBS2.Prelude.Plated
import HBS2.System.Dir
import HBS2.Net.Proto.Types
import HBS2.Actors.Peer.Types
import HBS2.Net.Messaging
import HBS2.Net.Messaging.Stream

import HBS2.System.Logger.Simple

import Control.Monad.Reader hiding (reader)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Hashable
import Data.HashMap.Strict qualified as HM
import Data.HashMap.Strict (HashMap)
import Data.Maybe
import Data.List qualified as List
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
import Control.Concurrent.STM (retry)
import Streaming.Prelude qualified as S

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
    MUNoFork
  | MUDontRetry
  deriving (Eq,Ord,Show,Generic,Data)

-- TODO: counters-to-detect-zombies
--   $class: leak
--   добавить счётчики для обнаружения
--   мёртвых соединений, а так же их отстрел.
--   есть основания полагать, что Messaging
--   может течь.
--
--   Шаг 1. добавить счётчики
--   Шаг 2. убедиться, что ресурсы текут
--   Шаг 3. устранить течь
--   Шаг 4. убедиться, что течь устранена

-- FIXME: use-bounded-queues
data MessagingUnix =
  MessagingUnix
  { msgUnixSockPath    :: FilePath
  , msgUnixServer      :: Bool
  , msgUnixRetryTime   :: Timeout 'Seconds
  , msgUnixSelf        :: Peer UNIX
  , msgUnixOpts        :: Set MessagingUnixOpts
  , msgAnyProbe        :: TVar AnyProbe
  , msgUnixSendTo      :: TVar (HashMap (Peer UNIX) (TQueue ByteString))
  , msgUnixRecv        :: TQueue (From UNIX, ByteString)
  , msgUnixLast        :: TVar TimeSpec
  , msgUnixAccepts     :: TVar Int
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
                <$> newTVarIO (AnyProbe ())
                <*> liftIO (newTVarIO mempty)
                <*> liftIO newTQueueIO
                <*> liftIO (newTVarIO now)
                <*> liftIO (newTVarIO 0)

data ReadTimeoutException = ReadTimeoutException deriving (Show, Typeable)

instance Exception ReadTimeoutException

data UnixMessagingStopped = UnixMessagingStopped deriving (Show,Typeable)

instance Exception UnixMessagingStopped

setProbe :: MonadIO m => MessagingUnix -> AnyProbe -> m ()
setProbe MessagingUnix{..} p = atomically $ writeTVar msgAnyProbe p

myAcceptReport :: MonadUnliftIO m => MessagingUnix -> [(Text,Integer)] -> m ()
myAcceptReport MessagingUnix{..} values = do
  p <- readTVarIO msgAnyProbe
  debug "myAcceptReport"
  acceptReport p values

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

      touch (msgUnixSockPath env)

      sock <- ContT $ bracket openSock closeSock

      _ <- ContT $ bracket (pure forked)  $ \clients -> do
             readTVarIO clients >>= mapM_ cancel

      liftIO $ bind sock $ SockAddrUnix (msgUnixSockPath env)
      liftIO $ listen sock 1024

      void $ ContT $ withAsync do
        pause @'Seconds 10
        readTVarIO forked >>= filterM (fmap isNothing . poll)
           >>= atomically . writeTVar forked
        n1 <- readTVarIO forked <&> List.length
        myAcceptReport env [("forked", fromIntegral n1)]

      let reportStuff = forever do
           pause @'Seconds 10
           what <- S.toList_ do
             n1 <- atomically $ readTVar (msgUnixSendTo env) <&> fromIntegral . HM.size
             S.yield ("msgUnixSendTo", n1)
           myAcceptReport env what

      void $ ContT $ bracket (async reportStuff) cancel

      forever do
        (so, _sa) <- liftIO $ accept sock

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

          let writer = liftIO $ async do
               -- FIXME: check!
               mq <- atomically $ readTVar (msgUnixSendTo env) <&> HM.lookup that
               for_ mq $ \q -> do

                 forever do

                    msg <- liftIO . atomically $ readTQueue q

                    let len = fromIntegral $ LBS.length msg :: Int
                    let _bs = bytestring32 (fromIntegral len)

                    liftIO $ sendAll so $ bytestring32 (fromIntegral len)

                    -- debug $ "sendAll" <+> pretty len <+> pretty (LBS.length msg) <+> viaShow bs

                    liftIO $ sendAll so (LBS.toStrict msg)

          void $ ContT $ bracket ( pure so ) closeSock

          void $ ContT $ bracket (createQueues env that) dropQueuesFor

          void $ ContT $ bracket ( debug $ "Client thread started" <+> pretty that )
                                 ( \_ -> debug $ "Client thread finished" <+> pretty that  )

          void $ ContT $ bracket writer (\x -> pause @'Seconds 0.1 >> cancel x)

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
            atomically do
              writeTVar (msgUnixLast env) now
              writeTVar seen now

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

      sockReady <- newTVarIO False

      void $ ContT $ bracket (createQueues env who) dropQueuesFor

      let attemptConnect = do
            result <- liftIO $ try $ connect sock $ SockAddrUnix (msgUnixSockPath env)
            case result of
              Right _ -> do
                atomically $ writeTVar sockReady True

              Left (e :: SomeException) -> do
                warn $ "MessagingUnix. failed to connect" <+> pretty sa <+> viaShow e
                pause (msgUnixRetryTime env)
                attemptConnect

      attemptConnect

      writer <- ContT $ liftIO . withAsync  do

                  forever do

                    atomically do
                      readTVar sockReady `orElse` retry

                    -- Мы клиент. Шлём кому? **ЕМУ**, на том конце трубы.
                    -- У нас один контрагент, имя сокета (файла) == адрес пира.
                    -- Как в TCP порт сервиса (а отвечает тот с другого порта)
                    mq <- atomically $ readTVar (msgUnixSendTo env) <&> HM.lookup who

                    maybe1 mq (err "MessagingUnix. no queue") $ \q -> do
                      -- если WD установлен, то просыпаемся, скажем, wd/2 и
                      -- шлём пустую строку серверу
                      -- withWD do
                      -- debug "FUCKING SEND"
                      msg <- liftIO $ atomically $ readTQueue q
                      let len = fromIntegral $ LBS.length msg :: Int
                      liftIO $ sendAll sock ( bytestring32 (fromIntegral len) <> LBS.toStrict msg)


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

      r <- waitAnyCatchCancel [reader, writer]

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
        modifyTVar (msgUnixSendTo env) (HM.delete who)
        -- modifyTVar (msgUnixRecvFrom env) (HashMap.delete who)

createQueues :: MonadIO m => MessagingUnix -> Peer UNIX -> m (Peer UNIX)
createQueues env who = liftIO do
  atomically $ do

    sHere <- readTVar (msgUnixSendTo env) <&> HM.member who

    if sHere then do
      pure False
    else do
      sendToQ   <- newTQueue
      modifyTVar (msgUnixSendTo env) (HM.insert who sendToQ)
      pure True

  pure who

instance Messaging MessagingUnix UNIX ByteString where

  sendTo bus (To who) (From _me) msg = liftIO do

    -- createQueues bus who

    -- FIXME: handle-no-queue-for-rcpt-situation-1

    mq <- atomically $ readTVar (msgUnixSendTo bus) <&> HM.lookup who

    maybe1 mq none $ \q -> do
      atomically $ writeTQueue q msg

  receive bus _ = liftIO do

    let q = msgUnixRecv bus
    atomically $ peekTQueue q >> flushTQueue q

instance (Monad m, Messaging MessagingUnix UNIX (Encoded UNIX)) => HasFabriq UNIX (ReaderT MessagingUnix m) where
  getFabriq = asks Fabriq

instance Monad m => HasOwnPeer UNIX (ReaderT MessagingUnix m) where
  ownPeer = asks msgUnixSelf

