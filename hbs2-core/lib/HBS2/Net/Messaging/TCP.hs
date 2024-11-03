{-# Language TemplateHaskell #-}
{-# LANGUAGE ImplicitParams #-}
module HBS2.Net.Messaging.TCP
  ( MessagingTCP
  , runMessagingTCP
  , newMessagingTCP
  , tcpSOCKS5
  , tcpOwnPeer
  , tcpPeerConn
  , tcpCookie
  , tcpOnClientStarted
  , messagingTCPSetProbe
  ) where

import HBS2.Clock
import HBS2.Net.IP.Addr
import HBS2.Net.Messaging
import HBS2.Prelude.Plated

import HBS2.Net.Messaging.Stream

import HBS2.System.Logger.Simple

import Control.Monad.Trans.Maybe
import Data.Bits
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.Maybe
import Data.Word
import Lens.Micro.Platform
import Network.ByteOrder hiding (ByteString)
import Network.Simple.TCP
import Network.Socket hiding (listen,connect)
import System.Random hiding (next)
import Control.Monad.Trans.Cont
import Control.Exception

import UnliftIO (MonadUnliftIO(..))
import UnliftIO.Async
import UnliftIO.STM
import UnliftIO.Exception qualified as U
import Streaming.Prelude qualified as S

{-HLINT ignore "Functor law"-}

-- FIXME: control-recv-capacity-to-avoid-leaks

outMessageQLen :: Natural
outMessageQLen = 256

-- | TCP Messaging environment
data MessagingTCP =
  MessagingTCP
  { _tcpSOCKS5           :: Maybe (PeerAddr L4Proto)
  , _tcpOwnPeer          :: Peer L4Proto
  , _tcpCookie           :: Word32
  , _tcpPeerConn         :: TVar (HashMap (Peer L4Proto) Word64)
  , _tcpPeerCookie       :: TVar (HashMap Word32 Int)
  , _tcpConnDemand       :: TQueue (Peer L4Proto)
  , _tcpReceived         :: TBQueue (Peer L4Proto, ByteString)
  , _tcpSent             :: TVar (HashMap (Peer L4Proto) (TBQueue ByteString))
  , _tcpClientThreadNum  :: TVar Int
  , _tcpClientThreads    :: TVar (HashMap Int (Async ()))
  , _tcpProbe            :: TVar AnyProbe
  , _tcpOnClientStarted  :: PeerAddr L4Proto -> Word64 -> IO () -- ^ Cient TCP connection succeed
  }

makeLenses 'MessagingTCP


newClientThread :: forall m . MonadUnliftIO m => MessagingTCP -> m () -> m Int
newClientThread MessagingTCP{..} a = do
  as <- async a
  atomically do
    n <- stateTVar _tcpClientThreadNum ( \x -> (x, succ x))
    modifyTVar _tcpClientThreads (HM.insert n as)
    pure n

delClientThread :: MonadIO m => MessagingTCP -> Int -> m ()
delClientThread MessagingTCP{..} threadId = atomically $
  modifyTVar' _tcpClientThreads (HM.delete threadId)

messagingTCPSetProbe :: MonadIO m => MessagingTCP -> AnyProbe -> m ()
messagingTCPSetProbe MessagingTCP{..} p = atomically $ writeTVar _tcpProbe p

newMessagingTCP :: ( MonadIO m
                   , FromSockAddr 'TCP (Peer L4Proto)
                   )
                => PeerAddr L4Proto
                -> m MessagingTCP

newMessagingTCP pa = liftIO do
  MessagingTCP Nothing
    <$> fromPeerAddr pa
    <*> randomIO
    <*> newTVarIO mempty
    <*> newTVarIO mempty
    <*> newTQueueIO
    <*> newTBQueueIO outMessageQLen
    <*> newTVarIO mempty
    <*> newTVarIO 0
    <*> newTVarIO mempty
    <*> newTVarIO (AnyProbe ())
    <*> pure (\_ _ -> none) -- do nothing by default

instance Messaging MessagingTCP L4Proto ByteString where

  sendTo MessagingTCP{..} (To p) (From _f) msg = liftIO do
    -- let _own = tcpOwnPeer
    -- debug $ "!!!! FUCKING SEND TO" <+> pretty p

    queue <- atomically do
      q' <- readTVar _tcpSent <&> HM.lookup p

      case q' of
        Nothing  -> do
          writeTQueue _tcpConnDemand p
          q <- newTBQueue outMessageQLen
          modifyTVar _tcpSent (HM.insert p q)
          pure q

        Just q -> pure q

    atomically $ writeTBQueueDropSTM 10 queue msg

    -- debug $ "!!!! FUCKING SEND TO" <+> pretty p <+> "DONE"

  receive MessagingTCP{..} _ = liftIO do
    atomically do
      s0 <- readTBQueue _tcpReceived
      sx <- flushTBQueue _tcpReceived
      pure $ fmap (over _1 From) ( s0 : sx )

connectionId :: Word32 -> Word32 -> Word64
connectionId a b = (fromIntegral hi `shiftL` 32) .|. fromIntegral low
  where
    low = min a b
    hi  = max a b

data ConnType = Server | Client
                deriving (Eq,Ord,Show,Generic)


sendCookie :: MonadIO m
           => MessagingTCP
           -> Socket
           -> m ()

sendCookie env so = do
  let coo = view tcpCookie env & bytestring32
  send so coo

recvCookie :: MonadIO m
           => MessagingTCP
           -> Socket
           -> m Word32

recvCookie _ so = liftIO do
  scoo <- readFromSocket so 4 <&> LBS.toStrict
  pure $ word32 scoo

handshake :: MonadIO m
           => ConnType
           -> MessagingTCP
           -> Socket
           -> m Word32

handshake Server env so = do
  cookie <- recvCookie env so
  sendCookie env so
  pure cookie

handshake Client env so = do
  sendCookie env so
  recvCookie env so

writeTBQueueDropSTM :: Integral n
                 => n
                 -> TBQueue a
                 -> a
                 -> STM ()
writeTBQueueDropSTM inQLen newInQ bs = do
  flip fix inQLen $ \more j -> do
    when (j > 0) do
      full <- isFullTBQueue newInQ
      if not full then do
        writeTBQueue newInQ bs
      else do
        void $ tryReadTBQueue newInQ
        more (pred j)


killCookie :: Int -> Maybe Int
killCookie  = \case
     1 -> Nothing
     n -> Just (pred n)

useCookie :: (?env :: MessagingTCP, MonadIO m) => Word32 -> m Bool
useCookie cookie = atomically do
  let MessagingTCP{..} = ?env
  n <- readTVar _tcpPeerCookie <&> HM.member cookie
  unless n do
    modifyTVar _tcpPeerCookie (HM.insertWith (+) cookie 1)
  pure n

runMessagingTCP :: forall m . MonadIO m => MessagingTCP -> m ()
runMessagingTCP env@MessagingTCP{..} = liftIO do

  void $ flip runContT pure do

    forever do

      p1 <- ContT $ withAsync runClient
      p2 <- ContT $ withAsync runServer

      probes <- ContT $ withAsync $ forever do
        pause @'Seconds 10
        p <- readTVarIO _tcpProbe
        acceptReport p =<< S.toList_ do
          S.yield =<< ( readTVarIO _tcpClientThreads <&> ("tcpClientThreads",) . fromIntegral . HM.size )
          S.yield =<< ( readTVarIO _tcpPeerConn <&> ("tcpPeerConn",) . fromIntegral . HM.size)
          S.yield =<< ( readTVarIO _tcpPeerCookie <&> ("tcpPeerCookie",) . fromIntegral . HM.size)
          S.yield =<< ( readTVarIO _tcpSent <&> ("tcpSent",) . fromIntegral . HM.size)

      waitAnyCatchCancel [p1,p2,probes]

    where

      readFrames so peer queue = forever $ do
        void $ readFromSocket so 4 <&> LBS.toStrict
        ssize <- readFromSocket so 4 <&> LBS.toStrict
        let size = word32 ssize & fromIntegral
        bs <- readFromSocket so size
        atomically $ writeTBQueueDropSTM outMessageQLen queue (peer, bs)

      runServer = do

        own <- toPeerAddr $ view tcpOwnPeer env
        let (L4Address _ (IPAddrPort (i,p))) = own
        let myCookie = view tcpCookie env

        -- server
        liftIO $ listen (Host (show i)) (show p) $ \(sock, sa) -> do
          withFdSocket sock setCloseOnExecIfNeeded
          debug $ "Listening on" <+> pretty sa

          forever do
            void $ acceptFork sock $ \(so, remote) -> void $ flip runContT pure $ callCC \exit -> do
              liftIO $ withFdSocket so setCloseOnExecIfNeeded
              debug $ "!!! GOT INCOMING CONNECTION FROM !!!"
                <+> brackets (pretty own)
                <+> brackets (pretty sa)

              let ?env = env

              cookie <- handshake Server env so

              when (cookie == myCookie) $ exit ()

              here <- useCookie cookie

              when here $ do
                debug $ "SERVER : ALREADY CONNECTED" <+> pretty cookie <+> viaShow so
                exit ()

              let newP = fromSockAddr @'TCP  remote :: Peer L4Proto

              newOutQ <- do
                atomically do
                  mbQ <- readTVar _tcpSent <&> HM.lookup newP
                  maybe (newTBQueue outMessageQLen) pure mbQ

              atomically do
                modifyTVar _tcpSent (HM.insert newP newOutQ)
                modifyTVar _tcpPeerConn (HM.insert newP (connectionId myCookie cookie))

              wr <- ContT $ withAsync $ forever  do
                      bs <- atomically $ readTBQueue newOutQ

                      -- FIXME: check-this!
                      let pq = myCookie -- randomIO
                      let qids = bytestring32 pq
                      let size = bytestring32 (fromIntegral $ LBS.length bs)

                      let frame = LBS.fromStrict qids
                                   <> LBS.fromStrict size -- req-size
                                   <> bs -- payload

                      sendLazy so frame --(LBS.toStrict frame)

              rd <- ContT $ withAsync $ readFrames so newP _tcpReceived

              void $ ContT $ bracket none $ const do
                debug $ "SHUTDOWN SOCKET AND SHIT" <+> pretty remote
                shutdown so ShutdownBoth
                cancel rd
                cancel wr

                atomically do
                  modifyTVar _tcpSent (HM.delete newP)
                  modifyTVar _tcpPeerCookie (HM.update killCookie cookie)

              void $ waitAnyCatchCancel [rd,wr]


      runClient = flip runContT pure do

        own <- toPeerAddr $ view tcpOwnPeer env
        let (L4Address _ (IPAddrPort (i,_))) = own
        let myCookie = view tcpCookie env

        pause @'Seconds 10

        void $ ContT $ bracket none $ const $ do
          what <- atomically $ stateTVar _tcpClientThreads (\x -> (HM.elems x, mempty))
          mapM_ cancel what

        void $ ContT $ withAsync $ forever do
          pause @Seconds 60

          done <- readTVarIO _tcpClientThreads <&> HM.toList
                   >>= filterM ( \(_,x) -> isJust <$> poll x )
                   <&> HS.fromList . fmap fst

          atomically $ modifyTVar _tcpClientThreads (HM.filterWithKey (\k _ -> not (HS.member k done)))

        forever $ void $ runMaybeT do
            -- client sockets

            -- смотрим к кому надо
            who <- atomically $ readTQueue _tcpConnDemand
            whoAddr <- toPeerAddr who

            already <- atomically $ readTVar _tcpPeerConn <&> HM.member who

            when already do
              debug "SHIT? BUSYLOOP?"
              mzero

            liftIO $ newClientThread env $ do
              let (L4Address _ (IPAddrPort (ip,port))) = whoAddr
              connect (show ip) (show port) $ \(so, remoteAddr) -> do

                let ?env = env

                flip runContT pure $ callCC \exit -> do

                  debug $ "OPEN CLIENT CONNECTION" <+> pretty ip <+> pretty port <+> pretty remoteAddr
                  cookie <- handshake Client env so
                  let connId = connectionId cookie myCookie

                  when (cookie == myCookie) $ do
                    debug $ "same peer, exit" <+> pretty remoteAddr
                    exit ()

                  here <- useCookie cookie

                  -- TODO: handshake notification
                  liftIO $ _tcpOnClientStarted whoAddr connId

                  when here do
                    debug $ "CLIENT: ALREADY CONNECTED" <+> pretty cookie <+> pretty ip <+> pretty port
                    exit ()

                  atomically $ modifyTVar _tcpPeerCookie (HM.insertWith (+) cookie 1)

                  wr <- ContT $ withAsync $ forever  do
                          bss <- atomically do
                                   q' <- readTVar _tcpSent <&> HM.lookup who
                                   maybe1 q' mempty $ \q -> do
                                    s <- readTBQueue q
                                    sx <- flushTBQueue q
                                    pure (s:sx)

                          for_ bss $ \bs -> do
                            -- FIXME: check-this!
                            let pq = myCookie -- randomIO
                            let qids = bytestring32 pq
                            let size = bytestring32 (fromIntegral $ LBS.length bs)

                            let frame = LBS.fromStrict qids
                                         <> LBS.fromStrict size -- req-size
                                         <> bs -- payload

                            sendLazy so frame --(LBS.toStrict frame)

                  void $ ContT $ bracket none (const $ cancel wr)

                  void $ ContT $ bracket none $ const $ do
                    atomically do
                      modifyTVar _tcpPeerConn (HM.delete who)
                      modifyTVar _tcpPeerCookie (HM.update killCookie cookie)

                  readFrames so who _tcpReceived

