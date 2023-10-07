{-# Language TemplateHaskell #-}
module HBS2.Net.Messaging.TCP
  ( MessagingTCP
  , runMessagingTCP
  , newMessagingTCP
  , tcpOwnPeer
  , tcpPeerConn
  , tcpCookie
  , tcpOnClientStarted
  ) where

import HBS2.Clock
import HBS2.Net.IP.Addr
import HBS2.Net.Messaging
import HBS2.Net.Proto.Types
import HBS2.Prelude.Plated

import HBS2.Net.Messaging.Stream

import HBS2.System.Logger.Simple

import Control.Concurrent.STM (flushTQueue)
import Control.Exception (try,Exception,SomeException,throwIO)
import Control.Monad
import Data.Bits
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString qualified as BS
import Data.Function
import Data.Functor
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.List qualified as L
import Data.Maybe
import Data.Word
import Lens.Micro.Platform
import Network.ByteOrder hiding (ByteString)
import Network.Simple.TCP
import Network.Socket hiding (listen,connect)
-- import Network.Socket.ByteString.Lazy hiding (send,recv)
import Streaming.Prelude qualified as S
import System.Random hiding (next)
import Control.Monad.Trans.Resource

import UnliftIO.Async
import UnliftIO.STM
import UnliftIO.Exception qualified as U



-- FIXME: control-recv-capacity-to-avoid-leaks

-- | TCP Messaging environment
data MessagingTCP =
  MessagingTCP
  { _tcpOwnPeer          :: Peer L4Proto
  , _tcpCookie           :: Word32
  , _tcpConnPeer         :: TVar (HashMap Word64 (Peer L4Proto))
  , _tcpPeerConn         :: TVar (HashMap (Peer L4Proto) Word64)
  , _tcpConnUsed         :: TVar (HashMap Word64 Int)
  , _tcpConnQ            :: TVar (HashMap Word64 (TQueue (Peer L4Proto, ByteString)))
  , _tcpPeerPx           :: TVar (HashMap Word32 (Peer L4Proto))
  , _tcpPeerXp           :: TVar (HashMap (Peer L4Proto) Word32)
  , _tcpRecv             :: TQueue (Peer L4Proto, ByteString)
  , _tcpDefer            :: TVar (HashMap (Peer L4Proto) [(TimeSpec, ByteString)])
  , _tcpDeferEv          :: TQueue ()
  , _tcpOnClientStarted  :: PeerAddr L4Proto -> Word64 -> IO () -- ^ Cient TCP connection succeed
  }

makeLenses 'MessagingTCP

newMessagingTCP :: ( MonadIO m
                   , FromSockAddr 'TCP (Peer L4Proto)
                   )
                => PeerAddr L4Proto
                -> m MessagingTCP

newMessagingTCP pa = liftIO do
  MessagingTCP <$> fromPeerAddr pa
               <*> randomIO
               <*> newTVarIO mempty
               <*> newTVarIO mempty
               <*> newTVarIO mempty
               <*> newTVarIO mempty
               <*> newTVarIO mempty
               <*> newTVarIO mempty
               <*> newTQueueIO
               <*> newTVarIO mempty
               <*> newTQueueIO
               <*> pure (\_ _ -> none) -- do nothing by default

instance Messaging MessagingTCP L4Proto ByteString where

  sendTo bus (To p) (From f) msg = liftIO do
    let own = view tcpOwnPeer bus

    co' <- atomically $ readTVar (view tcpPeerConn bus) <&> HashMap.lookup p

    -- debug $ "sendTo" <+> brackets (pretty own)
    --                  <+> pretty p
    --                  <+> braces (pretty co')
    --                  <+> pretty (LBS.length msg)

    maybe1 co' defer $ \co -> do
      -- trace $ "writing to" <+> pretty co
      q' <- atomically $ readTVar (view tcpConnQ bus) <&> HashMap.lookup co
      maybe1 q' (warn $ "no queue for" <+> pretty co) $ \q -> do
        atomically $ writeTQueue q (p, msg)

    where
      defer = do
        warn $ "defer" <+> pretty p
        t <- getTimeCoarse
        atomically $ modifyTVar (view tcpDefer bus) (HashMap.insertWith (<>) p [(t, msg)])
        atomically $ writeTQueue (view tcpDeferEv bus) ()

  receive bus _ = liftIO do
    let q = view tcpRecv bus

    ms  <- atomically do
            r <- readTQueue q
            rs <- flushTQueue q
            pure (r:rs)

    forM ms $ \(p, msg) -> pure (From p, msg)



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

spawnConnection :: forall m . MonadIO m
               => ConnType
               -> MessagingTCP
               -> Socket
               -> SockAddr
               -> m ()

spawnConnection tp env so sa = liftIO do

  runResourceT do

    let myCookie = view tcpCookie env
    let own = view tcpOwnPeer env
    let newP = fromSockAddr @'TCP sa

    theirCookie <- handshake tp env  so

    let connId = connectionId myCookie theirCookie

    when (tp == Client && theirCookie /= myCookie) do
      pa <- toPeerAddr newP
      liftIO $ view tcpOnClientStarted env pa connId -- notify if we opened client tcp connection

    traceCmd own
             ( "spawnConnection "
                  <+> viaShow tp
                  <+> pretty myCookie
                  <+> pretty connId )
             newP

    debug $ "handshake" <+> viaShow tp
                        <+> brackets (pretty (view tcpOwnPeer env))
                        <+> pretty sa
                        <+> pretty theirCookie
                        <+> pretty connId

    used <- atomically $ do
              modifyTVar (view tcpConnUsed env) (HashMap.insertWith (+) connId 1)
              readTVar (view tcpConnUsed env) <&> HashMap.findWithDefault 0 connId


    void $ allocate (pure connId) cleanupConn

    debug $ "USED:" <+> viaShow tp <+> pretty own <+> pretty used

    when ( used <= 2 ) do
      atomically $ modifyTVar (view tcpPeerConn env) (HashMap.insert newP connId)

    when (used == 1) do
      q <- getWriteQueue connId
      updatePeer connId newP

      debug $ "NEW PEER" <+> brackets (pretty own)
                         <+> pretty connId
                         <+> pretty newP
                         <+> parens ("used:" <+> pretty used)

      rd <- async $ fix \next -> do

            spx <- readFromSocket so 4 <&> LBS.toStrict
            ssize <- readFromSocket so 4 <&> LBS.toStrict --- УУУ, фреейминг
            let px = word32 spx -- & fromIntegral
            let size = word32 ssize & fromIntegral


            bs <- readFromSocket so size

            memReqId newP px

            pxes <- readTVarIO (view tcpPeerPx env)

            let orig = fromMaybe (fromSockAddr @'TCP sa) (HashMap.lookup px pxes)

            -- debug $ "RECEIVED" <+> pretty orig <+> pretty (LBS.length bs)

            atomically $ writeTQueue (view tcpRecv env) (orig, bs)

            next

      wr <- async $ fix \next -> do
              (rcpt, bs) <- atomically $ readTQueue q

              pq <- makeReqId rcpt
              let qids = bytestring32 pq
              let size = bytestring32 (fromIntegral $ LBS.length bs)

              let frame = LBS.fromStrict qids
                           <> LBS.fromStrict size -- req-size
                           <> bs -- payload

              sendLazy so frame --(LBS.toStrict frame)
              next

      void $ waitAnyCatchCancel [rd,wr]

    -- lift $ cleanupConn connId

  -- gracefulClose so 1000
  debug $ "spawnConnection exit" <+> pretty sa

  where

    memReqId newP px =
      atomically $ modifyTVar (view tcpPeerXp env) (HashMap.insert newP px)

    makeReqId rcpt = do
      let pxes = view tcpPeerPx  env
      let xpes = view tcpPeerXp  env

      nq <- randomIO
      atomically $ do
        px <- readTVar xpes <&> HashMap.lookup rcpt
        case px of
          Just qq -> pure qq
          Nothing -> do
            modifyTVar pxes (HashMap.insert nq rcpt)
            modifyTVar xpes (HashMap.insert rcpt nq)
            pure nq

    updatePeer connId newP = atomically $ do
      modifyTVar (view tcpPeerConn env) (HashMap.insert newP connId)
      modifyTVar (view tcpConnPeer env) (HashMap.insert connId newP)

    getWriteQueue connId = atomically $ do
      readTVar (view tcpConnQ env) >>= \x -> do
        case HashMap.lookup connId x of
          Just qq -> pure qq
          Nothing -> do
            newQ <- newTQueue
            modifyTVar (view tcpConnQ env) (HashMap.insert connId newQ)
            pure newQ

    cleanupConn connId = atomically do
      modifyTVar (view tcpConnUsed env) (HashMap.alter del connId)
      used <- readTVar (view tcpConnUsed env) <&> HashMap.findWithDefault 0 connId
      when (used == 0) do
        p <- stateTVar (view tcpConnPeer env)
                $ \x -> (HashMap.lookup connId x, HashMap.delete connId x)

        maybe1 p none $ \pp ->
          modifyTVar (view tcpPeerConn env) (HashMap.delete pp)

        modifyTVar (view tcpConnQ env) (HashMap.delete connId)

        where
          del = \case
            Nothing -> Nothing
            Just n | n <= 1 -> Nothing
                   | otherwise -> Just (pred n)


connectPeerTCP :: MonadIO m
               => MessagingTCP
               -> Peer L4Proto
               -> m ()

connectPeerTCP env peer = liftIO do
  pa <- toPeerAddr peer
  let (L4Address _ (IPAddrPort (i,p))) = pa

  connect (show i) (show p) $ \(sock, remoteAddr) -> do
    spawnConnection Client env sock remoteAddr
    shutdown sock ShutdownBoth

-- FIXME: link-all-asyncs

runMessagingTCP :: forall m . MonadIO m => MessagingTCP -> m ()
runMessagingTCP env = liftIO do

  own <- toPeerAddr $ view tcpOwnPeer env
  let (L4Address _ (IPAddrPort (i,p))) = own

  let defs = view tcpDefer env

  mon <- async $ forever do
    pause @'Seconds 30
    now <- getTimeCoarse

    -- FIXME: time-hardcode-again
    let expire = filter (\e -> (realToFrac (toNanoSecs (now - fst e)) / 1e9) < 30)
    atomically $ modifyTVar defs
               $ HashMap.mapMaybe
                  $ \es -> let rs = expire es
                           in case rs of
                             [] -> Nothing
                             xs -> Just xs

  con <- async $ forever do

    let ev = view tcpDeferEv env

    -- FIXME: wait-period-hardcode
    void $ race (pause @'Seconds 0.25) (atomically $ readTQueue ev >> flushTQueue ev)

    dePips <- readTVarIO defs <&> HashMap.keys


    forM_ dePips $ \pip -> do
      msgs <- readTVarIO defs <&> HashMap.findWithDefault mempty pip

      unless (L.null msgs) do
        trace $ "DEFERRED FOR"  <+> pretty pip <+> pretty (length msgs)

        let len = length msgs

        when (len > 10) do
          -- FIXME: deferred-message-hardcoded
          atomically $ modifyTVar defs (HashMap.adjust (L.drop (len - 10)) pip)

        co' <- atomically $ readTVar (view tcpPeerConn env) <&> HashMap.lookup pip

        maybe1 co' (void $ async (connectPeerTCP env pip)) $ \co -> do
          q' <- atomically $ readTVar (view tcpConnQ env) <&> HashMap.lookup co
          maybe1 q' none $ \q -> do
            atomically do
              mss <- readTVar defs <&> HashMap.findWithDefault mempty pip
              modifyTVar defs $ HashMap.delete pip
              forM_ mss $ \m -> writeTQueue q (pip, snd m)

        pure ()

  stat <- async $ forever do
    pause @'Seconds 120
    ps <- readTVarIO $ view tcpConnPeer env
    let peers = HashMap.toList ps
    forM_ peers $ \(c,pip) -> do
      used <- readTVarIO (view tcpConnUsed env) <&> HashMap.findWithDefault 0 c
      trace $ "peer" <+> brackets (pretty own)
                    <+> pretty pip
                    <+> pretty c
                    <+> parens ("used:" <+> pretty used)

  mapM_ link [mon,con,stat]

  liftIO (
    listen (Host (show i)) (show p) $ \(sock, sa) -> do
      withFdSocket sock setCloseOnExecIfNeeded
      debug $ "Listening on" <+> pretty sa

      forever do
        void $ acceptFork sock $ \(so, remote) -> do
          withFdSocket so setCloseOnExecIfNeeded
          trace $ "GOT INCOMING CONNECTION FROM"
            <+> brackets (pretty own)
            <+> brackets (pretty sa)
            <+> pretty remote

          void $ try @SomeException $ do

            spawnConnection Server env so remote

            -- gracefulClose so 1000

          -- TODO: probably-cleanup-peer
          -- TODO: periodically-drop-inactive-connections

          debug $ "CLOSING CONNECTION" <+> pretty remote
          shutdown so ShutdownBoth
          close so ) `U.finally` mapM_ cancel [mon,con,stat]


traceCmd :: forall a ann b m . ( Pretty a
                               , Pretty b
                               , MonadIO m
                               )
         => a -> Doc ann -> b -> m ()

traceCmd p1 s p2 = do
  trace $ brackets (pretty p1)
           <+> s
           <+> parens (pretty p2)

