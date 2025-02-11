
{-# Language TemplateHaskell #-}
{-# Language AllowAmbiguousTypes #-}
{-# Language UndecidableInstances #-}
{-# Language MultiWayIf #-}
module Main where

import HBS2.Prelude.Plated

import HBS2.Actors.Peer
import HBS2.Base58
import HBS2.Merkle
import HBS2.Defaults
import HBS2.System.Dir (takeDirectory,(</>))
import HBS2.Events
import HBS2.Hash
import HBS2.Data.Types.Refs
import HBS2.Data.Types.SignedBox
import HBS2.Data.Types
import HBS2.Net.Auth.Credentials
import HBS2.Net.Auth.Schema()
import HBS2.Net.IP.Addr
import HBS2.Net.Messaging.UDP
import HBS2.Net.Messaging.TCP
import HBS2.Net.Messaging.Unix
import HBS2.Net.Messaging.Encrypted.ByPass
import HBS2.Net.PeerLocator
import HBS2.Peer.Proto
import HBS2.Peer.Proto.RefChan qualified as R
import HBS2.Peer.Proto.RefChan.Adapter
import HBS2.Net.Proto.Notify
import HBS2.Peer.Proto.Mailbox
import HBS2.OrDie
import HBS2.Storage.Simple
import HBS2.Storage.Operations.Missed
import HBS2.Data.Detect

import HBS2.KeyMan.Keys.Direct

import HBS2.Version
import Paths_hbs2_peer qualified as Pkg

import Brains
import BrainyPeerLocator
import ByPassWorker
import PeerTypes hiding (info)
import BlockDownloadNew
import CheckBlockAnnounce (checkBlockAnnounce)
import CheckPeer (peerBanned)
import PeerInfo
import PeerConfig
import Bootstrap
import CheckMetrics
import RefLog qualified
import RefLog (reflogWorker)
import LWWRef (lwwRefWorker)
import Multicast
import MailboxProtoWorker
import HttpWorker
import DispatchProxy
import PeerMeta
import Watchdogs
import CLI.Common
import CLI.RefChan
import CLI.LWWRef
import CLI.Mailbox
import RefChan
import RefChanNotifyLog
import Fetch (fetchHash)
import Log hiding (info)

import HBS2.Misc.PrettyStuff
import HBS2.Peer.RPC.Internal.Types()
import HBS2.Peer.RPC.Internal.Storage()

import HBS2.Peer.RPC.API.Storage
import HBS2.Peer.RPC.API.Peer
import HBS2.Peer.RPC.API.RefLog
import HBS2.Peer.RPC.API.RefChan
import HBS2.Peer.RPC.API.LWWRef
import HBS2.Peer.RPC.API.Mailbox
import HBS2.Peer.Notify
import HBS2.Peer.RPC.Client.StorageClient

import HBS2.Peer.Proto.LWWRef.Internal

import RPC2(RPC2Context(..))

import Data.Config.Suckless.Script hiding (optional)
import Data.Config.Suckless.Almost.RPC

import Codec.Serialise as Serialise
import Control.Concurrent (myThreadId)
import Control.Concurrent.STM
import Control.Exception as Exception
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Writer.CPS qualified as W
import Crypto.Saltine (sodiumInit)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString qualified as BS
import Data.Cache qualified as Cache
import Data.Coerce
import Data.Fixed
import Data.List qualified as L
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Either
import Data.Set qualified as Set
import Data.Set (Set)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Time.Clock.POSIX
import Data.Time.Format
import Lens.Micro.Platform as Lens
import Network.Socket
import Options.Applicative
import Prettyprinter.Render.Terminal
import System.Directory
import System.Environment
import System.Exit
import System.IO
import System.Mem
import System.Metrics
import System.Posix.Process
import System.Posix.Signals
import Control.Monad.Trans.Cont


import UnliftIO (MonadUnliftIO(..))
import UnliftIO.Exception qualified as U
-- import UnliftIO.STM
import UnliftIO.Async
import UnliftIO.Concurrent (getNumCapabilities)

import Streaming.Prelude qualified as S

import Graphics.Vty qualified as Vty
import Graphics.Vty.Input qualified as Vty
import Graphics.Vty.Input hiding (Event)
import Graphics.Vty (Mode(..),setMode,outputIface,inputIface)
import Graphics.Vty.Platform.Unix qualified as Vty

import Control.Concurrent.Async (ExceptionInLinkedThread(..))


-- TODO: write-workers-to-config
defStorageThreads :: Integral a => a
defStorageThreads = 8


data PeerListenKey
data PeerKeyFileKey
data PeerStorageKey
data PeerDebugKey
data PeerTraceKey
data PeerTrace1Key
data PeerProxyFetchKey
data PeerTcpSOCKS5
data PeerDownloadThreadKey


instance HasCfgKey PeerDebugKey a where
  key = "debug"

instance HasCfgKey PeerTraceKey a where
  key = "trace"

instance HasCfgKey PeerTrace1Key a where
  key = "trace1"

instance HasCfgKey PeerListenKey (Maybe String) where
  key = "listen"

instance HasCfgKey PeerKeyFileKey (Maybe String) where
  key = "key"

instance HasCfgKey PeerStorageKey (Maybe String) where
  key = "storage"

instance HasCfgKey PeerProxyFetchKey (Set String) where
  key = "proxy-fetch-for"

-- NOTE: socks5-auth
--   Network.Simple.TCP does not support
--   SOCKS5 authentification
instance HasCfgKey PeerTcpSOCKS5 (Maybe String) where
  key = "tcp.socks5"

instance HasCfgKey PeerDownloadThreadKey (Maybe Int) where
  key = "download-threads"

data PeerOpts =
  PeerOpts
  { _storage       :: Maybe StoragePrefix
  , _listenOn      :: Maybe String
  , _peerCredFile  :: Maybe FilePath
  , _peerConfig    :: Maybe FilePath
  , _peerRespawn   :: Bool
  }
  deriving stock (Data)

makeLenses 'PeerOpts

main :: IO ()
main = do

  sodiumInit

  setLogging @INFO   defLog
  setLogging @ERROR  errorPrefix
  setLogging @WARN   warnPrefix
  setLogging @NOTICE noticePrefix

  setLoggingOff @TRACE
  setLoggingOff @TRACE1

  withSimpleLogger runCLI



data GOpts =
  GOpts
  { goDebug :: Bool
  , goTrace :: Bool
  }

runCLI :: IO ()
runCLI = do

  (g, cmd) <- customExecParser (prefs showHelpOnError) $
            info (helper <*> parser)
            (  fullDesc
            <> header "hbs2-peer daemon"
            <> progDesc "serves HBS2 protocol"
            )

  withOpts cmd g

  where


    parser ::  Parser (GOpts,IO ())
    parser = do

      (,) <$> ( GOpts <$> switch (long "debug" <> short 'd' <> help "debug mode on")
                      <*> switch (long "trace" <> help "trace on" )
              )
          <*> hsubparser (
                   command "init"      (info pInit (progDesc "creates default config"))
                <> command "run"       (info pRun  (progDesc "run peer"))
                <> command "poke"      (info pPoke (progDesc "poke peer by rpc"))
                <> command "die"       (info pDie (progDesc "die cmd"))
                <> command "announce"  (info pAnnounce (progDesc "announce block"))
                <> command "ping"      (info pPing (progDesc "ping another peer"))
                <> command "fetch"     (info pFetch (progDesc "fetch block"))
                <> command "reflog"    (info pRefLog (progDesc "reflog commands"))
                <> command "refchan"   (info pRefChan (progDesc "refchan commands"))
                <> command "lwwref"    (info pLwwRef (progDesc "lwwref commands"))
                <> command "mailbox"   (info pMailBox (progDesc "mailbox commands"))
                <> command "peers"     (info pPeers (progDesc "show known peers"))
                <> command "pexinfo"   (info pPexInfo (progDesc "show pex"))
                <> command "download"  (info pDownload  (progDesc "download management"))
                <> command "poll"      (info pPoll  (progDesc "polling management"))
                <> command "log"       (info pLog   (progDesc "set logging level"))
                <> command "bypass"    (info pByPass (progDesc "bypass"))
                <> command "gc"        (info pRunGC (progDesc "run RAM garbage collector"))
                <> command "probes"    (info pRunProbes (progDesc "show probes"))
                <> command "do"        (info pDoScript (progDesc "execute a command in peer context"))
                <> command "version"   (info pVersion (progDesc "show program version"))
                )

    common = do
      pref <- optional $ strOption ( short 'p' <> long "prefix"
                                               <> help "storage prefix" )

      l    <- optional $ strOption ( short 'l' <> long "listen"
                                    <> help "addr:port" )

      k    <- optional $ strOption ( short 'k' <> long "key"
                                    <> help "peer keys file" )

      c <- optional confOpt

      resp <- (optional $ flag' True ( long "no-respawn" <> short 'R' <> help "NO respawn"))
                     <&> isNothing

      -- NOTE: respawn-by-default-now
      pure $ PeerOpts pref l k c resp

    withOpts m g = do

      when (goDebug g) do
        setLogging @DEBUG ( debugPrefix  . toStderr )

      when (goTrace g) do
        setLogging @TRACE ( tracePrefix  . toStderr )

      m

    pVersion = pure do
        LBS.putStr $ Aeson.encode $(inlineBuildVersion Pkg.version)

    pPubKeySign = maybeReader (fromStringMay @(PubKey 'Sign 'HBS2Basic))

    pRun = do
      runPeer <$> common

    pDie = do
      rpc <- pRpcCommon
      pure $ withMyRPC @PeerAPI rpc $ \caller -> do
        l <- async $ void $ callService @RpcDie caller ()
        pause @'Seconds 0.25
        cancel l

    pPoke = do
      rpc <- pRpcCommon
      pure $ withMyRPC @PeerAPI rpc $ \caller -> do
        r <- callRpcWaitRetry @RpcPoke (TimeoutSec 0.5) 2 caller ()
        case r of
          Nothing -> exitFailure
          Just s -> putStrLn s >> exitSuccess

    pAnnounce = do
      rpc <- pRpcCommon
      h   <- strArgument ( metavar "HASH" )
      pure $ withMyRPC @PeerAPI rpc $ \caller -> do
        void $ callService @RpcAnnounce caller h

    pFetch = do
      rpc <- pRpcCommon
      pro   <- optional (switch (short 'p' <> long "progress" <> help "display progress"))
                 <&> fromMaybe False
      h   <- strArgument ( metavar "HASH" )

      pure $ flip runContT pure do


        client <- ContT $ withRPCMessaging rpc

        self <- runReaderT (ownPeer @UNIX) client
        -- refChanAPI  <- makeServiceCaller @RefChanAPI self
        peerAPI  <- makeServiceCaller @PeerAPI self
        storageAPI  <- makeServiceCaller @StorageAPI self
        let sto = AnyStorage (StorageClient storageAPI)

        let endpoints = [ Endpoint @UNIX peerAPI
                        , Endpoint @UNIX storageAPI
                        ]

        void $ ContT $ bracket (async $ runReaderT (runServiceClientMulti endpoints) client) cancel

        t0 <- getTimeCoarse

        void $ callService @RpcFetch peerAPI h

        liftIO do

          when pro $ flip runContT pure do
            cfg <- pure $ Vty.defaultConfig
            vty <- ContT $ bracket (Vty.mkVty cfg) Vty.shutdown

            let input  = inputIface vty & Vty.eventChannel

            poller <- ContT $ withAsync $ fix \next -> do
              miss <- findMissedBlocks sto h

              let l = length miss
              t1 <- getTimeCoarse
              let elapsed = toNanoSeconds (TimeoutTS (t1 - t0))
                               & realToFrac @_ @Double
                               & (/1e9)
                               & realToFrac @_ @(Fixed E3)
                               & showFixed True

              let msg = show $
                             "fetch tree:"   <+> pretty h <> line
                          <> "blocks left:"  <+> pretty l <> line
                          <> "time elapsed:" <+> pretty elapsed

              let pic = Vty.picForImage $ Vty.string Vty.defAttr msg
              liftIO $ Vty.update vty pic

              when (l > 0) do
                pause @'Seconds 2
                next

            let exitNicely = cancel poller

            inp <- ContT $ withAsync $ forever do
              atomically (readTChan input) >>= \case
                InputEvent (EvKey (KChar 'c') [MCtrl]) -> exitNicely
                InputEvent (EvKey (KChar 'q') [])      -> exitNicely
                _ -> none

            void $ waitAnyCatchCancel [poller, inp]
            liftIO $ Vty.shutdown vty

    pPing = do
      rpc <- pRpcCommon
      h   <- strArgument ( metavar "ADDR" )
      pure $ withMyRPC @PeerAPI rpc $ \caller -> do
        callService @RpcPing caller h >>= \case
          Left e -> err (viaShow e)
          Right True  -> putStrLn "pong"
          Right False -> putStrLn "pang"

    pPeers = do
      rpc <- pRpcCommon
      pure $ withMyRPC @PeerAPI rpc $ \caller -> do
        r <- callRpcWaitRetry @RpcPeers (TimeoutSec 1) 2 caller ()
                >>= orThrowUser "rpc timeout"
        liftIO $ print $ vcat (fmap fmt r)
      where
        fmt (a, b) = pretty (AsBase58 a) <+> pretty b

    pPexInfo = do
      rpc <- pRpcCommon
      pure $ withMyRPC @PeerAPI rpc $ \caller -> do
        r <- callService @RpcPexInfo caller ()
        case r of
          Left e -> err (viaShow e)
          Right p -> do
            print $ vcat (fmap pretty p)

    onOff l =
            hsubparser ( command "on" (info (pure (l True) ) (progDesc "on")  ) )
      <|>   hsubparser ( command "off" (info (pure (l False) ) (progDesc "off")  ) )

    pLog = do
      rpc <- pRpcCommon
      setlog <- hsubparser ( command "trace"  (info (onOff TraceOn) (progDesc "set trace")  ) )
                  <|>
                hsubparser ( command "debug"  (info (onOff DebugOn) (progDesc "set debug")  ) )

      pure $ withMyRPC @PeerAPI rpc $ \caller -> do
        void $ callService @RpcLogLevel caller setlog

    pInit = do
      pref <- optional $ strArgument ( metavar "DIR" )
      pure $ peerConfigInit pref

    pRefLog = hsubparser (  command "send" (info pRefLogSend (progDesc "send reflog transaction" ))
                         <> command "send-raw" (info pRefLogSendRaw (progDesc "send reflog raw transaction" ))
                         <> command "fetch" (info pRefLogFetch (progDesc "fetch reflog from all" ))
                         <> command "get"   (info pRefLogGet (progDesc "get own reflog from all" ))
                         <> command "cat"   (info pRefLogCat (progDesc "dump decoded reflog transcations"))
                         )

    pRefLogSend = do
      rpc <- pRpcCommon
      pk <- argument pPubKeySign (metavar "REFLOG-KEY")

      pure $ withMyRPC @RefLogAPI rpc $ \caller -> do

        creds <- runKeymanClient $ loadCredentials pk
                  >>= orThrowUser "can't find credentials"

        bs <- BS.take defChunkSize  <$> BS.hGetContents stdin
        let pubk = view peerSignPk creds
        let privk = view peerSignSk creds
        msg <- makeRefLogUpdate @L4Proto pubk privk bs
        void $ callService @RpcRefLogPost caller msg

    pRefLogSendRaw = do
      rpc <- pRpcCommon
      pure $ withMyRPC @RefLogAPI rpc $ \caller -> do
        bs <- LBS.take defChunkSize  <$> LBS.hGetContents stdin
        msg <- pure (deserialiseOrFail @(RefLogUpdate L4Proto) bs) `orDie` "Invalid reflog transaction"
        void $ callService @RpcRefLogPost caller msg

    pRefLogFetch = do
      rpc <- pRpcCommon
      ref <- strArgument ( metavar "REFLOG-KEY" )
      pure $ withMyRPC @RefLogAPI rpc $ \caller -> do
        href <- pure (fromStringMay ref) `orDie` "invalid REFLOG-KEY"
        void $ callService @RpcRefLogFetch caller href

    pRefLogGet = do
      rpc <- pRpcCommon
      ref <- strArgument ( metavar "REFLOG-KEY" )
      pure $ withMyRPC @RefLogAPI rpc $ \caller -> do
        href <- pure (fromStringMay ref) `orDie` "invalid REFLOG-KEY"
        callService @RpcRefLogGet caller href >>= \case
          Left{} -> exitFailure
          Right Nothing -> exitFailure
          Right (Just h) -> print (pretty h) >> exitSuccess

    pRefLogCat = do
      rpc <- pRpcCommon
      ref <- strArgument ( metavar "REFLOG-KEY" )

      pure $ flip runContT pure do

        client <- ContT $ withRPCMessaging rpc

        self         <- runReaderT (ownPeer @UNIX) client
        refLogAPI    <- makeServiceCaller @RefLogAPI self
        peerAPI      <- makeServiceCaller @PeerAPI self
        storageAPI   <- makeServiceCaller @StorageAPI self
        let sto = AnyStorage (StorageClient storageAPI)

        let endpoints = [ Endpoint @UNIX peerAPI
                        , Endpoint @UNIX refLogAPI
                        , Endpoint @UNIX storageAPI
                        ]

        void $ ContT $ bracket (async $ runReaderT (runServiceClientMulti endpoints) client) cancel


        href <- pure (fromStringMay ref) `orDie` "invalid REFLOG-KEY"

        rv' <- lift $ callRpcWaitMay @RpcRefLogGet (TimeoutSec 1) refLogAPI href
                >>= orThrowUser "rpc calling error"

        rv <- ContT $ maybe1 rv' none

        walkMerkle @[HashRef] (fromHashRef rv) (getBlock sto) $ \case
          Left h -> liftIO $ throwIO (userError $ show $ "missed block:" <+> pretty h)
          Right hrs -> do
            for_ hrs $ \h -> void $ runMaybeT do
              s <- getBlock sto (fromHashRef h)
                     >>= toMPlus
                     <&> deserialiseOrFail @(RefLogUpdate L4Proto)
                     >>= toMPlus

              guard =<< verifyRefLogUpdate s
              liftIO $ BS.putStr (view refLogUpdData s)

    pPoll = hsubparser (  command "list" (info pPollList (progDesc "list current pollers" ))
                       <> command "add" (info  pPollAdd (progDesc "add poller" ))
                       <> command "del" (info  pPollDel (progDesc "del poller" ))
                       )

    pPollAdd = do
      rpc <- pRpcCommon
      r <- argument refP (metavar "REF")
      t <- strArgument (metavar "TYPE")
      i <- argument auto (metavar "INTERVAL")
      pure $ withMyRPC @PeerAPI rpc $ \caller -> do
        callService @RpcPollAdd caller (r, t, i) >>= \case
          Left e -> die (show e)
          _      -> liftIO do
                       hPutDoc stdout $ "added poller for" <+> pretty (AsBase58 r)
                       exitSuccess

    pPollDel = do
      rpc <- pRpcCommon
      r <- argument refP (metavar "REF")
      pure $ withMyRPC @PeerAPI rpc $ \caller -> do
        callService @RpcPollDel caller r >>= \case
          Left e -> die (show e)
          _      -> liftIO do
                       hPutDoc stdout $ "deleted poller for" <+> pretty (AsBase58 r)
                       exitSuccess

    pPollList = do
      rpc <- pRpcCommon
      -- ref <- strArgument ( metavar "REFLOG-KEY" )
      pure $ withMyRPC @PeerAPI rpc $ \caller -> do
        void $ runMaybeT do
          polls <- toMPlus =<< callService @RpcPollList caller ()
          forM_ polls $ \(r,what,t) -> do
           liftIO $ hPutDoc stdout $ fill 44 (pretty (AsBase58 r))
                                     -- TODO: align-right
                                     <+> fill 3 (pretty t)
                                     <+> pretty what
                                     <> line


    pDownload = hsubparser (  command "list" (info pDownList (progDesc "list current downloads" ))
                           <> command "del" (info pDownDel (progDesc "delete download" ))
                           )

    pDownDel = do
      rpc <- pRpcCommon
      href <- argument hashP ( metavar "HASH-REF")
      pure $ withMyRPC @PeerAPI rpc $ \caller -> do
        void $ runMaybeT do
         toMPlus =<< callService @RpcDownloadDel caller href

    pDownList = do
      rpc <- pRpcCommon
      pure $ withMyRPC @PeerAPI rpc $ \caller -> do
        void $ runMaybeT do
         -- now <- getU
         d <- toMPlus =<< callService @RpcDownloadList caller ()
         now <- liftIO getPOSIXTime
         liftIO $ print $ vcat (fmap (fmt now) d)
         pure ()

      where
        fmt now (h,u) = pretty (AsBase58 h) <+> pretty diff
          where
            delta = now - fromIntegral u
            diff = formatTime defaultTimeLocale "%d:%H:%M:%S" delta

    pByPass = hsubparser (  command "show" (info pByPassShow (progDesc "show bypass info" ))
                         )

    pByPassShow = do
      rpc <- pRpcCommon
      pure $ withMyRPC @PeerAPI rpc $ \caller -> do
        void $ runMaybeT do
         d <- toMPlus =<< callService @RpcByPassInfo caller ()
         liftIO $ print $ pretty d

    pRunGC = do
      rpc <- pRpcCommon
      pure do
        withMyRPC @PeerAPI rpc $ \caller -> do
          void $ runMaybeT do
           void $ callService @RpcPerformGC caller ()

    pRunProbes = do
      rpc <- pRpcCommon
      pure do
        withMyRPC @PeerAPI rpc $ \caller -> do
          void $ runMaybeT do

           p <- lift (callRpcWaitRetry @RpcGetProbes (TimeoutSec 1) 3 caller ())
                  >>= toMPlus

           liftIO $ print $ vcat (fmap pretty p)

    pDoScript = do
      rpc <- pRpcCommon

      w    <- option (auto @Double) ( short 'w'
                                     <> long "timeout"
                                     <> value 1.00
                                     <> help "timeout in seconds (float)"
                                     <> showDefault
                                    )

      argz <- many (strArgument (metavar "TERM" <> help "script terms"))

      pure do
        let s = unlines $ unwords <$> splitForms argz


        withMyRPC @PeerAPI rpc $ \caller -> do

           r <- callRpcWaitRetry @RpcRunScript (TimeoutSec (realToFrac w)) 3 caller (Text.pack s)
                  >>= orThrowUser "rpc timeout"

           for_ (parseTop r & fromRight mempty) \sexy -> do
             liftIO $ hPutDoc stdout (pretty sexy)

    refP :: ReadM (PubKey 'Sign 'HBS2Basic)
    refP = maybeReader fromStringMay

    hashP :: ReadM HashRef
    hashP = maybeReader fromStringMay

myException :: SomeException -> IO ()
myException e = err ( viaShow e )

newtype CredentialsM e s m a =
  CredentialsM { fromCredentials :: ReaderT (PeerCredentials s) m a }
  deriving newtype ( Functor
                   , Applicative
                   , Monad
                   , MonadIO
                   , MonadReader (PeerCredentials s)
                   , MonadTrans)

withCredentials :: forall e s m a . (HasOwnPeer e m, Monad m, s ~ Encryption e)
                => PeerCredentials s
                -> CredentialsM e s m a -> m a

withCredentials pc m = runReaderT (fromCredentials m) pc


instance (Monad m, HasTimeLimits e p m, s ~ Encryption e) => HasTimeLimits  e p (CredentialsM e s m) where
  tryLockForPeriod p m = lift $ tryLockForPeriod p m

instance (HasOwnPeer e m) => HasOwnPeer e (CredentialsM e s m) where
  ownPeer = lift ownPeer

instance (Monad m, HasFabriq e m, s ~ Encryption e) => HasFabriq e (CredentialsM e s m)  where
  getFabriq = lift getFabriq

instance (Sessions e p m, s ~ Encryption e) => Sessions e p (CredentialsM e s m)  where
  find k f = lift (find k f)
  fetch i d k f = lift (fetch i d k f)
  update d k f = lift (update d k f)
  expire k = lift (expire  k)

instance (Monad m, HasPeerNonce e m, s ~ Encryption e) => HasPeerNonce e (CredentialsM e s m) where
  peerNonce = lift $ peerNonce @e

instance (Monad m, s ~ Encryption e) => HasCredentials s (CredentialsM e s m) where
  getCredentials = ask

instance (Monad m, s ~ Encryption e) => HasCredentials s (ResponseM e (CredentialsM e s m)) where
  getCredentials = lift getCredentials

instance (Monad m, HasThatPeer p e m, s ~ Encryption e) => HasThatPeer p e (CredentialsM e s m) where
  thatPeer = lift (thatPeer @p)

instance ( EventEmitter e p m
         ) => EventEmitter e p (CredentialsM e s m) where

  emit k d = lift $ emit k d

instance ( Monad m
         , Response e p m
         , s ~ Encryption e
         ) => Response e p (CredentialsM e s m) where

  response = lift . response

instance  (MonadUnliftIO m, HasProtocol UNIX (NotifyProto ev e)) => HasDeferred (NotifyProto ev e) UNIX m where
  deferred m = void $ async m

respawn :: PeerOpts -> IO ()
respawn opts =
  if not (view peerRespawn opts) then do
    exitFailure
  else do
    let secs = 5
    notice $ "RESPAWNING in" <+> viaShow secs <> "s"
    pause @'Seconds secs
    self <- getExecutablePath
    args <- getArgs
    print (self, args)
    executeFile self False args Nothing

respawnOnError :: PeerOpts -> IO () -> IO ()
respawnOnError opts act =
  Exception.try act >>= match
  where match (Right r) = return r
        match (Left e)
          | Just (ExceptionInLinkedThread _ e') <- Exception.fromException e = do
             match (Left e')
          | Just ec <- Exception.fromException @ExitCode e =
            notice $ viaShow ec
          | Just UserInterrupt <- Exception.fromException e =
            notice "Interrupted by user"
          | otherwise =
            myException e >> performGC >> respawn opts

runPeer :: forall e s . ( e ~ L4Proto
                        , FromStringMaybe (PeerAddr e)
                        , s ~ Encryption e
                        -- , ForLWWRefProto e
                        -- , Serialise (PubKey 'Sign (Encryption e))
                        , HasStorage (PeerM e IO)
                        )=> PeerOpts -> IO ()

runPeer opts = respawnOnError opts $ do

  probes <- liftIO $ newTVarIO (mempty :: [AnyProbe])

  myself <- liftIO myThreadId

  metrics <- liftIO newStore

  xdg <- liftIO $ getXdgDirectory XdgData defStorePath <&> fromString

  conf@(PeerConfig syn) <- peerConfigRead (view peerConfig opts)

  liftIO $ print $ pretty conf

  let listenConf      = runReader (cfgValue @PeerListenKey)  syn
  let keyConf         = runReader (cfgValue @PeerKeyFileKey) syn
  let storConf        = runReader (cfgValue @PeerStorageKey) syn <&> StoragePrefix
  let traceConf       = runReader (cfgValue @PeerTraceKey)   syn
  let debugConf       = runReader (cfgValue @PeerDebugKey)   syn :: FeatureSwitch
  let trace1Conf      = runReader (cfgValue @PeerTrace1Key)  syn :: FeatureSwitch
  let helpFetchKeys   = runReader (cfgValue @PeerProxyFetchKey) syn & toKeys
  let tcpListen       = runReader (cfgValue @PeerListenTCPKey) syn & fromMaybe ""
  let tcpProbeWait    = runReader (cfgValue @PeerTcpProbeWaitKey) syn
                          & fromInteger @(Timeout 'Seconds) . fromMaybe 300

  let rpc = getRpcSocketName conf

  let
    addProbe :: forall m . MonadIO m => AnyProbe -> m ()
    addProbe p = liftIO $ atomically $ modifyTVar probes (p:)

  -- let downloadThreadNum = runReader (cfgValue @PeerDownloadThreadKey) syn & fromMaybe 1

  let useSocks5  = runReader (cfgValue @PeerTcpSOCKS5) syn

  let listenSa = view listenOn opts <|> listenConf <|> Just "0.0.0.0:7351"

  credFile <- pure (view peerCredFile opts <|> keyConf) `orDie` "credentials not set"

  let pref = view storage opts <|> storConf <|> Just xdg

  debug $ "storage prefix:" <+> pretty pref

  liftIO $ print $ pretty "debug: " <+> pretty (show debugConf)
  liftIO $ print $ pretty "trace: " <+> pretty (show traceConf)
  liftIO $ print $ pretty "trace1: " <+> pretty (show trace1Conf)

  when (traceConf == FeatureOn) do
    setLogging @TRACE tracePrefix
    setLogging @DEBUG debugPrefix

  when (debugConf == FeatureOn) do
    setLogging @DEBUG debugPrefix

  when (trace1Conf == FeatureOn) do
    setLogging @TRACE1 tracePrefix


  refChanNotifySource <- newSomeNotifySource @(RefChanEvents L4Proto)
  refLogNotifySource  <- newSomeNotifySource @(RefLogEvents L4Proto)

  let ps = mempty

  pc' <- liftIO $ LBS.readFile credFile
            <&> parseCredentials @(Encryption e) . AsCredFile
                                                 . LBS.toStrict
                                                 . LBS.take 4096

  pc <- pure pc' `orDie` "can't parse credential file"

  notice $ "run peer" <+> pretty (AsBase58 (view peerSignPk pc))

  s <- simpleStorageInit @HbSync (Just pref)
  let blk = liftIO . hasBlock s

  stoProbe <- newSimpleProbe "StorageSimple"
  simpleStorageSetProbe s stoProbe
  addProbe stoProbe

  stn <- getNumCapabilities <&> max 2 . div 4

  w <- replicateM stn $ async $ liftIO $ simpleStorageWorker s

  localMulticast <- liftIO $ (headMay <$> parseAddrUDP (fromString defLocalMulticast)
                                      <&> fmap (fromSockAddr @'UDP . addrAddress) )

                           `orDie` "assertion: localMulticastPeer not set"

  brains <- newBasicBrains @e conf

  bProbe <- newSimpleProbe "Brains"
  addProbe bProbe
  basicBrainsSetProbe brains bProbe

  brainsThread <- async $ runBasicBrains conf brains


  pl <- AnyPeerLocator <$> newBrainyPeerLocator @e (SomeBrains @e brains) mempty

  -- FIXME: messaing-watchdog
  --    Раз уж мы не помирает в случае, если один
  --    из месседжингов отвалился --- то нужно
  --    сделать watchdog, который будет респавнить
  --    всё, если нет ни одного живого месседжинга

  msgAlive <- liftIO $ newTVarIO 0

  messWatchDog <- liftIO $ async do
    pause @'Seconds 5
    fix \next -> do
      alive <- readTVarIO msgAlive
      if alive <= 0 then do
          err "!!! No live messaging left. Trying to respawn"
          pure ()
      else do
        pause @'Seconds 2
        next

  udpPoint <- runMaybeT do
                sa <- toMPlus listenSa
                env <- toMPlus =<< liftIO (newMessagingUDP False (Just sa))

                void $ liftIO ( async do
                                  runMessagingUDP env
                                    `U.withException` \(e :: SomeException) -> do
                                    err (viaShow e)
                                    err "!!! UDP messaging stopped"
                                    liftIO $ atomically $ modifyTVar msgAlive pred
                              )

                let udpAddr = getOwnPeer env
                liftIO $ atomically $ modifyTVar msgAlive succ
                pure (env, (udpAddr, Dispatched env))

  tcpPoint <- runMaybeT do

                addr <- toMPlus $ fromStringMay @(PeerAddr L4Proto) tcpListen
                                     <|> Just "tcp://0.0.0.0:10351"

                let socks5 = useSocks5 >>= fromStringMay @(PeerAddr L4Proto)

                tcpEnv <- newMessagingTCP addr
                             <&> set tcpOnClientStarted (onClientTCPConnected brains)
                             <&> set tcpSOCKS5 socks5

                lift do
                  tcpProbe <- newSimpleProbe "MessagingTCP"
                  addProbe tcpProbe
                  messagingTCPSetProbe tcpEnv tcpProbe

                void $ liftIO ( async do
                          runMessagingTCP tcpEnv
                            `U.withException` \(e :: SomeException) -> do
                            err (viaShow e)
                            err "!!! TCP messaging stopped"
                            liftIO $ atomically $ modifyTVar msgAlive pred
                       )
                let tcpaddr = view tcpOwnPeer tcpEnv
                liftIO $ atomically $ modifyTVar msgAlive succ
                pure (tcpEnv, (tcpaddr, Dispatched tcpEnv))

  let tcp = fst <$> tcpPoint

  let mtcp = view (_2 . _2) <$> tcpPoint
  let mudp = view (_2 . _2) <$> udpPoint

  let points = catMaybes [ snd <$> udpPoint
                         , snd <$> tcpPoint
                         ]

  proxy <- newDispatchProxy points $ \_ pip -> case view sockType pip of
             TCP -> pure mtcp
             UDP -> pure mudp

  -- TODO: get-rid-of-from-addr
  --   From addres в Messaging -- пережиток,
  --   ни на что не влияет, ни для чего не нужен.
  --   Таскается везде со времени, когда Messaging был
  --   через TQueue. Нужно его удалить повсеместно
  --   Или сделать некий AnyAddr/DefaultAddr
  peerSelf <- fromPeerAddr "0.0.0.0:7351"
  byPassProbe <- newSimpleProbe "Messaging.Encrypted.ByPass"
  addProbe byPassProbe
  byPass <- newByPassMessaging @L4Proto
                          byPassDef
                          proxy
                          peerSelf
                          (view peerSignPk pc)
                          (view peerSignSk pc)

  byPassMessagingSetProbe byPass byPassProbe

  penv <- newPeerEnv pl (AnyStorage s) (Fabriq byPass) peerSelf
  do
    probe <- newSimpleProbe "PeerEnv_Main"
    addProbe probe
    peerEnvSetProbe penv probe
  probesPenv <- liftIO $ async $ forever do
      pause @'Seconds 10
      peerEnvCollectProbes penv

  proxyThread <- async $ runDispatchProxy proxy

  let peerMeta = mkPeerMeta conf penv

  nbcache <- liftIO $ Cache.newCache (Just $ toTimeSpec ( 600 :: Timeout 'Seconds))

  void $ async $ forever do
    pause @'Seconds 600
    liftIO $ Cache.purgeExpired nbcache

  rce <- refChanWorkerEnv conf penv refChanNotifySource

  rcwProbe <- newSimpleProbe "RefChanWorker"
  addProbe rcwProbe
  refChanWorkerEnvSetProbe rce rcwProbe

  let refChanAdapter =
        RefChanAdapter
        { refChanOnHead = refChanOnHeadFn rce
        , refChanSubscribed = isPolledRef @e brains "refchan"
        , refChanWriteTran = refChanWriteTranFn rce
        , refChanValidatePropose = refChanValidateTranFn @e rce
        -- TODO: inject-refchanUpdateNotifyCallback
        , refChanNotifyRely = \r u -> do
           trace "refChanNotifyRely!"
           refChanNotifyRelyFn @e rce r u
           case u of
             R.Notify rr x -> do
               emitNotify refChanNotifySource (RefChanNotifyKey r, RefChanNotifyData rr x)
             _ -> pure ()

        -- , refChanEmitRefChanUpdated = \rchan val -> do
        --     emitNotify refChanNotifySource (RefChanNotifyKey rchan, RefChanUpdated rchan val)

        -- , refChanEmitRefChanHeadUpdated = \rchan old val -> do
        --     emitNotify refChanNotifySource (RefChanNotifyKey rchan, RefChanHeadUpdated rchan old val)
        --     pure ()
        }

  rcw <- async $ liftIO $ runRefChanRelyWorker rce refChanAdapter

  mailboxWorker <- createMailboxProtoWorker pc  penv (AnyStorage s)

  p <- newSimpleProbe "MailboxProtoWorker"
  mailboxProtoWorkerSetProbe mailboxWorker p
  addProbe p

  let onNoBlock (p, h) = do
        already <- liftIO $ Cache.lookup nbcache (p,h) <&> isJust
        unless already do
          mpde <- find (KnownPeerKey p) id
          maybe1 mpde none $ \pd -> do
            let pk = view peerSignKey pd
            when (Set.member pk helpFetchKeys) do
              liftIO $ Cache.insert nbcache (p,h) ()
              -- debug  $ "onNoBlock" <+> pretty p <+> pretty h
              liftIO $ withPeerM penv $ addDownload @e mzero h

  loop <- liftIO $ async do

            runPeerM penv $ do
              adapter <- mkAdapter

              reflogReqAdapter <- RefLog.mkRefLogRequestAdapter @e (SomeBrains brains)

              let doDownload h = do
                    pro <- isReflogProcessed @e brains h
                    if pro then do
                      withPeerM penv $ addDownload @e mzero h
                    else do
                      -- FIXME: separate-process-to-mark-logs-processed
                      withPeerM penv $ addDownload @e Nothing h

              let doFetchRef puk = do
                   withPeerM penv $ do
                     forKnownPeers @e $ \p _ -> do
                       request p (RefLogRequest @e puk)

              let rwa = RefLog.RefLogWorkerAdapter
                   { RefLog.reflogDownload = doDownload
                   , RefLog.reflogFetch = doFetchRef
                   , RefLog.reflogUpdated = \(r,v) -> do
                        debug $ "EMIT REFLOG UPDATE" <+> pretty (AsBase58 r)
                        emitNotify refLogNotifySource ( RefLogNotifyKey @L4Proto r
                                                      , RefLogUpdateNotifyData @L4Proto r (HashRef v)
                                                      )
                   }

              let addNewRtt (p,rttNew) = withPeerM penv $ void $ runMaybeT do
                    def <- newPeerInfo
                    tv <- lift $ fetch True def (PeerInfoKey p) (view peerRTTBuffer)
                    insertRTT rttNew tv

              let hshakeAdapter = PeerHandshakeAdapter addNewRtt

              env <- ask

              pnonce <- peerNonce @e

              pl <- getPeerLocator @e

              addPeers @e pl ps

              -- subscribe @e (BlockSizeEventKey h) $ \case
              --   BlockSizeEvent (that, hx, sz) -> do
              --     debug $ "FUCKING GOT BLOCK SIZE!" <+> pretty (AsBase58 hx) <+> pretty sz
                  -- atomically $ writeTQueue answ (sz, that)

              subscribe @e PeerAnnounceEventKey $ \(PeerAnnounceEvent pip nonce) -> do
               unless (nonce == pnonce) $ do
                 debug $ "Got peer announce!" <+> pretty pip
                 mpd :: Maybe (PeerData e) <- find (KnownPeerKey pip) id
                 banned <- maybe (pure False) (peerBanned conf) mpd
                 let known = isJust mpd && not banned
                 sendPing pip

              subscribe @e BlockAnnounceInfoKey $ \(BlockAnnounceEvent p bi no) -> do
                 pa <- toPeerAddr p
                 checkBlockAnnounce conf penv no pa (view biHash bi)

              subscribe @e AnyKnownPeerEventKey $ \(KnownPeerEvent p pd) -> do

                let thatNonce = view peerOwnNonce pd

                now <- liftIO getTimeCoarse

                -- defPeerInfo <- newPeerInfo
                -- fetch True defPeerInfo (PeerInfoKey p) id >>= \pinfo -> do

                find (PeerInfoKey p) id >>= mapM_ \pinfo -> do
                      liftIO $ atomically $ writeTVar (view peerPingFailed pinfo) 0
                      liftIO $ atomically $ writeTVar (view peerLastWatched pinfo) now

                banned <- peerBanned conf pd

                let doAddPeer p = do
                      addPeers pl [p]

                      -- TODO: better-handling-for-new-peers
                      npi    <- newPeerInfo

                      here <- find @e (KnownPeerKey p) id <&> isJust

                      unless here do
                        debug $ "Got authorized peer!" <+> pretty p
                                                       <+> pretty (AsBase58 (view peerSignKey pd))
                        request @e p (GetPeerMeta @e)


                -- FIXME: check if we've got a reference to ourselves
                if | pnonce == thatNonce -> do
                    debug $ "GOT OWN NONCE FROM" <+> pretty p
                    delPeers pl [p]
                    addExcluded pl [p]
                    expire (KnownPeerKey p)

                   | banned -> do
                       notice $ pretty p <+> "banned"

                   | otherwise -> do

                    update pd (KnownPeerKey p) id

                    pdkv :: Map BS.ByteString (Peer L4Proto) <- fmap (Map.fromList . catMaybes)
                        $ knownPeers @e pl >>= mapM \pip ->
                              fmap (, pip) <$> find (KnownPeerKey pip) (view peerOwnNonce)

                    case Map.lookup thatNonce pdkv of

                      -- FIXME: move-protocol-comparison-to-peer-nonce
                      --

                      Nothing -> doAddPeer p

                      Just p0 -> do

                        pa0 <- toPeerAddr p0
                        pa1 <- toPeerAddr p

                        if | pa0 == pa1 -> none
                           | view sockType p0 /= view sockType p -> do
                              doAddPeer p

                           | otherwise -> do

                              debug "Same peer, different address"

                              void $ runMaybeT do

                                pinfo0 <- MaybeT $ find (PeerInfoKey p0) id
                                pinfo1 <- MaybeT $ find (PeerInfoKey p) id

                                rtt0 <- MaybeT $ medianPeerRTT pinfo0
                                rtt1 <- MaybeT $ medianPeerRTT pinfo1

                                when ( rtt1 < rtt0 ) do
                                  debug $ "Better rtt!" <+> pretty p0
                                                        <+> pretty p
                                                        <+> pretty rtt0
                                                        <+> pretty rtt1

                                  lift $ do
                                    expire (KnownPeerKey p0)
                                    delPeers pl [p0]
                                    -- addExcluded pl [p0]
                                    doAddPeer p


              void $ liftIO $ async $ withPeerM env do
                pause @'Seconds 3
                debug "sending first peer announce"
                request localMulticast (PeerAnnounce @e pnonce)

              let peerThread t mx = ContT $ withAsync $ liftIO $
                                      withPeerM env mx
                                       `U.withException` \e -> case fromException e of
                                          Just (_ :: AsyncCancelled) -> pure ()
                                          Nothing -> do
                                            err $ red "Exception" <+> "in thread" <+> pretty t <+> viaShow e
                                            liftIO $ throwTo myself GoAgainException


              let lwwRefProtoA = lwwRefProto (LWWRefProtoAdapter { lwwFetchBlock = download })
                   where download h = liftIO $ withPeerM env $ addDownload @e Nothing h

              flip runContT pure do

                mcastProbe <- newSimpleProbe "PeerEnv_Announce"
                addProbe mcastProbe

                peerThread "multicastWorker" $ multicastWorker conf env mcastProbe

                peerThread "byPassWorker" (byPassWorker byPass)

                peerThread "httpWorker" (httpWorker conf peerMeta)

                metricsProbe <- newSimpleProbe "ghc.runtime"
                addProbe metricsProbe

                peerThread "checkMetrics" (checkMetrics metrics metricsProbe)

                t0 <- getTimeCoarse

                peerThread "monotonicTime" $ forever do
                  pause @'Seconds 10
                  tx <- getTimeCoarse
                  acceptReport metricsProbe [("monotonicTime", fromIntegral (tx-t0) `div` round 1e9)]

                peerThread "peerPingLoop" (peerPingLoop @e conf penv)

                peerThread "knownPeersPingLoop" (knownPeersPingLoop @e conf (SomeBrains brains))

                peerThread "bootstrapDnsLoop" (bootstrapDnsLoop @e conf)

                peerThread "pexLoop" (pexLoop @e brains tcp)

                downloadProbe <- newSimpleProbe "Download"
                addProbe downloadProbe

                -- FIXME: new-download-loop
                peerThread "downloadDispatcher" (downloadDispatcher downloadProbe (SomeBrains brains) env)

                peerThread "fillPeerMeta" (fillPeerMeta tcp tcpProbeWait)

                peerThread "reflogWorker" (reflogWorker @e conf (SomeBrains brains) rwa)

                peerThread "refChanWorker" (refChanWorker @e rce (SomeBrains brains))

                peerThread "refChanNotifyLogWorker" (refChanNotifyLogWorker @e conf (SomeBrains brains))

                peerThread "lwwRefWorker" (lwwRefWorker @e conf (SomeBrains brains))

                -- setup mailboxes stuff
                let defConf = coerce conf
                let mboxConf = maybe1 pref defConf $ \p -> do
                       let mboxDir = takeDirectory (coerce p) </> "hbs2-mailbox"
                       mkList [mkSym hbs2MailboxDirOpt, mkStr mboxDir] : coerce defConf

                peerThread "mailboxProtoWorker" (mailboxProtoWorker (pure mboxConf) mailboxWorker)

                peerThread "rpcWatchDog" (runRpcWatchDog myself rpc)

                liftIO $ withPeerM penv do
                  runProto @e
                    [ makeResponse (blockSizeProto blk onNoBlock)
                    , makeResponse (blockChunksProto adapter)
                    , makeResponse blockAnnounceProto
                    , makeResponse (withCredentials @e pc . peerHandShakeProto hshakeAdapter penv)
                    , makeResponse peerExchangeProto
                    , makeResponse refLogUpdateProto
                    , makeResponse (refLogRequestProto reflogReqAdapter)
                    , makeResponse (peerMetaProto peerMeta)
                    , makeResponse (refChanHeadProto False refChanAdapter)
                    , makeResponse (refChanUpdateProto False pc refChanAdapter)
                    , makeResponse (refChanRequestProto False refChanAdapter)
                    , makeResponse (refChanNotifyProto False refChanAdapter)
                    -- TODO: change-all-to-authorized
                    , makeResponse ((authorized . subscribed (SomeBrains brains)) lwwRefProtoA)
                    , makeResponse ((authorized . mailboxProto False) mailboxWorker)
                    ]


  let refChanHeadPostAction href = do
        void $ liftIO $ withPeerM penv $ do
          let h = fromHashRef href
          debug $ "rpc.refChanHeadPost" <+> pretty h
          me <- ownPeer @e
          sto <- getStorage

          chunks <- S.toList_ $ do
            deepScan ScanDeep (const none) h (liftIO . getBlock sto) $ \ha -> do
              unless (ha == h) do
                blk1 <- liftIO $ getBlock sto ha
                maybe1 blk1 none S.yield

          let box = deserialiseOrFail @(SignedBox (RefChanHeadBlock e) s) (LBS.concat chunks)

          case box of
            -- FIXME: proper-error-handling
            Left{} -> err $ "can't read head block" <+> pretty h
            Right (SignedBox k _ _) -> do
              let msg = RefChanHead k (RefChanHeadBlockTran (HashRef h))
              refChanNotifyOnUpdated rce k
              runResponseM me $ refChanHeadProto @e True refChanAdapter msg

  let refChanProposeAction (puk, box) = do
        debug $ "rpc.reChanPropose" <+> pretty (AsBase58 puk)
        void $ liftIO $ withPeerM penv $ do
          me <- ownPeer @e
          runMaybeT do
            proposed <- MaybeT $ makeProposeTran @e pc puk box
            lift $ runResponseM me $ refChanUpdateProto @e True pc refChanAdapter (Propose @e puk proposed)

  -- NOTE: moved-to-rpc
  let refChanNotifyAction (puk, box) = do
        emitNotify refChanNotifySource (RefChanNotifyKey puk, RefChanNotifyData puk box)
        void $ liftIO $ withPeerM penv $ do
          me <- ownPeer @e
          runMaybeT do
            lift $ runResponseM me $ refChanNotifyProto @e True refChanAdapter (R.Notify @e puk box)



  let k = view peerSignPk pc

  let http = case runReader (cfgValue @PeerHttpPortKey @PeerHttpPort) syn of
               PeerHttpPort Nothing -> mempty
               PeerHttpPort (Just p)  -> "http-port:" <+> pretty p


  let pokeAnsw = show $ vcat [ "peer-key:" <+> dquotes (pretty (AsBase58 k))
                             , "udp:" <+> dquotes (pretty (fst . snd <$> udpPoint))
                             , "tcp:" <+> dquotes (pretty (fst . snd <$> tcpPoint))
                             , "local-multicast:" <+> dquotes (pretty localMulticast)
                             , "rpc:" <+> dquotes (pretty rpc)
                             , http
                             ]

  let rpcSa = getRpcSocketName conf
  rpcmsg <- newMessagingUnix True 1.0 rpcSa

  rpcProbe <- newSimpleProbe "RPC.MessagingUnix"
  setProbe rpcmsg rpcProbe
  addProbe rpcProbe

  let rpcctx = RPC2Context { rpcSelf = myself
                           , rpcConfig = fromPeerConfig conf
                           , rpcMessaging = rpcmsg
                           , rpcTCP = tcp
                           , rpcPokeAnswer = pokeAnsw
                           , rpcPeerEnv = penv
                           , rpcLocalMultiCast = localMulticast
                           , rpcStorage = AnyStorage s
                           , rpcBrains = SomeBrains brains
                           , rpcByPassInfo = liftIO (getStat byPass)
                           , rpcProbes = probes
                           , rpcDoFetch = liftIO . fetchHash penv
                           , rpcDoRefChanHeadPost = refChanHeadPostAction
                           , rpcDoRefChanPropose = refChanProposeAction
                           , rpcDoRefChanNotify = refChanNotifyAction
                           , rpcMailboxService = AnyMailboxService @s mailboxWorker
                           , rpcMailboxAdapter  = AnyMailboxAdapter @s mailboxWorker
                           }

  m1 <- async $ runMessagingUnix rpcmsg

  rpcProto <- async $ flip runReaderT rpcctx do
    env <- newNotifyEnvServer @(RefChanEvents L4Proto) refChanNotifySource
    envrl <- newNotifyEnvServer @(RefLogEvents L4Proto) refLogNotifySource
    w1 <- asyncLinked $ runNotifyWorkerServer env
    w2 <- asyncLinked $ runNotifyWorkerServer envrl
    wws <- replicateM 1 $ async $ runProto @UNIX
      [ makeResponse (makeServer @PeerAPI)
      , makeResponse (makeServer @RefLogAPI)
      , makeResponse (makeServer @RefChanAPI)
      , makeResponse (makeServer @StorageAPI)
      , makeResponse (makeServer @LWWRefAPI)
      , makeResponse (makeServer @MailboxAPI)
      , makeResponse (makeNotifyServer @(RefChanEvents L4Proto) env)
      , makeResponse (makeNotifyServer @(RefLogEvents L4Proto) envrl)
      ]
    void $ waitAnyCancel (w1 : w2 : wws )

  void $ waitAnyCancel $ w <> [ loop
                              , m1
                              , rpcProto
                              -- , probesMenv
                              -- , ann
                              , probesPenv
                              , proxyThread
                              , brainsThread
                              , messWatchDog
                              ]

  liftIO $ simpleStorageStop s
  pause @'Seconds 1

  -- we want to clean up all resources
  throwIO GoAgainException


