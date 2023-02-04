{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language TemplateHaskell #-}
{-# Language AllowAmbiguousTypes #-}
{-# Language UndecidableInstances #-}
module Main where

import HBS2.Actors.Peer
import HBS2.Base58
import HBS2.Clock
import HBS2.Defaults
import HBS2.Events
import HBS2.Hash
import HBS2.Net.Auth.Credentials
import HBS2.Net.IP.Addr
import HBS2.Net.Messaging.UDP
import HBS2.Net.PeerLocator
import HBS2.Net.Proto
import HBS2.Net.Proto.Definition
import HBS2.Net.Proto.Peer
import HBS2.Net.Proto.PeerAnnounce
import HBS2.Net.Proto.Sessions
import HBS2.OrDie
import HBS2.Prelude.Plated
import HBS2.Storage.Simple

import HBS2.System.Logger.Simple hiding (info)
import HBS2.System.Logger.Simple qualified as Log

import RPC
import BlockDownload
import PeerInfo

import Data.Maybe
import Crypto.Saltine (sodiumInit)
import Data.Function
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception as Exception
import Control.Monad.Reader
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.List qualified  as L
import Data.Text (Text)
import Lens.Micro.Platform
import Network.Socket
import Options.Applicative
import Prettyprinter
import System.Directory
import System.Exit
import System.IO

defStorageThreads :: Integral a => a
defStorageThreads = 4

defListenUDP :: String
defListenUDP = "0.0.0.0:7351"

defRpcUDP :: String
defRpcUDP = "localhost:13331"

defLocalMulticast :: String
defLocalMulticast = "239.192.152.145:10153"

data RPCCommand =
    POKE
  | ANNOUNCE (Hash HbSync)
  | PING (PeerAddr UDP) (Maybe (Peer UDP))
  | CHECK PeerNonce (PeerAddr UDP) (Hash HbSync)
  | FETCH (Hash HbSync)

data PeerOpts =
  PeerOpts
  { _storage       :: Maybe StoragePrefix
  , _listenOn      :: String
  , _listenRpc     :: String
  , _peerCredFile  :: FilePath
  }
  deriving stock (Data)

makeLenses 'PeerOpts

deriving newtype instance Hashable (SessionKey UDP (BlockChunks UDP))
deriving stock instance Eq (SessionKey UDP (BlockChunks UDP))

main :: IO ()
main = do

  sodiumInit

  setLogging @DEBUG  (set loggerTr ("[debug] " <>))
  setLogging @INFO   (set loggerTr ("[info] " <>))
  setLogging @ERROR  (set loggerTr ("[error] " <>))
  setLogging @WARN   (set loggerTr ("[warn] " <>))
  setLogging @NOTICE (set loggerTr ("[notice] " <>))

  withSimpleLogger runCLI

runCLI :: IO ()
runCLI = join . customExecParser (prefs showHelpOnError) $
  info (helper <*> parser)
  (  fullDesc
  <> header "hbs2-peer daemon"
  <> progDesc "serves HBS2 protocol"
  )
  where
    parser ::  Parser (IO ())
    parser = hsubparser (  command "run"       (info pRun  (progDesc "run peer"))
                        <> command "poke"      (info pPoke (progDesc "poke peer by rpc"))
                        <> command "announce"  (info pAnnounce (progDesc "announce block"))
                        <> command "ping"      (info pPing (progDesc "ping another peer"))
                        <> command "fetch"     (info pFetch (progDesc "fetch block"))
                        )

    common = do
      pref <- optional $ strOption ( short 'p' <> long "prefix"
                                               <> help "storage prefix" )

      l    <- strOption ( short 'l' <> long "listen"
                                    <> help "addr:port"
                                    <> value defListenUDP )

      r    <- strOption ( short 'r' <> long "rpc"
                                    <> help "addr:port"
                                    <> value defRpcUDP )

      k    <- strOption ( short 'k' <> long "key"
                                    <> help "peer keys file"
                        )


      pure $ PeerOpts pref l r k

    pRun = do
      runPeer <$> common

    pRpcCommon = do
      strOption (     short 'r' <> long "rpc"
                   <> help "addr:port"
                   <> value defRpcUDP
                )

    pPoke = do
      rpc <- pRpcCommon
      pure $ runRpcCommand rpc POKE

    pAnnounce = do
      rpc <- pRpcCommon
      h   <- strArgument ( metavar "HASH" )
      pure $ runRpcCommand rpc (ANNOUNCE h)

    pFetch = do
      rpc <- pRpcCommon
      h   <- strArgument ( metavar "HASH" )
      pure $ runRpcCommand rpc (FETCH h)

    pPing = do
      rpc <- pRpcCommon
      h   <- strArgument ( metavar "ADDR" )
      pure $ runRpcCommand rpc (PING h Nothing)

myException :: SomeException -> IO ()
myException e = die ( show e ) >> exitFailure


newtype CredentialsM e m a =
  CredentialsM { fromCredentials :: ReaderT (PeerCredentials e) m a }
  deriving newtype ( Functor
                   , Applicative
                   , Monad
                   , MonadIO
                   , MonadReader (PeerCredentials e)
                   , MonadTrans)

withCredentials :: forall e m a . (HasOwnPeer e m, Monad m)
                => PeerCredentials e
                -> CredentialsM e m a -> m a

withCredentials pc m = runReaderT (fromCredentials m) pc

instance (HasOwnPeer e m) => HasOwnPeer e (CredentialsM e m) where
  ownPeer = lift ownPeer

instance (Monad m, HasFabriq e m) => HasFabriq e (CredentialsM e m)  where
  getFabriq = lift getFabriq

instance (Sessions e p m ) => Sessions e p (CredentialsM e m)  where
  find k f = lift (find k f)
  fetch i d k f = lift (fetch i d k f)
  update d k f = lift (update d k f)
  expire k = lift (expire  k)

-- instance (Monad m, HasProtocol e p, HasThatPeer e p m) => Response e p (CredentialsM e m) where

instance Monad m => HasCredentials e (CredentialsM e m) where
  getCredentials = ask

instance Monad m => HasCredentials e (ResponseM e (CredentialsM e m)) where
  getCredentials = lift getCredentials

instance (Monad m, HasThatPeer e p m) => HasThatPeer e p (CredentialsM e m) where
  thatPeer = lift . thatPeer

instance ( EventEmitter e p m
         ) => EventEmitter e p (CredentialsM e m) where

  emit k d = lift $ emit k d

instance ( Monad m
         , Response e p m
         ) => Response e p (CredentialsM e m) where

  response = lift . response


-- FIXME: Нормальные синхронизированные логи. Можно даже цветные.
--        Ориентированные на Prettyprinter.
--        Без лишнего мусора.

-- FIXME: Убрать хардкод UDP отовсюду ниже.
--        Вынести  в сигнатуру.

runPeer :: PeerOpts -> IO ()
runPeer opts = Exception.handle myException $ do


  rpcQ <- newTQueueIO @RPCCommand

  let ps = mempty

  pc' <- LBS.readFile (view peerCredFile opts)
            <&> parseCredentials @UDP . AsCredFile
                                      . LBS.toStrict
                                      . LBS.take 4096

  pc <- pure pc' `orDie` "can't parse credential file"

  notice $ "run peer" <+> pretty (AsBase58 (view peerSignPk pc))

  xdg <- getXdgDirectory XdgData defStorePath <&> fromString

  let pref = uniLastDef xdg (view storage opts) :: StoragePrefix

  s <- simpleStorageInit @HbSync (Just pref)
  let blk = liftIO . hasBlock s

  w <- replicateM defStorageThreads $ async $ simpleStorageWorker s

  localMulticast <- (headMay <$> parseAddr (fromString defLocalMulticast)
                                      <&> fmap (PeerUDP . addrAddress))

                           `orDie` "assertion: localMulticastPeer not set"

  notice $ "multicast:" <+> pretty localMulticast

  mess <- newMessagingUDP False (Just (view listenOn opts))
            `orDie` "unable listen on the given addr"

  udp <- async $ runMessagingUDP mess
                   `catch` (\(e::SomeException) -> throwIO e )

  udp1 <- newMessagingUDP False (Just (view listenRpc opts))
            `orDie` "Can't start RPC listener"

  mrpc <- async $ runMessagingUDP udp1
                   `catch` (\(e::SomeException) -> throwIO e )

  mcast <- newMessagingUDPMulticast defLocalMulticast
            `orDie` "Can't start RPC listener"

  messMcast <- async $ runMessagingUDP mcast
                 `catch` (\(e::SomeException) -> throwIO e )

  denv <- newDownloadEnv

  penv <- newPeerEnv (AnyStorage s) (Fabriq mess) (getOwnPeer mess)

  loop <- async do

            runPeerM penv $ do
              adapter <- mkAdapter
              env <- ask

              pnonce <- peerNonce @UDP

              pl <- getPeerLocator @UDP

              addPeers @UDP pl ps

              subscribe @UDP PeerAnnounceEventKey $ \pe@(PeerAnnounceEvent pip nonce) -> do
                unless (nonce == pnonce) $ do
                  debug $ "Got peer announce!" <+> pretty pip
                  known <- find (KnownPeerKey pip) id <&> isJust
                  unless known $ sendPing pip

              subscribe @UDP AnyKnownPeerEventKey $ \(KnownPeerEvent p d) -> do
                addPeers pl [p]

                npi    <- newPeerInfo
                pfails <- fetch True npi (PeerInfoKey p) (view peerPingFailed)
                liftIO $ atomically $ writeTVar pfails 0

                debug $ "Got authorized peer!" <+> pretty p
                                               <+> pretty (AsBase58 (view peerSignKey d))

              void $ liftIO $ async $ withPeerM env do
                pause @'Seconds 1
                debug "sending first peer announce"
                request localMulticast (PeerAnnounce @UDP pnonce)

              void $ liftIO $ async $ withPeerM env $ forever $ do
                pause defPeerAnnounceTime -- FIXME: setting!
                debug "sending local peer announce"
                request localMulticast (PeerAnnounce @UDP pnonce)

              as <- liftIO $ async $ withPeerM env (peerPingLoop @UDP)

              as <- liftIO $ async $ withPeerM env (blockDownloadLoop denv)

              rpc <- liftIO $ async $ withPeerM env $ forever $ do
                        cmd <- liftIO $ atomically $ readTQueue rpcQ
                        case cmd of
                          POKE -> debug "on poke: alive and kicking!"

                          PING pa r -> do
                            debug $ "ping" <+> pretty pa
                            pip <- fromPeerAddr @UDP pa
                            subscribe (ConcretePeerKey pip) $ \(ConcretePeerData{}) -> do

                              maybe1 r (pure ()) $ \rpcPeer -> do
                                pinged <- toPeerAddr pip
                                request rpcPeer (RPCPong @UDP pinged)

                            sendPing pip

                          ANNOUNCE h  -> do
                            debug $ "got announce rpc" <+> pretty h
                            sto <- getStorage
                            mbsize <- liftIO $ hasBlock sto h

                            maybe1 mbsize (pure ()) $ \size -> do
                              let ann = BlockAnnounceInfo 0 NoBlockInfoMeta size h
                              no <- peerNonce @UDP
                              request localMulticast (BlockAnnounce @UDP no ann)

                          CHECK nonce pa h -> do
                            pip <- fromPeerAddr @UDP pa

                            n1 <- peerNonce @UDP

                            unless (nonce == n1) do

                              peer <- find @UDP (KnownPeerKey pip) id

                              debug $ "received announce from"
                                            <+> pretty pip
                                            <+> pretty h

                              case peer of
                                Nothing -> sendPing @UDP pip
                                Just{}  -> do
                                  debug "announce from a known peer"
                                  debug "preparing to dowload shit"
                                  debug "checking policy, blah-blah-blah. tomorrow"

                                  withDownload denv $ do
                                    processBlock h

                          _ -> pure ()


              me <- liftIO $ async $ withPeerM env $ do
                runProto @UDP
                  [ makeResponse (blockSizeProto blk dontHandle)
                  , makeResponse (blockChunksProto adapter)
                  , makeResponse blockAnnounceProto
                  , makeResponse (withCredentials pc . peerHandShakeProto)
                  ]

              void $ liftIO $ waitAnyCatchCancel [me,as]

  let pokeAction _ = do
        liftIO $ atomically $ writeTQueue rpcQ POKE

  let annAction h = do
        liftIO $ atomically $ writeTQueue rpcQ (ANNOUNCE h)

  let pingAction pa = do
        that <- thatPeer (Proxy @(RPC UDP))
        liftIO $ atomically $ writeTQueue rpcQ (PING pa (Just that))

  let fetchAction h = do
        debug  $ "fetchAction" <+> pretty h
        liftIO $ withPeerM penv
               $ withDownload denv (processBlock h)

  let arpc = RpcAdapter pokeAction
                        dontHandle
                        annAction
                        pingAction
                        dontHandle
                        fetchAction

  rpc <- async $ runRPC udp1 do
                   runProto @UDP
                     [ makeResponse (rpcHandler arpc)
                     ]

  menv <- newPeerEnv (AnyStorage s) (Fabriq mcast) (getOwnPeer mcast)

  ann <- async $ runPeerM menv $ do

                   self <- ownPeer @UDP

                   subscribe @UDP BlockAnnounceInfoKey $ \(BlockAnnounceEvent p bi no) -> do
                    unless (p == self) do
                      pa <- toPeerAddr p
                      liftIO $ atomically $ writeTQueue rpcQ (CHECK no pa (view biHash bi))

                   subscribe @UDP PeerAnnounceEventKey $ \pe@(PeerAnnounceEvent pip nonce) -> do
                      -- debug $ "Got peer announce!" <+> pretty pip
                      emitToPeer penv PeerAnnounceEventKey pe

                   runProto @UDP
                     [ makeResponse blockAnnounceProto
                     , makeResponse peerAnnounceProto
                     ]

  void $ waitAnyCatchCancel $ w <> [udp,loop,rpc,mrpc,ann,messMcast]

  simpleStorageStop s



emitToPeer :: ( MonadIO m
              , EventEmitter e a (PeerM e IO)
              )
           => PeerEnv e
           -> EventKey e a
           -> Event e a
           -> m ()

emitToPeer env k e = liftIO $ withPeerM env (emit k e)

withRPC :: String -> RPC UDP -> IO ()
withRPC saddr cmd = do

  as <- parseAddr (fromString saddr) <&> fmap (PeerUDP . addrAddress)
  let rpc' = headMay $ L.sortBy (compare `on` addrPriority) as

  rpc <- pure rpc' `orDie` "Can't parse RPC endpoing"

  udp1 <- newMessagingUDP False Nothing `orDie` "Can't start RPC"

  mrpc <- async $ runMessagingUDP udp1

  pingQ <- newTQueueIO

  prpc <- async $ runRPC udp1 do
                    env <- ask
                    proto <- liftIO $ async $ continueWithRPC env $ do
                      runProto @UDP
                        [ makeResponse (rpcHandler (adapter pingQ))
                        ]

                    request rpc cmd

                    case cmd of
                      RPCAnnounce{} -> pause @'Seconds 0.1 >> liftIO exitSuccess

                      RPCFetch{} -> pause @'Seconds 0.1 >> liftIO exitSuccess

                      RPCPing{} -> do
                        void $ liftIO $ void $ race (pause @'Seconds 5 >> exitFailure) do
                                 pa <- liftIO $ atomically $ readTQueue pingQ
                                 notice $ "pong from" <+> pretty pa
                                 exitSuccess

                      _ -> pure ()

                    void $ liftIO $ waitAnyCatchCancel [proto]

  void $ waitAnyCatchCancel [mrpc, prpc]

  where
    adapter q = RpcAdapter dontHandle
                          (const $ notice "alive-and-kicking" >> liftIO exitSuccess)
                          (const $ liftIO exitSuccess)
                          (const $ notice "ping?")
                          (liftIO . atomically . writeTQueue q)
                          dontHandle

runRpcCommand :: String -> RPCCommand -> IO ()
runRpcCommand saddr = \case
  POKE -> withRPC saddr (RPCPoke @UDP)
  PING s _ -> withRPC saddr (RPCPing s)
  ANNOUNCE h -> withRPC saddr (RPCAnnounce @UDP h)
  FETCH h  -> withRPC saddr (RPCFetch @UDP h)

  _ -> pure ()

