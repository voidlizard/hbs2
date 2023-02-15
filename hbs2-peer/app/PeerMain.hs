{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language TemplateHaskell #-}
{-# Language AllowAmbiguousTypes #-}
{-# Language UndecidableInstances #-}
{-# Language MultiWayIf #-}
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
import HBS2.Net.Proto.PeerExchange
import HBS2.Net.Proto.Sessions
import HBS2.OrDie
import HBS2.Prelude.Plated
import HBS2.Storage.Simple

import HBS2.System.Logger.Simple hiding (info)
import HBS2.System.Logger.Simple qualified as Log

import RPC
import PeerTypes
import BlockDownload
import PeerInfo
import PeerConfig

import Data.Text qualified as Text
import Data.Foldable (for_)
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
import Data.Set qualified as Set
import Data.Map qualified as Map
import Data.Text (Text)
import Lens.Micro.Platform
import Network.Socket
import Options.Applicative
import Prettyprinter
import System.Directory
import System.Exit
import System.IO
import Data.Set (Set)

defStorageThreads :: Integral a => a
defStorageThreads = 4

defListenUDP :: String
defListenUDP = "0.0.0.0:7351"

defRpcUDP :: String
defRpcUDP = "localhost:13331"

defLocalMulticast :: String
defLocalMulticast = "239.192.152.145:10153"


data PeerListenKey
data PeerRpcKey
data PeerKeyFileKey
data PeerBlackListKey
data PeerStorageKey
data PeerAcceptAnnounceKey

data AcceptAnnounce = AcceptAnnounceAll
                    | AcceptAnnounceFrom (Set (PubKey 'Sign UDP))

instance Pretty AcceptAnnounce where
  pretty = \case
    AcceptAnnounceAll     -> parens ("accept-announce" <+> "*")

    -- FIXME: better-pretty-for-AcceptAnnounceFrom
    AcceptAnnounceFrom xs -> parens ("accept-announce" <+> pretty (fmap AsBase58 (Set.toList xs)))

instance HasCfgKey PeerListenKey (Maybe String) where
  key = "listen"

instance HasCfgKey PeerRpcKey (Maybe String) where
  key = "rpc"

instance HasCfgKey PeerKeyFileKey (Maybe String) where
  key = "key"

instance HasCfgKey PeerStorageKey (Maybe String) where
  key = "storage"

instance HasCfgKey PeerBlackListKey (Set String) where
  key = "blacklist"

instance HasCfgKey PeerAcceptAnnounceKey AcceptAnnounce where
  key = "accept-block-announce"

instance HasCfgValue PeerAcceptAnnounceKey AcceptAnnounce where
  cfgValue (PeerConfig syn) = fromMaybe (AcceptAnnounceFrom lst) fromAll
    where
      fromAll = headMay [ AcceptAnnounceAll | ListVal @C (Key s [SymbolVal "*"]) <- syn, s == kk ]
      lst = Set.fromList $
                catMaybes [ fromStringMay @(PubKey 'Sign UDP) (Text.unpack e)
                          | ListVal @C (Key s [LitStrVal e]) <- syn, s == kk
                          ]
      kk = key @PeerAcceptAnnounceKey @AcceptAnnounce


data RPCCommand =
    POKE
  | ANNOUNCE (Hash HbSync)
  | PING (PeerAddr UDP) (Maybe (Peer UDP))
  | CHECK PeerNonce (PeerAddr UDP) (Hash HbSync)
  | FETCH (Hash HbSync)
  | PEERS

data PeerOpts =
  PeerOpts
  { _storage       :: Maybe StoragePrefix
  , _listenOn      :: Maybe String
  , _listenRpc     :: Maybe String
  , _peerCredFile  :: Maybe FilePath
  , _peerConfig    :: Maybe FilePath
  }
  deriving stock (Data)

makeLenses 'PeerOpts


main :: IO ()
main = do

  sodiumInit

  setLogging @DEBUG  (set loggerTr ("[debug] " <>))
  setLogging @INFO   defLog
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
    parser = hsubparser (  command "init"      (info pInit (progDesc "creates default config"))
                        <> command "run"       (info pRun  (progDesc "run peer"))
                        <> command "poke"      (info pPoke (progDesc "poke peer by rpc"))
                        <> command "announce"  (info pAnnounce (progDesc "announce block"))
                        <> command "ping"      (info pPing (progDesc "ping another peer"))
                        <> command "fetch"     (info pFetch (progDesc "fetch block"))
                        <> command "peers"     (info pPeers (progDesc "show known peers"))
                        )

    common = do
      pref <- optional $ strOption ( short 'p' <> long "prefix"
                                               <> help "storage prefix" )

      l    <- optional $ strOption ( short 'l' <> long "listen"
                                    <> help "addr:port" )

      r    <- optional $ strOption ( short 'r' <> long "rpc"
                                    <> help "addr:port" )

      k    <- optional $ strOption ( short 'k' <> long "key"
                                    <> help "peer keys file" )

      c <- optional $ strOption ( long "config"  <> short 'c' <> help "config" )

      pure $ PeerOpts pref l r k c

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

    pPeers = do
      rpc <- pRpcCommon
      pure $ runRpcCommand rpc PEERS

    pInit = do
      pref <- optional $ strArgument ( metavar "DIR" )
      pure $ peerConfigInit pref

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


instance (Monad m, HasTimeLimits e p m) => HasTimeLimits  e p (CredentialsM e m) where
  tryLockForPeriod p m = lift $ tryLockForPeriod p m

instance (HasOwnPeer e m) => HasOwnPeer e (CredentialsM e m) where
  ownPeer = lift ownPeer

instance (Monad m, HasFabriq e m) => HasFabriq e (CredentialsM e m)  where
  getFabriq = lift getFabriq

instance (Sessions e p m ) => Sessions e p (CredentialsM e m)  where
  find k f = lift (find k f)
  fetch i d k f = lift (fetch i d k f)
  update d k f = lift (update d k f)
  expire k = lift (expire  k)

instance (Monad m, HasPeerNonce e m) => HasPeerNonce e (CredentialsM e m) where
  peerNonce = lift $ peerNonce @e

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

forKnownPeers :: forall e  m . ( MonadIO m
                               , HasPeerLocator e m
                               , Sessions e (KnownPeer e) m
                               , HasPeer e
                               )
               =>  ( Peer e -> PeerData e -> m () ) -> m ()
forKnownPeers m = do
  pl <- getPeerLocator @e
  pips <- knownPeers @e pl
  for_ pips $ \p -> do
    pd' <- find (KnownPeerKey p) id
    maybe1 pd' (pure ()) (m p)

runPeer :: forall e . e ~ UDP => PeerOpts -> IO ()
runPeer opts = Exception.handle myException $ do

  xdg <- getXdgDirectory XdgData defStorePath <&> fromString

  conf <- peerConfigRead (view peerConfig opts)

  -- let (PeerConfig syn) = conf
  -- print $ pretty syn

  let listenConf = cfgValue @PeerListenKey conf
  let rpcConf = cfgValue @PeerRpcKey conf
  let keyConf = cfgValue @PeerKeyFileKey conf
  let storConf = cfgValue @PeerStorageKey conf <&> StoragePrefix

  let listenSa = view listenOn opts <|> listenConf <|> Just defListenUDP
  let rpcSa = view listenRpc opts <|> rpcConf <|> Just defRpcUDP
  credFile <- pure (view peerCredFile opts <|> keyConf) `orDie` "credentials not set"

  let pref = view storage opts <|> storConf <|> Just xdg

  debug $ "storage prefix:" <+> pretty pref

  let bls = cfgValue @PeerBlackListKey conf :: Set String

  let blkeys = Set.fromList
                    $ catMaybes [ fromStringMay x | x <- Set.toList bls
                                ] :: Set (PubKey 'Sign UDP)

  let accptAnn = cfgValue @PeerAcceptAnnounceKey conf :: AcceptAnnounce

  print $ pretty accptAnn

  -- FIXME: move-peerBanned-somewhere
  let peerBanned p d = do
        let k = view peerSignKey d
        pure $ k `Set.member` blkeys

  let acceptAnnounce p d = do
        case accptAnn of
          AcceptAnnounceAll    -> pure True
          AcceptAnnounceFrom s -> pure $ view peerSignKey d `Set.member` s

  rpcQ <- newTQueueIO @RPCCommand

  let ps = mempty

  pc' <- LBS.readFile credFile
            <&> parseCredentials @e . AsCredFile
                                    . LBS.toStrict
                                    . LBS.take 4096

  pc <- pure pc' `orDie` "can't parse credential file"

  notice $ "run peer" <+> pretty (AsBase58 (view peerSignPk pc))



  s <- simpleStorageInit @HbSync (Just pref)
  let blk = liftIO . hasBlock s

  w <- replicateM defStorageThreads $ async $ simpleStorageWorker s

  localMulticast <- (headMay <$> parseAddr (fromString defLocalMulticast)
                                      <&> fmap (PeerUDP . addrAddress))

                           `orDie` "assertion: localMulticastPeer not set"

  notice $ "multicast:" <+> pretty localMulticast

  mess <- newMessagingUDP False listenSa
            `orDie` "unable listen on the given addr"

  udp <- async $ runMessagingUDP mess
                   `catch` (\(e::SomeException) -> throwIO e )

  udp1 <- newMessagingUDP False rpcSa
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

              pnonce <- peerNonce @e

              pl <- getPeerLocator @e

              addPeers @e pl ps

              subscribe @e PeerAnnounceEventKey $ \(PeerAnnounceEvent pip nonce) -> do
                unless (nonce == pnonce) $ do
                  debug $ "Got peer announce!" <+> pretty pip
                  pd <- find (KnownPeerKey pip) id -- <&> isJust
                  banned <- maybe (pure False) (peerBanned pip) pd
                  let known = isJust pd && not banned
                  unless known $ sendPing pip

              subscribe @e AnyKnownPeerEventKey $ \(KnownPeerEvent p d) -> do

                let thatNonce = view peerOwnNonce d

                banned <- peerBanned p d

                -- FIXME: check if we've got a reference to ourselves
                if | pnonce == thatNonce -> do
                    delPeers pl [p]
                    addExcluded pl [p]
                    expire (KnownPeerKey p)

                   | banned -> do
                       notice $ pretty p <+> "banned"

                   | otherwise -> do

                    pd' <- knownPeers @e pl >>=
                              \peers -> forM peers $ \pip -> do
                                          pd <- find (KnownPeerKey pip) (view peerOwnNonce)
                                          pure $ (,pip) <$> pd

                    let pd = Map.fromList $ catMaybes pd'

                    case Map.lookup thatNonce pd of
                      Just p0 | p0 /= p -> debug "Same peer, different address"
                      _ -> do

                        addPeers pl [p]

                        npi    <- newPeerInfo
                        pfails <- fetch True npi (PeerInfoKey p) (view peerPingFailed)
                        liftIO $ atomically $ writeTVar pfails 0

                        debug $ "Got authorized peer!" <+> pretty p
                                                       <+> pretty (AsBase58 (view peerSignKey d))

              void $ liftIO $ async $ withPeerM env do
                pause @'Seconds 1
                debug "sending first peer announce"
                request localMulticast (PeerAnnounce @e pnonce)

              let wo = fmap L.singleton

              workers <- do

                wo $ liftIO $ async $ withPeerM env $ forever $ do
                  pause defPeerAnnounceTime -- FIXME: setting!
                  debug "sending local peer announce"
                  request localMulticast (PeerAnnounce @e pnonce)

                wo $ liftIO $ async $ withPeerM env (peerPingLoop @e)

                wo $ liftIO $ async $ withPeerM env (pexLoop @e)

                wo $ liftIO $ async $ withPeerM env (blockDownloadLoop denv)

                wo $ liftIO $ async $ withPeerM env $ forever $ do
                          cmd <- liftIO $ atomically $ readTQueue rpcQ
                          case cmd of
                            POKE -> debug "on poke: alive and kicking!"

                            PING pa r -> do
                              debug $ "ping" <+> pretty pa
                              pip <- fromPeerAddr @e pa
                              subscribe (ConcretePeerKey pip) $ \(ConcretePeerData{}) -> do

                                maybe1 r (pure ()) $ \rpcPeer -> do
                                  pinged <- toPeerAddr pip
                                  request rpcPeer (RPCPong @e pinged)

                              sendPing pip

                            ANNOUNCE h  -> do
                              debug $ "got announce rpc" <+> pretty h
                              sto <- getStorage
                              mbsize <- liftIO $ hasBlock sto h

                              maybe1 mbsize (pure ()) $ \size -> do
                                debug "send multicast announce"

                                no <- peerNonce @e
                                let annInfo = BlockAnnounceInfo 0 NoBlockInfoMeta size h
                                let announce = BlockAnnounce @e no annInfo

                                request localMulticast announce

                                liftIO $ withPeerM env do
                                  debug "send single-cast announces"
                                  forKnownPeers $ \p _ -> do
                                    request @e p announce

                            CHECK nonce pa h -> do
                              pip <- fromPeerAddr @e pa

                              n1 <- peerNonce @e

                              unless (nonce == n1) do

                                peer <- find @e (KnownPeerKey pip) id

                                debug $ "received announce from"
                                              <+> pretty pip
                                              <+> pretty h

                                case peer of
                                  Nothing -> do
                                    sendPing @e pip
                                    -- TODO: enqueue-announce-from-unknown-peer?

                                  Just pd  -> do

                                    banned <- peerBanned pip pd

                                    notAccepted <- acceptAnnounce pip pd <&> not

                                    if | banned -> do

                                          notice $ pretty pip <+> "banned"

                                       | notAccepted -> do

                                          debug $ pretty pip <+> "announce-not-accepted"

                                       | otherwise -> do

                                          debug "announce from a known peer"
                                          debug "preparing to dowload shit"

                                          withDownload denv $ do
                                            processBlock h

                            _ -> pure ()


                wo $ liftIO $ async $ withPeerM env $ do
                  runProto @e
                    [ makeResponse (blockSizeProto blk dontHandle)
                    , makeResponse (blockChunksProto adapter)
                    , makeResponse blockAnnounceProto
                    , makeResponse (withCredentials pc . peerHandShakeProto)
                    , makeResponse peerExchangeProto
                    ]

              void $ liftIO $ waitAnyCatchCancel workers

  let pokeAction _ = do
        who <- thatPeer (Proxy @(RPC e))
        let k = view peerSignPk pc
        -- FIXME: to-delete-POKE
        liftIO $ atomically $ writeTQueue rpcQ POKE
        request who (RPCPokeAnswer @e k)

  let annAction h = do
        liftIO $ atomically $ writeTQueue rpcQ (ANNOUNCE h)

  let pingAction pa = do
        that <- thatPeer (Proxy @(RPC e))
        liftIO $ atomically $ writeTQueue rpcQ (PING pa (Just that))

  let fetchAction h = do
        debug  $ "fetchAction" <+> pretty h
        liftIO $ withPeerM penv
               $ withDownload denv (processBlock h)

  let peersAction _ = do
        who <- thatPeer (Proxy @(RPC e))
        void $ liftIO $ async $ withPeerM penv $ do
          forKnownPeers @e $ \p pd -> do
              pa <- toPeerAddr p
              let k = view peerSignKey pd
              request who (RPCPeersAnswer @e pa k)

  let arpc = RpcAdapter pokeAction
                        dontHandle
                        annAction
                        pingAction
                        dontHandle
                        fetchAction
                        peersAction
                        dontHandle

  rpc <- async $ runRPC udp1 do
                   runProto @e
                     [ makeResponse (rpcHandler arpc)
                     ]

  menv <- newPeerEnv (AnyStorage s) (Fabriq mcast) (getOwnPeer mcast)

  ann <- async $ runPeerM menv $ do

                   self <- ownPeer @e

                   subscribe @e BlockAnnounceInfoKey $ \(BlockAnnounceEvent p bi no) -> do
                    unless (p == self) do
                      pa <- toPeerAddr p
                      liftIO $ atomically $ writeTQueue rpcQ (CHECK no pa (view biHash bi))

                   subscribe @e PeerAnnounceEventKey $ \pe@(PeerAnnounceEvent pip nonce) -> do
                      -- debug $ "Got peer announce!" <+> pretty pip
                      emitToPeer penv PeerAnnounceEventKey pe

                   runProto @e
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

  pokeQ <- newTQueueIO

  prpc <- async $ runRPC udp1 do
                    env <- ask
                    proto <- liftIO $ async $ continueWithRPC env $ do
                      runProto @UDP
                        [ makeResponse (rpcHandler (adapter pingQ pokeQ))
                        ]

                    request rpc cmd

                    case cmd of
                      RPCAnnounce{} -> pause @'Seconds 0.1 >> liftIO exitSuccess

                      RPCFetch{} -> pause @'Seconds 0.1 >> liftIO exitSuccess

                      RPCPing{} -> do
                        void $ liftIO $ void $ race (pause @'Seconds 5 >> exitFailure) do
                                 pa <- liftIO $ atomically $ readTQueue pingQ
                                 Log.info $ "pong from" <+> pretty pa
                                 exitSuccess


                      RPCPoke{} -> do
                        let onTimeout = do pause @'Seconds 0.5
                                           Log.info "no-one-is-here"
                                           exitFailure

                        void $ liftIO $ race onTimeout do
                                 k <- liftIO $ atomically $ readTQueue pokeQ
                                 Log.info $ "alive-and-kicking" <+> pretty (AsBase58 k)
                                 exitSuccess

                      RPCPeers{} -> liftIO do
                        pause @'Seconds 1
                        exitSuccess

                      _ -> pure ()

                    void $ liftIO $ waitAnyCatchCancel [proto]

  void $ waitAnyCatchCancel [mrpc, prpc]

  where
    adapter q pq = RpcAdapter dontHandle
                          (liftIO . atomically . writeTQueue pq)
                          (const $ liftIO exitSuccess)
                          (const $ notice "ping?")
                          (liftIO . atomically . writeTQueue q)
                          dontHandle
                          dontHandle

                          (\(pa, k) -> Log.info $ pretty (AsBase58 k) <+> pretty pa
                          )

runRpcCommand :: String -> RPCCommand -> IO ()
runRpcCommand saddr = \case
  POKE -> withRPC saddr RPCPoke
  PING s _ -> withRPC saddr (RPCPing s)
  ANNOUNCE h -> withRPC saddr (RPCAnnounce h)
  FETCH h  -> withRPC saddr (RPCFetch h)
  PEERS -> withRPC saddr RPCPeers

  _ -> pure ()

