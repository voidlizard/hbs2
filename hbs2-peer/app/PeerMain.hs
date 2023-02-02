{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language TemplateHaskell #-}
{-# Language AllowAmbiguousTypes #-}
module Main where

import HBS2.Actors.Peer
import HBS2.Clock
import HBS2.Defaults
import HBS2.Events
import HBS2.Hash
import HBS2.Net.IP.Addr
import HBS2.Net.Messaging.UDP
import HBS2.Net.PeerLocator
import HBS2.Net.Proto
import HBS2.Net.Proto.Definition
import HBS2.Net.Proto.Peer
import HBS2.Net.Proto.Sessions
import HBS2.OrDie
import HBS2.Prelude.Plated
import HBS2.Storage.Simple
import HBS2.Net.Auth.Credentials

import RPC
import BlockDownload

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception as Exception
import Control.Monad.Reader
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Lens.Micro.Platform
import Network.Socket
import Options.Applicative
import Prettyprinter
import System.Directory
import System.Exit
import System.IO

debug :: (MonadIO m) => Doc ann -> m ()
debug p = liftIO $ hPrint stderr p

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
  | PING (PeerAddr UDP)

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
main = join . customExecParser (prefs showHelpOnError) $
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

    pPing = do
      rpc <- pRpcCommon
      h   <- strArgument ( metavar "ADDR" )
      pure $ runRpcCommand rpc (PING h)

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

instance Monad m => HasCredentials e (CredentialsM e m) where
  getCredentials = ask

instance Monad m => HasCredentials e (ResponseM e (CredentialsM e m)) where
  getCredentials = lift getCredentials

runPeer :: () =>  PeerOpts -> IO ()
runPeer opts = Exception.handle myException $ do


  rpcQ <- newTQueueIO @RPCCommand

  let ps = mempty

  pc' <- LBS.readFile (view peerCredFile opts)
            <&> parseCredentials @UDP . AsCredFile
                                      . LBS.toStrict
                                      . LBS.take 4096

  pc <- pure pc' `orDie` "can't parse credential file"

  debug $ "run peer" <+> pretty (AsBase58 (view peerSignPk pc))

  xdg <- getXdgDirectory XdgData defStorePath <&> fromString

  let pref = uniLastDef xdg (view storage opts) :: StoragePrefix

  s <- simpleStorageInit @HbSync (Just pref)
  let blk = liftIO . hasBlock s

  w <- replicateM defStorageThreads $ async $ simpleStorageWorker s

  localMulticast <- (headMay <$> parseAddr (fromString defLocalMulticast)
                                      <&> fmap (PeerUDP . addrAddress))

                           `orDie` "assertion: localMulticastPeer not set"

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

  loop <- async do

            runPeerM (AnyStorage s) (Fabriq mess) (getOwnPeer mess) $ do
              adapter <- mkAdapter
              env <- ask

              pl <- getPeerLocator @UDP

              addPeers @UDP pl ps

              as <- liftIO $ async $ withPeerM env blockDownloadLoop

              rpc <- liftIO $ async $ withPeerM env $ forever $ do
                        cmd <- liftIO $ atomically $ readTQueue rpcQ
                        case cmd of
                          POKE -> debug "on poke: alive and kicking!"

                          PING s -> do
                            debug $ "ping" <> pretty s
                            -- pip <- parseAddr s
                            pure ()

                          ANNOUNCE h  -> do
                            debug $ "got announce rpc" <+> pretty h
                            sto <- getStorage
                            mbsize <- liftIO $ hasBlock sto h

                            maybe1 mbsize (pure ()) $ \size -> do
                              let ann = BlockAnnounceInfo 0 NoBlockInfoMeta size h
                              request localMulticast (BlockAnnounce @UDP ann)

              me <- liftIO $ async $ withPeerM env $ do
                runProto @UDP
                  [ makeResponse (blockSizeProto blk dontHandle)
                  , makeResponse (blockChunksProto adapter)
                  , makeResponse blockAnnounceProto
                  ]

              poo <- liftIO $ async $ withPeerM env $ withCredentials pc $ do
                runProto @UDP
                  [ makeResponse peerHandShakeProto
                  ]

              void $ liftIO $ waitAnyCatchCancel [me,poo,as]

  let pokeAction _ = do
        liftIO $ atomically $ writeTQueue rpcQ POKE

  let annAction h = do
        liftIO $ atomically $ writeTQueue rpcQ (ANNOUNCE h)

  let pingAction pa = do
        liftIO $ atomically $ writeTQueue rpcQ (PING pa)

  let arpc = RpcAdapter pokeAction
                        dontHandle
                        annAction
                        pingAction

  rpc <- async $ runRPC udp1 do
                   runProto @UDP
                     [ makeResponse (rpcHandler arpc)
                     ]

  ann <- async $ runPeerM (AnyStorage s) (Fabriq mcast) (getOwnPeer mcast) $ do

                   self <- ownPeer @UDP

                   subscribe @UDP BlockAnnounceInfoKey $ \(BlockAnnounceEvent p bi) -> do
                    unless (p == self) do
                      debug $ "announce" <+> pretty p
                                         <+> pretty (view biHash bi)

                   runProto @UDP
                     [ makeResponse blockAnnounceProto
                     ]

  void $ waitAnyCatchCancel $ w <> [udp,loop,rpc,mrpc,ann,messMcast]

  simpleStorageStop s


withRPC :: String -> RPC UDP -> IO ()
withRPC saddr cmd = do

  rpc' <- headMay <$> parseAddr (fromString saddr) <&> fmap (PeerUDP . addrAddress)

  rpc <- pure rpc' `orDie` "Can't parse RPC endpoing"

  udp1 <- newMessagingUDP False Nothing `orDie` "Can't start RPC"

  mrpc <- async $ runMessagingUDP udp1

  prpc <- async $ runRPC udp1 do
                    env <- ask
                    proto <- liftIO $ async $ continueWithRPC env $ do
                      runProto @UDP
                        [ makeResponse (rpcHandler adapter)
                        ]

                    request rpc cmd

                    case cmd of
                      RPCAnnounce{} -> pause @'Seconds 0.1 >> liftIO exitSuccess

                      _ -> pure ()

                    void $ liftIO $ waitAnyCatchCancel [proto]

  void $ waitAnyCatchCancel [mrpc, prpc]

  where
    adapter = RpcAdapter dontHandle
                         (const $ debug "alive-and-kicking" >> liftIO exitSuccess)
                         (const $ liftIO exitSuccess)
                         (const $ debug "wat?")

runRpcCommand :: String -> RPCCommand -> IO ()
runRpcCommand saddr = \case
  POKE -> withRPC saddr (RPCPoke @UDP)
  PING s -> withRPC saddr (RPCPing s)
  ANNOUNCE h -> withRPC saddr (RPCAnnounce @UDP h)

