{-# Language TemplateHaskell #-}
{-# Language UndecidableInstances #-}
module RPC where


import HBS2.Actors.Peer
import HBS2.Base58
import HBS2.Clock
import HBS2.Hash
import HBS2.Net.Auth.Credentials
import HBS2.Net.IP.Addr
import HBS2.Net.Messaging.UDP
import HBS2.Net.Proto
import HBS2.Net.Proto.Definition()
import HBS2.OrDie
import HBS2.Prelude.Plated
import HBS2.System.Logger.Simple hiding (info)
import HBS2.System.Logger.Simple qualified as Log

import PeerConfig


import Codec.Serialise (serialise,deserialiseOrFail)
import Control.Applicative
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Data.ByteString.Lazy (ByteString)
import Data.Function
import Data.Functor
import Data.List qualified as L
import Lens.Micro.Platform
import Network.Socket
import System.Exit
import System.IO
import UnliftIO.Async as U
import Control.Concurrent.MVar

data PeerRpcKey

instance HasCfgKey PeerRpcKey (Maybe String) where
  key = "rpc"

data SetLogging =
    DebugOn Bool
  | TraceOn Bool
  deriving (Generic,Eq,Show)

instance Serialise SetLogging

data RPCCommand =
    DIE
  | POKE
  | ANNOUNCE (Hash HbSync)
  | PING (PeerAddr L4Proto) (Maybe (Peer L4Proto))
  | CHECK PeerNonce (PeerAddr L4Proto) (Hash HbSync)
  | FETCH (Hash HbSync)
  | PEERS
  | SETLOG SetLogging
  | REFLOGUPDATE ByteString
  | REFLOGFETCH (PubKey 'Sign (Encryption L4Proto))
  | REFLOGGET (PubKey 'Sign (Encryption L4Proto))
  | REFCHANHEADSEND (Hash HbSync)
  | REFCHANHEADGET (PubKey 'Sign (Encryption L4Proto))
  | REFCHANHEADFETCH (PubKey 'Sign (Encryption L4Proto))
  | REFCHANPROPOSE (PubKey 'Sign (Encryption L4Proto), ByteString)

data RPC e =
    RPCDie
  | RPCPoke
  | RPCPing (PeerAddr e)
  | RPCPong (PeerAddr e)
  | RPCPokeAnswer (PubKey 'Sign (Encryption e))
  | RPCPokeAnswerFull Text
  | RPCAnnounce (Hash HbSync)
  | RPCFetch (Hash HbSync)
  | RPCPeers
  | RPCPeersAnswer (PeerAddr e) (PubKey 'Sign (Encryption e))
  | RPCLogLevel SetLogging
  | RPCRefLogUpdate ByteString
  | RPCRefLogFetch (PubKey 'Sign (Encryption e))
  | RPCRefLogGet (PubKey 'Sign (Encryption e))
  | RPCRefLogGetAnswer (Maybe (Hash HbSync))
  | RPCRefChanHeadSend (Hash HbSync)
  | RPCRefChanHeadGet (PubKey 'Sign (Encryption e))
  | RPCRefChanHeadGetAnsw (Maybe (Hash HbSync))
  | RPCRefChanHeadFetch (PubKey 'Sign (Encryption e))
  | RPCRefChanPropose (PubKey 'Sign (Encryption e), ByteString)

  deriving stock (Generic)

instance (Serialise (PeerAddr e), Serialise (PubKey 'Sign (Encryption e))) => Serialise (RPC e)

instance HasProtocol L4Proto (RPC L4Proto) where
  type instance ProtocolId (RPC L4Proto) = 0xFFFFFFE0
  type instance Encoded L4Proto = ByteString
  decode = either (const Nothing) Just . deserialiseOrFail
  encode = serialise


data RPCEnv =
  RPCEnv
  { _rpcSelf :: Peer L4Proto
  , _rpcFab  :: Fabriq L4Proto
  }

makeLenses 'RPCEnv

data RpcAdapter e m =
  RpcAdapter
  { rpcOnPoke          :: RPC e -> m ()
  , rpcOnDie           :: RPC e -> m ()
  , rpcOnPokeAnswer    :: PubKey 'Sign (Encryption e) -> m ()
  , rpcOnPokeAnswerFull :: Text -> m ()
  , rpcOnAnnounce      :: Hash HbSync -> m ()
  , rpcOnPing          :: PeerAddr e -> m ()
  , rpcOnPong          :: PeerAddr e -> m ()
  , rpcOnFetch         :: Hash HbSync -> m ()
  , rpcOnPeers         :: RPC e -> m ()
  , rpcOnPeersAnswer   :: (PeerAddr e, PubKey 'Sign (Encryption e)) -> m ()
  , rpcOnLogLevel      :: SetLogging -> m ()
  , rpcOnRefLogUpdate  :: ByteString -> m ()
  , rpcOnRefLogFetch   :: PubKey 'Sign (Encryption e) -> m ()
  , rpcOnRefLogGet     :: PubKey 'Sign (Encryption e) -> m ()
  , rpcOnRefLogGetAnsw :: Maybe (Hash HbSync) -> m ()
  , rpcOnRefChanHeadSend :: Hash HbSync -> m ()
  , rpcOnRefChanHeadGet :: PubKey 'Sign (Encryption e) -> m ()
  , rpcOnRefChanHeadGetAnsw :: Maybe (Hash HbSync) -> m ()
  , rpcOnRefChanHeadFetch :: PubKey 'Sign (Encryption e) -> m ()
  , rpcOnRefChanPropose :: (PubKey 'Sign (Encryption e), ByteString) -> m ()
  }

newtype RpcM m a = RpcM { fromRpcM :: ReaderT RPCEnv m a }
                        deriving newtype ( Functor
                                         , Applicative
                                         , Monad
                                         , MonadIO
                                         , MonadReader RPCEnv
                                         , MonadTrans
                                         )

runRPC :: ( MonadIO m
          , PeerMessaging L4Proto
          )
       => MessagingUDP -> RpcM m a -> m a

runRPC udp m = runReaderT (fromRpcM m) (RPCEnv pip (Fabriq udp))
  where
    pip = getOwnPeer udp

continueWithRPC :: RPCEnv -> RpcM m a -> m a
continueWithRPC e m = runReaderT (fromRpcM m) e

instance Monad m => HasFabriq L4Proto (RpcM m) where
  getFabriq = asks (view rpcFab)

instance Monad m => HasOwnPeer L4Proto (RpcM m) where
  ownPeer = asks (view rpcSelf)

instance (Monad m, HasProtocol L4Proto p) => HasTimeLimits L4Proto p (RpcM m) where
  tryLockForPeriod _ _ = pure True

rpcHandler :: forall e m  . ( MonadIO m
                            , Response e (RPC e) m
                            , HasProtocol e (RPC e)
                            , IsPeerAddr e m
                            )
           => RpcAdapter e m -> RPC e -> m ()

rpcHandler adapter = \case
    p@RPCDie{}         -> rpcOnDie adapter p
    p@RPCPoke{}        -> rpcOnPoke adapter p
    (RPCPokeAnswer k)  -> rpcOnPokeAnswer adapter k
    (RPCPokeAnswerFull k)  -> rpcOnPokeAnswerFull adapter k
    (RPCAnnounce h)    -> rpcOnAnnounce adapter h
    (RPCPing pa)       -> rpcOnPing adapter pa
    (RPCPong pa)       -> rpcOnPong adapter pa
    (RPCFetch h)       -> rpcOnFetch adapter h
    p@RPCPeers{}       -> rpcOnPeers adapter p
    (RPCPeersAnswer pa k) -> rpcOnPeersAnswer adapter (pa,k)
    (RPCLogLevel l)    -> rpcOnLogLevel adapter l
    (RPCRefLogUpdate bs)  -> rpcOnRefLogUpdate adapter bs
    (RPCRefLogFetch e) -> rpcOnRefLogFetch adapter e
    (RPCRefLogGet e)   -> rpcOnRefLogGet adapter e
    (RPCRefLogGetAnswer s)   -> rpcOnRefLogGetAnsw adapter s
    (RPCRefChanHeadSend s)   -> rpcOnRefChanHeadSend adapter s
    (RPCRefChanHeadGet s)   -> rpcOnRefChanHeadGet adapter s
    (RPCRefChanHeadGetAnsw s)   -> rpcOnRefChanHeadGetAnsw adapter s
    (RPCRefChanHeadFetch s)   -> rpcOnRefChanHeadFetch adapter s
    (RPCRefChanPropose s)   -> rpcOnRefChanPropose adapter s

data RPCOpt =
  RPCOpt
  { _rpcOptConf :: Maybe FilePath
  , _rpcOptAddr :: Maybe String
  }

makeLenses 'RPCOpt


runRpcCommand :: FromStringMaybe (IPAddrPort L4Proto) => RPCOpt -> RPCCommand -> IO ()
runRpcCommand opt = \case
  DIE  -> withRPC opt RPCDie
  POKE -> withRPC opt RPCPoke
  PING s _ -> withRPC opt (RPCPing s)
  ANNOUNCE h -> withRPC opt (RPCAnnounce h)
  FETCH h  -> withRPC opt (RPCFetch h)
  PEERS -> withRPC opt RPCPeers
  SETLOG s -> withRPC opt (RPCLogLevel s)
  REFLOGUPDATE bs -> withRPC opt (RPCRefLogUpdate bs)
  REFLOGFETCH k -> withRPC opt (RPCRefLogFetch k)
  REFLOGGET k -> withRPC opt (RPCRefLogGet k)
  REFCHANHEADSEND h -> withRPC opt (RPCRefChanHeadSend h)
  REFCHANHEADGET s -> withRPC opt (RPCRefChanHeadGet s)
  REFCHANHEADFETCH s -> withRPC opt (RPCRefChanHeadFetch s)
  REFCHANPROPOSE s -> withRPC opt (RPCRefChanPropose s)

  _ -> pure ()


withRPC :: FromStringMaybe (PeerAddr L4Proto) => RPCOpt -> RPC L4Proto -> IO ()
withRPC o cmd = rpcClientMain o $ runResourceT do

  liftIO $ hSetBuffering stdout LineBuffering

  conf <- peerConfigRead (view rpcOptConf o)

  let rpcConf = cfgValue @PeerRpcKey conf :: Maybe String

  saddr <- pure  (view rpcOptAddr o <|> rpcConf) `orDie` "RPC endpoint not set"

  as <- liftIO $ parseAddrUDP (fromString saddr) <&> fmap (fromSockAddr @'UDP . addrAddress)
  let rpc' = headMay $ L.sortBy (compare `on` addrPriority) as

  rpc <- pure rpc' `orDie` "Can't parse RPC endpoint"

  udp1 <- newMessagingUDP False Nothing `orDie` "Can't start RPC"

  mrpc <- async $ runMessagingUDP udp1

  pingQ <- liftIO newTQueueIO

  pokeQ <- liftIO newTQueueIO

  pokeFQ <- liftIO newTQueueIO

  refQ <- liftIO newTQueueIO

  rchanheadMVar <- liftIO newEmptyMVar

  let  adapter =
        RpcAdapter dontHandle
                   dontHandle
                   (liftIO . atomically . writeTQueue pokeQ)
                   (liftIO . atomically . writeTQueue pokeFQ)
                   (const $ liftIO exitSuccess)
                   (const $ notice "ping?")
                   (liftIO . atomically . writeTQueue pingQ)
                   dontHandle
                   dontHandle

                   (\(pa, k) -> Log.info $ pretty (AsBase58 k) <+> pretty pa
                   )

                   dontHandle
                   dontHandle
                   dontHandle
                   dontHandle

                   ( liftIO . atomically . writeTQueue refQ )

                   dontHandle

                   dontHandle -- rpcOnRefChanHeadGet

                   (liftIO . putMVar rchanheadMVar) -- rpcOnRefChanHeadGetAnsw

                   dontHandle -- rpcOnRefChanHeadFetch

                   dontHandle -- rpcOnRefChanPropose


  prpc <- async $ runRPC udp1 do
                    env <- ask
                    proto <- liftIO $ async $ continueWithRPC env $ do
                      runProto @L4Proto
                        [ makeResponse (rpcHandler adapter)
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


                      RPCDie{} -> do
                        pause @'Seconds 0.25
                        liftIO exitSuccess

                      RPCPoke{} -> do
                        let onTimeout = do pause @'Seconds 1.5
                                           Log.info "no-one-is-here"
                                           exitFailure

                        void $ liftIO $ race onTimeout do
                                 k <- liftIO $ atomically $ readTQueue pokeFQ
                                 print (pretty k)
                                 hFlush stdout
                                 exitSuccess

                      RPCPeers{} -> liftIO do
                        pause @'Seconds 1
                        exitSuccess

                      RPCLogLevel{} -> liftIO exitSuccess

                      RPCRefLogUpdate{} -> liftIO do
                        pause @'Seconds 0.1
                        exitSuccess

                      RPCRefLogFetch {} -> liftIO do
                        pause @'Seconds 0.5
                        exitSuccess

                      RPCRefLogGet{} -> liftIO do
                        void $ liftIO $ race (pause @'Seconds 0.1 >> exitFailure) do
                                 k <- liftIO $ atomically $ readTQueue refQ
                                 case k of
                                  Nothing -> exitFailure
                                  Just re -> do
                                   print $ pretty re
                                   hFlush stdout
                                   exitSuccess

                      RPCRefChanHeadSend {} -> liftIO do
                        pause @'Seconds 0.25
                        exitSuccess

                      RPCRefChanHeadGet {} -> liftIO do

                        r <- race (pause @'Seconds 2) do
                                withMVar rchanheadMVar $ \v -> do
                                  pure v

                        case r of
                          Right (Just x) -> print (pretty x) >> exitSuccess

                          _ -> exitFailure

                      RPCRefChanHeadFetch {} -> liftIO do
                        pause @'Seconds 0.25
                        exitSuccess

                      RPCRefChanPropose{} -> liftIO do
                        pause @'Seconds 0.25
                        exitSuccess

                      _ -> pure ()

                    void $ liftIO $ waitAnyCancel [proto]

  void $ waitAnyCancel [mrpc, prpc]


rpcClientMain :: RPCOpt -> IO () -> IO ()
rpcClientMain opt action = do
  setLoggingOff @DEBUG
  action

