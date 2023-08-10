{-# LANGUAGE StrictData #-}

module PeerMain.DialogCliCommand where

-- import Data.Generics.Labels
-- import Data.Generics.Product.Fields
import HBS2.Actors.Peer
import HBS2.Base58
import HBS2.Clock
import HBS2.Net.Proto.RefLog (RefLogKey(..))
import HBS2.Defaults
import HBS2.Events
import HBS2.Hash
import HBS2.Merkle
import HBS2.Net.Auth.Credentials
import HBS2.Net.IP.Addr
import HBS2.Net.Messaging
import HBS2.Net.Messaging.TCP
import HBS2.Net.Messaging.UDP
import HBS2.Net.PeerLocator
import HBS2.Net.Proto
import HBS2.Net.Proto.Definition
import HBS2.Net.Proto.Dialog
import HBS2.Net.Proto.Peer
import HBS2.Net.Proto.PeerAnnounce
import HBS2.Net.Proto.PeerExchange
import HBS2.Net.Proto.PeerMeta
import HBS2.Net.Proto.RefLog
import HBS2.Net.Proto.Sessions
import HBS2.Net.Proto.Types
import HBS2.OrDie
import HBS2.Prelude
import HBS2.Prelude.Plated
import HBS2.Storage.Simple
import HBS2.System.Logger.Simple hiding (info)
import HBS2.System.Logger.Simple qualified as Log

import BlockDownload
import BlockHttpDownload
import Bootstrap
import Brains
import CheckMetrics
import DownloadQ
import HttpWorker
import PeerConfig
import PeerInfo
import PeerMeta
import PeerTypes
import ProxyMessaging
import RefLog (reflogWorker)
import RefLog qualified
import RPC

import Control.Monad
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Maybe
import Crypto.Saltine.Core.Box qualified as Encrypt
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Default
import Data.Function
import Data.Functor
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Monoid qualified as Monoid
import Data.Set qualified as Set
import Data.String.Conversions as X (cs)
import Data.Void (absurd, Void)
import Lens.Micro.Platform
import Network.Socket
import Options.Applicative
import Streaming as S
import Streaming.Prelude qualified as S
import System.Directory
import UnliftIO.Async
import UnliftIO.Concurrent
import UnliftIO.Exception as U
import UnliftIO.Resource

-- import System.FilePath.Posix
import System.IO
import System.Exit


pDialog :: Parser (IO ())
pDialog = hsubparser $ mempty
   <> command "ping" (info pPing (progDesc "ping hbs2 node via dialog inteface") )
   <> command "debug" (info pDebug (progDesc "debug call different dialog inteface routes") )

confOpt :: Parser FilePath
confOpt = strOption ( long "config"  <> short 'c' <> help "config" )

data OptInitial (f :: * -> *) a b = OptInitial { unOptInitial :: f a }
  deriving (Generic, Show)

data OptResolved (f :: * -> *) a b = OptResolved { unOptResolved :: b }
  deriving (Generic, Show)

type DialOptInitial = DialOpt OptInitial
type DialOptResolved = DialOpt OptResolved

data DialOpt (f :: (* -> *) -> * -> * -> *) = DialOpt
  { dialOptConf :: f Maybe FilePath PeerConfig
  , dialOptAddr :: f Maybe String (Peer L4Proto)
  }
  deriving (Generic)

deriving instance Show DialOptInitial

pDialCommon :: Parser DialOptInitial
pDialCommon = do

    dialOptConf <- OptInitial <$> optional do
        strOption ( long "config"  <> short 'c' <> help "config" )

    dialOptAddr <- OptInitial <$> optional do
        strOption ( short 'r' <> long "dial" <> help "addr:port" )

    pure DialOpt {..}

resolveDialOpt :: DialOptInitial -> IO DialOptResolved
resolveDialOpt dopt = do
    config <- peerConfigRead (dopt ^. #dialOptConf . #unOptInitial)

    let dialConf = cfgValue @PeerRpcKey config :: Maybe String

    saddr <- (dopt ^. #dialOptAddr . #unOptInitial <|> dialConf)
        `orDieM` "Dial endpoint not set"

    as <- parseAddrUDP (cs saddr) <&> fmap (fromSockAddr @'UDP . addrAddress)
    peer <- (headMay $ List.sortBy (compare `on` addrPriority) as)
        `orDieM` "Can't parse Dial endpoint"

    pure DialOpt
        { dialOptConf = OptResolved config
        , dialOptAddr = OptResolved peer
        }

pPing :: Parser (IO ())
pPing = do
    dopt <- pDialCommon
    pure $ withDial dopt \peer dclient ->
        withClient dclient \cli -> do
            liftIO . print =<< do
                dQuery1 def cli peer (dpath "ping" [])


pDebug :: Parser (IO ())
pDebug = do
    dopt <- pDialCommon

    pure $ withDial dopt \peer dclient ->
        withClient dclient \cli -> do

            threadDelay 100
            liftIO $ putStrLn ""
            liftIO $ putStrLn "ping"
            liftIO . print =<< do
                dQuery' def cli peer (dpath "ping" []) \flow -> do
                    S.print flow

            threadDelay 100
            liftIO $ putStrLn ""
            liftIO $ putStrLn "ping1"
            liftIO . print =<< do
                dQuery1 def cli peer (dpath "ping" [])

            threadDelay 100
            liftIO $ putStrLn ""
            liftIO $ putStrLn "undefined-route"
            liftIO . print =<< do
                dQuery' def cli peer (dpath "undefined-rout" []) \flow -> do
                    S.print flow

            threadDelay 100
            liftIO $ putStrLn ""
            liftIO $ putStrLn "debug/timeout"
            liftIO . print =<< do
                dQuery' (def & #requestParamsTimeout .~ 0.1)
                            cli peer (dpath "debug/timeout" []) \flow -> do
                    S.print flow

            threadDelay 100
            liftIO $ putStrLn ""
            liftIO $ putStrLn "debug/no-response-header"
            liftIO . print =<< do
                dQuery' def cli peer (dpath "debug/no-response-header" []) \flow -> do
                    S.print flow

            threadDelay 100
            liftIO $ putStrLn ""
            liftIO $ putStrLn "debug/wrong-header"
            liftIO . print =<< do
                dQuery' def cli peer (dpath "debug/wrong-header" []) \flow -> do
                    S.print flow

            threadDelay 100
            liftIO $ putStrLn ""
            liftIO $ putStrLn "undefined-route-1"
            (U.handleAny \e -> liftIO (print e)) do
                liftIO . print =<< do
                    dQuery1 def cli peer (dpath "undefined-route-1" [])

            threadDelay 100
            liftIO $ putStrLn ""
            liftIO $ putStrLn "spec"
            liftIO . print =<< do
                dQuery' def cli peer (dpath "spec" []) \flow -> do
                    S.print flow


evalContT' :: ContT r m Void -> m r
evalContT' = flip runContT absurd

withDial :: forall e i .
    ( e ~ L4Proto
    )
    => DialOptInitial
    -> (     Peer e
          -> DClient (ResponseM e (RpcM (ResourceT IO))) (Peer e) i
          -> (ResponseM e (RpcM (ResourceT IO))) ()
        )
    -> IO ()
withDial dopt' cmd = do
    dopt <- resolveDialOpt dopt'
    setLoggingOff @DEBUG
    hSetBuffering stdout LineBuffering

    runResourceT do
        udp1 <- newMessagingUDP False Nothing `orDie` "Can't start Dial"

        evalContT' do

            dialProtoEnv :: DialogProtoEnv (ResponseM L4Proto (RpcM (ResourceT IO))) L4Proto
                <- newDialogProtoEnv

            amessaging <- ContT $ withAsync do
                runMessagingUDP udp1

            aprotos <- ContT $ withAsync $ runRPC udp1 do

                    runProto @e
                        [ makeResponse do
                              dialRespProto (DialRespProtoAdapter dialProtoEnv)
                        ]

            aclient <- ContT $ withAsync $
                runRPC udp1 do
                    let p = dopt ^. #dialOptAddr . #unOptResolved
                    runResponseM p $
                        cmd p
                        DClient
                          { clientCallerEnv = dialogProtoEnvCallerEnv dialProtoEnv
                          , clientSendProtoRequest = \peer frames -> do
                                  request peer (DialReq @e frames)

                          -- , clientGetKnownPeers :: m [(p, i)]
                          , clientGetKnownPeers = pure []
                          }

            ContT \_ -> waitAnyCancel [amessaging, aprotos, aclient]

    pure ()

