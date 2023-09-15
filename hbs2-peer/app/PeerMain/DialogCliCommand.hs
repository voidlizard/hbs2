{-# LANGUAGE StrictData #-}

module PeerMain.DialogCliCommand where

import Data.Generics.Labels
import Data.Generics.Product.Fields
import HBS2.Actors.Peer
import HBS2.Hash
import HBS2.Net.IP.Addr
import HBS2.Net.Messaging.UDP
import HBS2.Net.Proto
import HBS2.Net.Proto.Dialog
import HBS2.OrDie
import HBS2.Prelude
import HBS2.System.Logger.Simple hiding (info)

import PeerConfig
import RPC (PeerRpcKey, RpcM, runRPC)

import Control.Monad.Except (Except(..), ExceptT(..), runExcept, runExceptT)
import Control.Monad.State.Strict (evalStateT)
import Control.Monad.Trans.Cont
import Data.Default
import Data.Function
import Data.Functor
import Data.Kind
import Data.List qualified as List
import Data.String.Conversions as X (cs)
import Data.Void (absurd, Void)
import Lens.Micro.Platform
import Network.Socket
import Options.Applicative
import Streaming.Prelude qualified as S
import System.IO
import UnliftIO.Async
import UnliftIO.Concurrent
import UnliftIO.Exception as U
import UnliftIO.Resource


pDialog :: Parser (IO ())
pDialog = hsubparser $ mempty
   <> command "ping" (info pPing (progDesc "ping hbs2 node via dialog inteface") )
   <> command "debug" (info pDebug (progDesc "debug call different dialog inteface routes") )
   <> command "reflog" (info pReflog (progDesc "reflog commands") )

pReflog :: Parser (IO ())
pReflog = hsubparser $ mempty
   <> command "get" (info pRefLogGet (progDesc "get own reflog from all" ))
   <> command "fetch" (info pRefLogFetch (progDesc "fetch reflog from all" ))

confOpt :: Parser FilePath
confOpt = strOption ( long "config"  <> short 'c' <> help "config" )

newtype OptInitial (f :: Type -> Type) a b = OptInitial { unOptInitial :: f a }
  deriving (Generic, Show)

newtype OptResolved (f :: Type -> Type) a b = OptResolved { unOptResolved :: b }
  deriving (Generic, Show)

type DialOptInitial = DialOpt OptInitial
type DialOptResolved = DialOpt OptResolved

data DialOpt (f :: (Type -> Type) -> Type -> Type -> Type) = DialOpt
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
    peer <- headMay (List.sortBy (compare `on` addrPriority) as)
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

reflogKeyP :: ReadM (PubKey 'Sign (Encryption L4Proto))
reflogKeyP = eitherReader $
    maybe (Left "invalid REFLOG-KEY") pure . fromStringMay

pRefLogGet :: Parser (IO ())
pRefLogGet = do
    dopt <- pDialCommon
    rkey <- argument reflogKeyP ( metavar "REFLOG-KEY" )
    pure do
        withDial dopt \peer dclient ->
            withClient dclient \cli -> do
                xs <- dQuery1 def cli peer (dpath "reflog/get" [serialiseS rkey])

                hash <- either (lift . lift . fail) pure $ runExcept $ flip evalStateT xs do
                    cutFrameDecode @(Hash HbSync) do
                        "No hash in response: " <> show xs

                liftIO . print $ pretty hash

pRefLogFetch :: Parser (IO ())
pRefLogFetch = do
    dopt <- pDialCommon
    rkey <- argument reflogKeyP ( metavar "REFLOG-KEY" )
    pure do
        withDial dopt \peer dclient ->
            withClient dclient \cli -> do
                xs <- dQuery1 def cli peer (dpath "reflog/fetch" [serialiseS rkey])

                liftIO . print $ "Response: " <> show xs

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

