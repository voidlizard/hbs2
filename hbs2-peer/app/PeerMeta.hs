module PeerMeta where

import HBS2.Actors.Peer
import HBS2.Clock
import HBS2.Defaults
import HBS2.Events
import HBS2.Merkle
import HBS2.Net.IP.Addr
import HBS2.Net.Messaging.TCP
import HBS2.Net.PeerLocator
import HBS2.Net.Proto
import HBS2.Net.Proto.Peer
import HBS2.Net.Proto.PeerMeta
import HBS2.Net.Proto.Sessions
import HBS2.Prelude.Plated

import PeerTypes

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State.Strict qualified as State
import Data.ByteString (ByteString)
import Data.Foldable hiding (find)
import Data.HashMap.Strict qualified as HashMap
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import Data.Time
import Data.Word
import Lens.Micro.Platform
import Network.HTTP.Simple (getResponseBody, httpLbs, parseRequest, getResponseStatus)
import Network.HTTP.Types.Status
import Text.InterpolatedString.Perl6 (qc)
import UnliftIO.Exception


fillPeerMeta :: forall e  m t .
    ( m ~ PeerM e IO
    , MonadIO m
    , HasProtocol e (PeerMetaProto e)
    , PeerSessionKey e (PeerInfo e)
    , PeerMessaging e
    , IsPeerAddr e m
    , Pretty (Peer e)
    , Pretty (PeerAddr e)
    , EventListener e ( PeerMetaProto e) m
    , e ~ L4Proto
    , IsTimeout t
    )
    => Maybe MessagingTCP -> Timeout t -> m ()
fillPeerMeta mtcp probePeriod = do
  debug "I'm fillPeerMeta"
  pl <- getPeerLocator @e

  pause @'Seconds 5 -- wait 'till everything calm down
  flip State.evalStateT Map.empty $ forever do
    pause @'Seconds 12

    ps <- knownPeers pl
    now <- liftIO getCurrentTime
    let pss = Set.fromList ps
    psActual <- Map.filterWithKey (\k _ -> k `Set.member` pss) <$> State.get
    let psNew = pss Set.\\ (Map.keysSet psActual)
    let psReady = Map.keysSet . Map.filter (\t -> t < now) $ psActual
    let ps' = Set.toList (psNew <> psReady)
    (State.put . (<> psActual) . Map.fromList) $
        (, now & addUTCTime (toNominalDiffTime probePeriod)) <$> ps'

    when ((not . null) ps') $ lift do
      debug $ "fillPeerMeta peers:" <+> pretty ps'
      for_ ps' $ \p -> do
        npi <- newPeerInfo

        pinfo <- fetch True npi (PeerInfoKey p) id
        mmApiAddr <- liftIO $ readTVarIO (_peerHttpApiAddress pinfo)

        debug $ "Find peer meta and http address" <+> pretty p <+> "current:" <+> viaShow mmApiAddr
        case mmApiAddr of
          Left attemptn -> do

              q <- liftIO newTQueueIO

              subscribe @e (PeerMetaEventKey p) $ \case
                PeerMetaEvent meta -> do
                    liftIO $ atomically $ writeTQueue q (Just meta)

              request p (GetPeerMeta @e)

              r <- liftIO $ race ( pause defGetPeerMetaTimeout )
                            ( atomically $ do
                                  s <- readTQueue q
                                  void $ flushTQueue q
                                  pure s
                            )
              case r of
                  Left _ ->
                      liftIO $ atomically $ writeTVar (_peerHttpApiAddress pinfo) $
                          if attemptn < 3 then (Left (attemptn + 1)) else (Right Nothing)

                  Right (Just meta) -> (void . runMaybeT) do

                      debug $ "*** GOT GOOD META *** " <+> pretty p <+> viaShow meta

                      peerMeta <- case meta of
                          NoMetaData -> (MaybeT . pure) Nothing
                          ShortMetadata t -> do
                              (MaybeT . pure) (parsePeerMeta t)
                          AnnHashRef h -> (MaybeT . pure) Nothing
                      liftIO $ atomically $ writeTVar (_peerMeta pinfo) (Just peerMeta)

                      debug $ "*** GOT VERY GOOD META *** " <+> pretty p <+> viaShow peerMeta

                      -- 3) пробить, что есть tcp
                      forM_ (lookupDecode "listen-tcp" (unPeerMeta peerMeta)) \listenTCPPort -> lift do
                          peerTCPAddrPort <- replacePort p listenTCPPort
                          candidate <- fromPeerAddr (L4Address TCP peerTCPAddrPort)

                          debug $ "** SENDING PING BASE ON META ** " <+> pretty candidate

                          sendPing candidate
                          -- если пинг на этот адрес уйдет, то пир сам добавится
                          -- в knownPeers, делать ничего не надо

                          forM_ mtcp \(tcp :: MessagingTCP) -> do
                              -- 4) выяснить, можно ли к нему открыть соединение на этот порт
                              -- возможно, с ним уже открыто соединение
                              -- или попробовать открыть или запомнить, что было открыто
                              -- connectPeerTCP  ?
                              tcpAddressIsAvailable <- isJust <$> do
                                  liftIO $ atomically $ readTVar (view tcpPeerConn tcp) <&> HashMap.lookup p
                              when tcpAddressIsAvailable do
                                  -- добавить этого пира в pex
                                  addPeers pl [p]

                      port <- (MaybeT . pure) (lookupDecode "http-port" (unPeerMeta peerMeta))

                      lift do

                          peerHttpApiAddr <- show . pretty <$> replacePort p port
                          -- check peerHttpApiAddr

                          r :: Maybe () <- runMaybeT do
                              resp <- MaybeT (liftIO $ fmap eitherToMaybe
                                        $ race ( pause defBlockWaitMax )
                                        (do
                                            req  <- liftIO $ parseRequest [qc|http://{peerHttpApiAddr}/metadata|]
                                            httpLbs req
                                         )
                                          `catch` (\(e :: SomeException) -> debug (viaShow e) >> pure (Left ()))
                                    )
                              MaybeT . pure $ case statusCode (getResponseStatus resp) of
                                  200 -> Just ()
                                  _   -> Nothing

                          liftIO $ atomically $ writeTVar (_peerHttpApiAddress pinfo) $ Right $ peerHttpApiAddr <$ r
                          mapM_ (liftIO . atomically . writeTVar (_peerMeta pinfo) . Just) $ peerMeta <$ r
                          debug $ "Got peer meta from" <+> pretty p <+> ":" <+> viaShow peerMeta

                  _ -> do
                          liftIO $ atomically $ writeTVar (_peerHttpApiAddress pinfo) $ Right Nothing

          _ -> pure ()

  where
    replacePort :: Peer e -> Word16 -> PeerM e IO (IPAddrPort e)
    replacePort peer port = do
        IPAddrPort (ip,_port) <- fromString @(IPAddrPort e) . show . pretty <$> toPeerAddr peer
        pure $ IPAddrPort (ip,port)

    lookupDecode :: (Eq k, Read v) => k -> [(k, ByteString)] -> Maybe v
    lookupDecode k d =
        readMay . Text.unpack . TE.decodeUtf8 =<< lookup k d
