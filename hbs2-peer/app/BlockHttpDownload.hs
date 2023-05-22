{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language UndecidableInstances #-}
{-# Language MultiWayIf #-}
module BlockHttpDownload where

import HBS2.Actors.Peer
import HBS2.Clock
import HBS2.Data.Detect
import HBS2.Data.Types.Refs
import HBS2.Defaults
import HBS2.Events
import HBS2.Hash
import HBS2.Merkle
import HBS2.Net.IP.Addr
import HBS2.Net.Messaging.TCP
import HBS2.Net.PeerLocator
import HBS2.Net.Proto
import HBS2.Net.Proto.Definition
import HBS2.Net.Proto.Peer
import HBS2.Net.Proto.PeerMeta
import HBS2.Net.Proto.RefLog
import HBS2.Net.Proto.Sessions
import HBS2.Prelude
import HBS2.Prelude.Plated
import HBS2.Storage
import HBS2.System.Logger.Simple

import PeerTypes
import PeerInfo
import BlockDownload
import Brains

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Cache qualified as Cache
import Data.Foldable hiding (find)
import Data.HashMap.Strict qualified as HashMap
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List qualified as List
import Data.Maybe
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import Data.Word
import Lens.Micro.Platform
import Network.HTTP.Simple (getResponseBody, httpLbs, parseRequest, getResponseStatus)
import Network.HTTP.Types.Status
import Network.Socket
import Streaming (Stream, Of)
import Streaming.Prelude qualified as S
import System.Random (randomRIO)
import System.Random.Shuffle (shuffleM)
import Text.InterpolatedString.Perl6 (qc)
import UnliftIO.Exception

blockHttpDownloadLoop :: forall e  m .
    ( m ~ PeerM e IO
    , MonadIO m
    , HasProtocol e (BlockInfo e)
    , Sessions e (KnownPeer e) m
    , PeerSessionKey e (PeerInfo e)
    , Pretty (Peer e)
    , IsPeerAddr e m
    -- FIXME: backlog-do-something-with-that
    --   это не ревью, это надо что-то с этим
    --   сделать, неудачное решение
    , Block LBS.ByteString ~ LBS.ByteString
    )
    => DownloadEnv e -> m ()
blockHttpDownloadLoop denv = do

    e <- ask

    pl <- getPeerLocator @e

    pause @'Seconds 3.81

    debug "I'm blockHttpDownloadLoop"

---

    let streamPeers :: Stream (Of (Peer e, PeerInfo e)) m ()
        streamPeers = flip fix [] \goPeers -> \case
            [] -> do
                pause @'Seconds 1.44
                ps <- knownPeers @e pl
                when (null ps) do
                    trace $ "No peers to use for http download"
                    pause @'Seconds 4
                goPeers ps
            peer:ps -> do
                authorized <- lift $ find (KnownPeerKey peer) id <&> isJust
                pinfo <- lift $ find (PeerInfoKey peer) id <&> isJust
                when (authorized && pinfo) do
                      npi <- lift newPeerInfo
                      pinfo <- lift $ fetch True npi (PeerInfoKey peer) id
                      S.yield (peer, pinfo)
                goPeers ps

    let streamPeerAddrs = S.catMaybes $ streamPeers & S.mapM \(peer, pinfo) ->
            (fmap (peer, pinfo, ) . join . eitherToMaybe) <$> do
                r <- liftIO $ readTVarIO (_peerHttpApiAddress pinfo)
                -- debug $ "streamPeerAddrs" <+> pretty peer <+> viaShow (viaShow <$> r)
                pure r

    let streamBlockHs = S.catMaybes $ streamPeerAddrs & S.mapM \(peer, pinfo, apiAddr) -> do
            -- inq <- liftIO $ readTVarIO (_blockInQ denv)
            -- TODO: change to only use blockInQ
            -- do we need analog of getBlockForDownload ?
            mbh <- withDownload denv $ getBlockForDownload peer
            -- debug $ "streamBlockHs" <+> pretty peer <+> pretty apiAddr <+> viaShow (pretty <$> mbh)
            pure $ (peer, pinfo, apiAddr, ) <$> mbh

    streamBlockHs & S.mapM_ \(peer, pinfo, apiAddr, h) -> do

        trace $ "Querying via http from" <+> pretty (peer, apiAddr) <+> "block" <+> pretty h
        r <- liftIO $ race ( pause defBlockWaitMax )
          $ do
              req  <- liftIO $ parseRequest [qc|http://{apiAddr}/cat/{pretty h}|]
              resp <- httpLbs req

              case statusCode (getResponseStatus resp) of
                200 -> pure $ Just (getResponseBody resp)
                _   -> pure Nothing

        case r of
          Right (Just block) -> do
              trace $ "SUCCESS" <+> pretty peer <+> "http-download block" <+> pretty h
              liftIO $ atomically $ modifyTVar (_peerHttpDownloaded pinfo) (+1)
              sto <- getStorage
              liftIO $ putBlock sto block
              withDownload denv $ processBlock h
          _ -> do
              trace $ "FAIL" <+> pretty peer <+> "download block" <+> pretty h
              withDownload denv $ failedDownload peer h


---

-- FIXME: move-fillPeerMeta-to-separate-module
fillPeerMeta :: forall e  m .
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
    )
    => Maybe MessagingTCP -> m ()
fillPeerMeta mtcp = do
  debug "I'm fillPeerMeta"
  pl <- getPeerLocator @e

  pause @'Seconds 5 -- wait 'till everything calm down
  forever do

    ps <- knownPeers @e pl
    debug $ "fillPeerMeta peers:" <+> pretty ps
    npi <- newPeerInfo
    for_ ps $ \p -> do

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
                      peerMeta <- case meta of
                          NoMetaData -> (MaybeT . pure) Nothing
                          ShortMetadata t -> do
                              (MaybeT . pure) (parsePeerMeta t)
                          AnnHashRef h -> (MaybeT . pure) Nothing
                      liftIO $ atomically $ writeTVar (_peerMeta pinfo) (Just peerMeta)

                      -- 3) пробить, что есть tcp
                      forM_ (lookupDecode "listen-tcp" (unPeerMeta peerMeta)) \listenTCPPort -> lift do
                          peerTCPAddrPort <- replacePort p listenTCPPort
                          p <- fromPeerAddr (L4Address TCP peerTCPAddrPort)
                          sendPing p

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

        -- FIXME: move-hardcode-to-a-config
        pause @'Seconds 300

  where
    replacePort :: Peer e -> Word16 -> PeerM e IO (IPAddrPort e)
    replacePort peer port = do
        IPAddrPort (ip,_port) <- fromString @(IPAddrPort e) . show . pretty <$> toPeerAddr peer
        pure $ IPAddrPort (ip,port)

    lookupDecode :: (Eq k, Read v) => k -> [(k, ByteString)] -> Maybe v
    lookupDecode k d =
        readMay . Text.unpack . TE.decodeUtf8 =<< lookup k d
