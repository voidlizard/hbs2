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
