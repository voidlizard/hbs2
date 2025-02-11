module Multicast where

import HBS2.Prelude

import PeerTypes

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
-- import Control.Concurrent.STM
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
import UnliftIO.STM
import UnliftIO.Async
import UnliftIO.Concurrent (getNumCapabilities)

import Streaming.Prelude qualified as S


import Data.Kind

  -- menv <- newPeerEnv pl (AnyStorage s) (Fabriq mcast) (getOwnPeer mcast)
  -- do
  --   probe <- newSimpleProbe "PeerEnv_Announce"
  --   addProbe probe
  --   peerEnvSetProbe menv probe

  -- probesMenv <- liftIO $ async $ forever do
  --     pause @'Seconds 10
  --     peerEnvCollectProbes menv

  -- ann <- liftIO $ async $ runPeerM menv $ do

  --                  self <- ownPeer @e

  --                  subscribe @e BlockAnnounceInfoKey $ \(BlockAnnounceEvent p bi no) -> do
  --                   unless (p == self) do
  --                     pa <- toPeerAddr p
  --                     checkBlockAnnounce conf penv no pa (view biHash bi)

  --                  subscribe @e PeerAnnounceEventKey $ \pe@(PeerAnnounceEvent{}) -> do
  --                     -- debug $ "Got peer announce!" <+> pretty pip
  --                     emitToPeer penv PeerAnnounceEventKey pe

  --                  runProto @e
  --                    [ makeResponse blockAnnounceProto
  --                    , makeResponse peerAnnounceProto
  --                    ]


multicastWorker :: forall e s m . ( s ~ Encryption e
                                  , e ~ L4Proto
                                  , MonadUnliftIO m
                                  -- , HasStorage m
                                  -- , HasPeerLocator e m
                                  -- , HasPeerNonce L4Proto m
                                  )
                => PeerConfig -> PeerEnv e -> PeerM e m ()

multicastWorker conf penv = recover do

  debug $ red "multicastWorker started"

  sto <- getStorage
  pl <- getPeerLocator @e
  pnonce <- peerNonce @e

  localMCast_ <- liftIO newEmptyTMVarIO

  flip runContT pure do

    mcast' <- lift (newMessagingUDPMulticast defLocalMulticast)

    -- FIXME: log-on-exit
    mcast <- ContT $ maybe1 mcast' none

    messMcast <- ContT $ withAsync $ runMessagingUDP mcast

    menv <- newPeerEnv pl sto (Fabriq mcast) (getOwnPeer mcast)

    ann <- ContT $ withAsync $ do
            localMulticast <- atomically $ takeTMVar localMCast_
            forever do
              pips <- getKnownPeers @L4Proto
              let w = if null pips then 10 else defPeerAnnounceTime
              debug $ yellow "Sending local peer announce"
              request localMulticast (PeerAnnounce @e pnonce)
              pause w

    liftIO $ runPeerM menv $ do

      self <- ownPeer @e

      atomically $ putTMVar localMCast_ self

      subscribe @e BlockAnnounceInfoKey $ \(BlockAnnounceEvent p bi no) -> do
       unless (p == self) do
         pa <- toPeerAddr p
         checkBlockAnnounce (coerce conf) penv no pa (view biHash bi)

      subscribe @e PeerAnnounceEventKey $ \pe@(PeerAnnounceEvent p _) -> do
         debug $ green "Got peer announce from" <+> pretty p
         emitToPeer penv PeerAnnounceEventKey pe

      runProto @e
        [ makeResponse blockAnnounceProto
        , makeResponse peerAnnounceProto
        ]

  where
    recover m = U.catch (withPeerM penv m) \case
                  ( e :: IOError ) -> do
                    err $ "Multicast thread error" <+> viaShow e
                    let t = 10 :: Timeout 'Seconds
                    warn $ "Wait" <+> pretty t
                    pause @'Seconds 120
                    recover m



