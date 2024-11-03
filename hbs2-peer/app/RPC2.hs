{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language UndecidableInstances #-}
module RPC2
  ( module RPC2.Peer
  , module RPC2.RefLog
  , module RPC2.RefChan
  , module RPC2.LWWRef
  , HandleMethod(..)
  -- , module RPC2.Mailbox
  ) where


import HBS2.Prelude.Plated
import HBS2.Net.Proto.Service
import HBS2.Net.Proto.Sessions

import HBS2.Base58
import HBS2.Data.Types.Peer
import HBS2.Actors.Peer
import HBS2.Peer.Proto.Peer
import HBS2.Clock
import HBS2.Net.Auth.Schema

import HBS2.Peer.RPC.Internal.Types
import HBS2.Peer.RPC.API.Peer

import Data.Config.Suckless.Script

import RPC2.Peer
import RPC2.RefLog
import RPC2.RefChan
import RPC2.LWWRef
import RPC2.Mailbox()

import PeerTypes
import PeerInfo

import UnliftIO

import Data.Text qualified as Text
import Data.Either
import Data.Maybe
import Numeric

instance (e ~ L4Proto, MonadUnliftIO m, HasRpcContext PeerAPI RPC2Context m) => HandleMethod m RpcRunScript where
  handleMethod top = do

    co <- getRpcContext @PeerAPI

    let cli = parseTop top & fromRight mempty

    r <- try @_ @SomeException (run (dict co) cli)

    either (pure . Text.pack . show) (pure . Text.pack . show . pretty) r

   where

    dict RPC2Context{..} = makeDict @_ @m do
        entry $ bindMatch "hey" $ const do
          pure $ mkSym @C "hey"

        entry $ bindMatch "peer-info" $ const do

          now <- getTimeCoarse

          liftIO $ withPeerM rpcPeerEnv do
            pl <- getPeerLocator @e
            pips <- knownPeers @e pl
            npi <- newPeerInfo

            r <- for pips $ \p -> do
              pinfo@PeerInfo{..} <- fetch True npi (PeerInfoKey p) id
              burst     <- readTVarIO _peerBurst
              buM       <- readTVarIO _peerBurstMax
              errors    <- readTVarIO _peerErrorsPerSec
              downFails <- readTVarIO _peerDownloadFail
              downMiss  <- readTVarIO _peerDownloadMiss
              down      <- readTVarIO _peerDownloadedBlk
              rtt       <- medianPeerRTT pinfo <&> fmap realToFrac
              seen      <- readTVarIO _peerLastWatched
              let l = realToFrac (toNanoSecs $ now - seen) / 1e9

              let rttMs = (/1e6) <$> rtt <&> (\x -> showGFloat (Just 2) x "") <&> (<> "ms")
              let ls = showGFloat (Just 2) l "" <> "s"

              mpde <- find (KnownPeerKey p) id
              let pk = maybe1 mpde mempty $ \PeerData{..} -> do
                        [ mkList [ mkSym "key", mkSym (show $ pretty (AsBase58 _peerSignKey)) ] ]

              let peerStaff = mkList @C $
                    pk <>
                    [ mkList [ mkSym "addr",        mkSym (show $ pretty p) ]
                    , mkList [ mkSym "seen",        mkSym ls ]
                    , mkList [ mkSym "burst",       mkInt burst ]
                    , mkList [ mkSym "burst-max",   mkInt (fromMaybe 0 buM) ]
                    , mkList [ mkSym "errors",      mkInt (downFails + errors) ]
                    , mkList [ mkSym "downloaded",  mkInt down ]
                    , mkList [ mkSym "miss",        mkInt downMiss ]
                    ]
                    <> maybe1 rttMs mempty (\r -> [ mkList [ mkSym "rtt", mkSym r ] ])

              pure $ mkList @C [mkSym "peer", peerStaff ]

            pure $ mkList r

