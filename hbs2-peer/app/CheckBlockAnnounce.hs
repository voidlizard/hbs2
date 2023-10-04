{-# Language MultiWayIf #-}
module CheckBlockAnnounce where

import HBS2.Prelude.Plated
import HBS2.Actors.Peer
import HBS2.Base58
import HBS2.Data.Types.Peer
import HBS2.Hash
import HBS2.Net.Proto.Peer
import HBS2.Net.Proto.Sessions
import HBS2.Net.Proto.Types

import PeerTypes
import PeerConfig
import CheckPeer (peerBanned)
import BlockDownload
import DownloadQ

import HBS2.System.Logger.Simple

import Data.Set qualified as Set
import Data.Set (Set)
import Lens.Micro.Platform
import Data.Text qualified as Text
import Data.Maybe

data PeerAcceptAnnounceKey

data AcceptAnnounce = AcceptAnnounceAll
                    | AcceptAnnounceFrom (Set (PubKey 'Sign (Encryption L4Proto)))

instance Pretty AcceptAnnounce where
  pretty = \case
    AcceptAnnounceAll     -> parens ("accept-announce" <+> "*")

    -- FIXME: better-pretty-for-AcceptAnnounceFrom
    AcceptAnnounceFrom xs -> parens ("accept-announce" <+> pretty (fmap AsBase58 (Set.toList xs)))



instance HasCfgKey PeerAcceptAnnounceKey AcceptAnnounce where
  key = "accept-block-announce"

instance HasCfgValue PeerAcceptAnnounceKey AcceptAnnounce where
  cfgValue (PeerConfig syn) = fromMaybe (AcceptAnnounceFrom lst) fromAll
    where
      fromAll = headMay [ AcceptAnnounceAll | ListVal @C (Key s [SymbolVal "*"]) <- syn, s == kk ]
      lst = Set.fromList $
                catMaybes [ fromStringMay @(PubKey 'Sign (Encryption L4Proto)) (Text.unpack e)
                          | ListVal @C (Key s [LitStrVal e]) <- syn, s == kk
                          ]
      kk = key @PeerAcceptAnnounceKey @AcceptAnnounce


checkBlockAnnounce :: forall e m . ( e ~ L4Proto
                                   , m ~ PeerM e IO
                                   )
                   => PeerConfig
                   -> DownloadEnv e
                   -> PeerNonce
                   -> PeerAddr e
                   -> Hash HbSync
                   -> m ()

checkBlockAnnounce conf denv nonce pa h = do

  let accptAnn = cfgValue @PeerAcceptAnnounceKey conf :: AcceptAnnounce

  let acceptAnnounce p pd = do
        case accptAnn of
          AcceptAnnounceAll    -> pure True
          AcceptAnnounceFrom s -> pure $ view peerSignKey pd `Set.member` s

  pip <- fromPeerAddr @e pa

  n1 <- peerNonce @e

  unless (nonce == n1) do

    mpde <- find @e (KnownPeerKey pip) id

    debug $ "received announce from"
                  <+> pretty pip
                  <+> pretty h

    case mpde of
      Nothing -> do
        sendPing @e pip
        -- TODO: enqueue-announce-from-unknown-peer?

      Just pd  -> do

        banned <- peerBanned conf pd

        notAccepted <- acceptAnnounce pip pd <&> not

        if | banned -> do

              notice $ pretty pip <+> "banned"

           | notAccepted -> do

              debug $ pretty pip <+> "announce-not-accepted"

           | otherwise -> do

              downloadLogAppend @e h
              withDownload denv $ do
                  processBlock h

