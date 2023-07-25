{-# Language AllowAmbiguousTypes #-}
module Bootstrap where

import HBS2.Data.Types.Peer
import HBS2.Prelude
import HBS2.Net.Proto.Types
import HBS2.Net.Proto.Peer
import HBS2.Clock
import HBS2.Net.IP.Addr
import HBS2.Net.Proto.Sessions

import PeerConfig
import HBS2.System.Logger.Simple

import Data.Functor
import Network.DNS
import Data.ByteString.Char8 qualified as B8
import Data.Foldable
import Data.Maybe
import Data.Set qualified as Set
import Data.Set (Set)
import Control.Monad
import Network.Socket
import Control.Monad.Trans.Maybe


data PeerDnsBootStrapKey

data PeerKnownPeer

instance HasCfgKey PeerDnsBootStrapKey (Set String) where
  key = "bootstrap-dns"

instance HasCfgKey PeerKnownPeer [String] where
  key = "known-peer"

-- FIXME: tcp-addr-support-bootstrap
bootstrapDnsLoop :: forall e m . ( HasPeer e
                                 , Request e (PeerHandshake e) m
                                 , HasNonces (PeerHandshake e) m
                                 , Nonce (PeerHandshake e) ~ PingNonce
                                 , Sessions e (PeerHandshake e) m
                                 , Pretty (Peer e)
                                 -- , FromSockAddr 'UDP (Peer e)
                                 , e ~ L4Proto
                                 , MonadIO m
                                 )
                             => PeerConfig -> m ()
bootstrapDnsLoop conf = do

  pause @'Seconds 2

  rs <- liftIO $ makeResolvSeed defaultResolvConf

  forever do
    debug "I'm a bootstrapLoop"

    let dns = cfgValue @PeerDnsBootStrapKey conf <> Set.singleton "bootstrap.hbs2.net"

    -- FIXME: utf8-domains
    for_ (Set.toList dns) $ \dn -> do
      debug $ "bootstrapping from" <+> pretty dn
      answers <- liftIO $ withResolver rs $ \resolver -> lookupTXT resolver (B8.pack dn) <&> either mempty id
      void $ runMaybeT do
        for_ answers $ \answ -> do
          -- FIXME: tcp-addr-support-1
          pa <- MaybeT $ pure $ fromStringMay @(PeerAddr L4Proto) (B8.unpack answ)
          pip <- fromPeerAddr pa
          debug $ "BOOTSTRAP:" <+> pretty pip
          lift $ sendPing @e pip

    -- FIXME: fix-bootstrapDnsLoop-time-hardcode
    pause @'Seconds 300


-- FIXME: tcp-addr-support-known-peers-loop
knownPeersPingLoop :: forall e m . ( HasPeer e
                                   , Request e (PeerHandshake e) m
                                   , HasNonces (PeerHandshake e) m
                                   , Nonce (PeerHandshake e) ~ PingNonce
                                   , Sessions e (PeerHandshake e) m
                                   , Pretty (Peer e)
                                   , e ~ L4Proto
                                   , MonadIO m)
                   => PeerConfig -> m ()
knownPeersPingLoop conf = do
  -- FIXME: add validation and error handling
  -- FIXME: tcp-addr-support-2
  let parseKnownPeers xs = do
        let pa = foldMap (maybeToList . fromStringMay) xs
        mapM fromPeerAddr pa

  knownPeers' <- liftIO $ parseKnownPeers $ cfgValue @PeerKnownPeer conf
  forever do
    forM_ knownPeers' (sendPing @e)
    pause @'Minutes 20

