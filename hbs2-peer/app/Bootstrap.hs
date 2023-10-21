{-# Language AllowAmbiguousTypes #-}
{-# Language TypeOperators #-}
module Bootstrap where

import HBS2.Data.Types.Peer
import HBS2.Prelude
import HBS2.Net.Proto.Types
import HBS2.Net.Proto.Peer
import HBS2.Clock
import HBS2.Net.Proto.Sessions
import HBS2.Peer.Brains

import PeerConfig
import HBS2.System.Logger.Simple

import Network.DNS
import Control.Monad.Reader
import Data.ByteString.Char8 qualified as B8
import Data.Foldable
import Data.Maybe
import Data.Set qualified as Set
import Data.Set (Set)
import Data.List qualified as List
import Control.Monad.Trans.Maybe


data PeerDnsBootStrapKey

data PeerKnownPeer

instance Monad m => HasCfgKey PeerDnsBootStrapKey (Set String) m where
  key = "bootstrap-dns"

instance Monad m => HasCfgKey PeerKnownPeer (Set String) m where
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

bootstrapDnsLoop (PeerConfig syn) = do

  pause @'Seconds 2

  rs <- liftIO $ makeResolvSeed defaultResolvConf

  forever do
    debug "I'm a bootstrapLoop"

    dns <- runReaderT(cfgValue @PeerDnsBootStrapKey) syn
             <&> (<> Set.singleton "bootstrap.hbs2.net")

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
                   => PeerConfig
                   -> SomeBrains e
                   -> m ()
knownPeersPingLoop (PeerConfig syn) brains = do
  -- FIXME: add validation and error handling
  -- FIXME: tcp-addr-support-2
  let parseKnownPeers xs = do
        let pa = foldMap (maybeToList . fromStringMay) xs
        mapM fromPeerAddr pa

  let them = runReader (cfgValue @PeerKnownPeer) syn & Set.toList

  pex <- listPexInfo @e brains >>= liftIO . mapM fromPeerAddr

  knownPeers' <- liftIO $ parseKnownPeers them

  let pips = List.nub (knownPeers' <> pex)

  forever do
    forM_ pips (sendPing @e)
    pause @'Minutes 10




