{-# Language AllowAmbiguousTypes #-}
module Bootstrap where

import HBS2.Prelude
import HBS2.Net.Proto.Types
import HBS2.Net.Proto.Peer
import HBS2.Clock
import HBS2.Net.Messaging.UDP
import HBS2.Net.IP.Addr
import HBS2.Net.Proto.Sessions

import PeerConfig
import HBS2.System.Logger.Simple

import Data.Functor
import Network.DNS qualified as DNS
import Network.DNS (Name(..),CharStr(..))
import Data.ByteString.Char8 qualified as B8
import Data.Foldable
import Data.Maybe
import Data.Set qualified as Set
import Data.Set (Set)
import Control.Monad
import Network.Socket

data PeerDnsBootStrapKey

data PeerKnownPeer

instance HasCfgKey PeerDnsBootStrapKey (Set String) where
  key = "bootstrap-dns"

instance HasCfgKey PeerKnownPeer [String] where
  key = "known-peer"

bootstrapDnsLoop :: forall e m . ( HasPeer e
                                 , Request e (PeerHandshake e) m
                                 , HasNonces (PeerHandshake e) m
                                 , Nonce (PeerHandshake e) ~ PingNonce
                                 , Sessions e (PeerHandshake e) m
                                 , Pretty (Peer e)
                                 , MonadIO m
                                 , e ~ UDP
                                 )
                             => PeerConfig -> m ()
bootstrapDnsLoop conf = do

  pause @'Seconds 2

  forever do
    debug "I'm a bootstrapLoop"

    let dns = cfgValue @PeerDnsBootStrapKey conf <> Set.singleton "bootstrap.hbs2.net"

    for_ (Set.toList dns) $ \dn -> do
      debug $ "bootstrapping from" <+> pretty dn
      answers <- liftIO $ DNS.queryTXT (Name $ fromString dn) <&> foldMap ( fmap mkStr . snd )
      for_ answers $ \answ -> do
        pips <-  liftIO $ parseAddr (fromString answ) <&> fmap (PeerUDP . addrAddress)
        for_ pips $ \pip -> do
          debug $ "got dns answer" <+> pretty pip
          sendPing @e pip

    -- FIXME: fix-bootstrapDnsLoop-time-hardcode
    pause @'Seconds 300

  where
    mkStr (CharStr s) = B8.unpack s

knownPeersPingLoop ::
  forall e m.
  ( HasPeer e,
    Request e (PeerHandshake e) m,
    HasNonces (PeerHandshake e) m,
    Nonce (PeerHandshake e) ~ PingNonce,
    Sessions e (PeerHandshake e) m,
    Pretty (Peer e),
    MonadIO m,
    e ~ UDP
  ) =>
  PeerConfig ->
  m ()
knownPeersPingLoop conf = do
  -- FIXME: add validation and error handling
  let parseKnownPeers xs =
        fmap (PeerUDP . addrAddress)
          . catMaybes
          <$> (fmap headMay . parseAddr . fromString)
          `mapM` xs
  knownPeers' <- liftIO $ parseKnownPeers $ cfgValue @PeerKnownPeer conf
  forever do
    forM_ knownPeers' (sendPing @e)
    pause @'Minutes 20
