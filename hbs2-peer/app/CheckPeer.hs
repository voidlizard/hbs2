module CheckPeer where

import HBS2.Prelude.Plated
import HBS2.Data.Types.Peer
import HBS2.Net.Proto.Types

import PeerTypes
import PeerConfig

import Control.Monad.Reader
import Data.Set qualified as Set
import Data.Set (Set)
import Lens.Micro.Platform


data PeerBlackListKey
data PeerWhiteListKey

instance Monad m => HasCfgKey PeerBlackListKey (Set String) m where
  key = "blacklist"

instance Monad m => HasCfgKey PeerWhiteListKey (Set String) m where
  key = "whitelist"

peerBanned :: forall e m . ( Monad m
                           , FromStringMaybe (PubKey 'Sign (Encryption e))
                           , Ord (PubKey 'Sign (Encryption e))
                           )
           => PeerConfig
           -> PeerData e -> m Bool

peerBanned (PeerConfig syn) pd = do

  flip runReaderT syn do
    bls <- cfgValue @PeerBlackListKey
    whs <- cfgValue @PeerWhiteListKey

    let blkeys = toKeys bls
    let wlkeys = toKeys (whs `Set.difference` bls)

    let k = view peerSignKey pd
    let blacklisted = k `Set.member` blkeys
    let whitelisted = Set.null wlkeys || (k `Set.member` wlkeys)
    pure $ blacklisted || not whitelisted


