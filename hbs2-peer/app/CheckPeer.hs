module CheckPeer where

import HBS2.Prelude.Plated
import HBS2.Data.Types.Peer
import HBS2.Net.Proto.Types

import PeerTypes
import PeerConfig

import Data.Set qualified as Set
import Data.Set (Set)
import Lens.Micro.Platform


data PeerBlackListKey
data PeerWhiteListKey

instance HasCfgKey PeerBlackListKey (Set String) where
  key = "blacklist"

instance HasCfgKey PeerWhiteListKey (Set String) where
  key = "whitelist"

peerBanned :: forall e m . ( Monad m, FromStringMaybe (PubKey 'Sign (Encryption e))
                           , Ord (PubKey 'Sign (Encryption e))
                           )
           => PeerConfig
           -> PeerData e -> m Bool

peerBanned conf pd = do

  let bls = cfgValue @PeerBlackListKey conf :: Set String
  let whs = cfgValue @PeerWhiteListKey conf :: Set String
  let blkeys = toKeys bls
  let wlkeys = toKeys (whs `Set.difference` bls)


  let k = view peerSignKey pd
  let blacklisted = k `Set.member` blkeys
  let whitelisted = Set.null wlkeys || (k `Set.member` wlkeys)
  pure $ blacklisted || not whitelisted


