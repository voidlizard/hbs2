{-# Language UndecidableInstances #-}
module HBS2.Data.Types.Peer where

import Codec.Serialise
import Data.ByteString qualified as BS
import Data.Hashable
import Lens.Micro.Platform

import HBS2.Prelude
import HBS2.Data.Types.Crypto
import HBS2.Net.Auth.Credentials
import HBS2.Net.Proto.Types


type PingSign e = Signature (Encryption e)
type PingNonce = BS.ByteString

data PeerData e =
  PeerData
  { _peerSignKey  :: PubKey 'Sign (Encryption e)
  , _peerOwnNonce :: PeerNonce -- TODO: to use this field to detect if it's own peer to avoid loops
  }
  deriving stock (Typeable,Generic)

deriving instance
    ( Eq (PubKey 'Sign (Encryption e))
    , Eq PeerNonce
    )
    => Eq (PeerData e)

instance
    ( Hashable (PubKey 'Sign (Encryption e))
    , Hashable PeerNonce
    )
    => Hashable (PeerData e) where
  hashWithSalt s PeerData{..} = hashWithSalt s (_peerOwnNonce)

deriving instance
    ( Show (PubKey 'Sign (Encryption e))
    , Show PeerNonce
    )
    => Show (PeerData e)

makeLenses 'PeerData

