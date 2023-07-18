{-# Language UndecidableInstances #-}
module HBS2.Data.Types.Peer where

import Data.ByteString qualified as BS
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
    ( Show (PubKey 'Sign (Encryption e))
    , Show (Nonce ())
    )
    => Show (PeerData e)

makeLenses 'PeerData

