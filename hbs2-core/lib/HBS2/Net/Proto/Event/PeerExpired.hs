module HBS2.Net.Proto.Event.PeerExpired where

import HBS2.Clock
import HBS2.Events
import HBS2.Net.Proto
import HBS2.Prelude.Plated

data PeerExpires = PeerExpires

data instance EventKey e PeerExpires =
  PeerExpiredEventKey
  deriving stock (Typeable, Eq, Generic)

data instance Event e PeerExpires =
  PeerExpiredEvent (Peer e)
  deriving stock (Typeable)

instance EventType (Event e PeerExpires) where
  isPersistent = True

instance Expires (EventKey e PeerExpires) where
  expiresIn _ = Nothing

instance Hashable (EventKey e PeerExpires)

--instance ( Serialise (PubKey 'Sign (Encryption e))
--         , Serialise (PubKey 'Encrypt (Encryption e))
--         , Serialise (Signature (Encryption e))
--         , Serialise PeerNonce
--         )

--  => Serialise PeerExpires

