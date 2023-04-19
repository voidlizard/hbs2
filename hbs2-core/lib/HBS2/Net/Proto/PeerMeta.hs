module HBS2.Net.Proto.PeerMeta where

import HBS2.Base58
import HBS2.Events
import HBS2.Hash
import HBS2.Merkle
import HBS2.Net.Proto
import HBS2.Net.Proto.Peer
import HBS2.Net.Proto.Sessions
import HBS2.Prelude.Plated

import Codec.Serialise
import Control.Monad
import Data.ByteString ( ByteString )
import Data.ByteString.Lazy qualified as LBS
import Data.Functor
import Data.Maybe
import Data.Text.Encoding qualified as TE

data PeerMetaProto e
  = GetPeerMeta
  | ThePeerMeta AnnMetaData
  deriving stock (Eq,Generic,Show)

instance Serialise (PeerMetaProto e)


peerMetaProto :: forall e m  . ( MonadIO m
                                , Response e (PeerMetaProto e) m
                                , HasDeferred e (PeerMetaProto e) m
                                , EventEmitter e (PeerMetaProto e) m
                                , Sessions e (KnownPeer e) m
                                )
               => AnnMetaData
               -> PeerMetaProto e
               -> m ()

peerMetaProto peerMeta =
  \case
    GetPeerMeta -> do
      p <- thatPeer (Proxy @(PeerMetaProto e))
      auth <- find (KnownPeerKey p) id <&> isJust
      when auth do
        deferred (Proxy @(PeerMetaProto e)) do
          response (ThePeerMeta @e peerMeta)

    ThePeerMeta meta -> do
      that <- thatPeer (Proxy @(PeerMetaProto e))
      emit @e (PeerMetaEventKey that) (PeerMetaEvent meta)

newtype instance EventKey e (PeerMetaProto e) =
  PeerMetaEventKey (Peer e)
  deriving stock (Typeable, Generic)

deriving instance Eq (Peer e) => Eq (EventKey e (PeerMetaProto e))
deriving instance (Eq (Peer e), Hashable (Peer e)) => Hashable (EventKey e (PeerMetaProto e))

newtype instance Event e (PeerMetaProto e)
  = PeerMetaEvent AnnMetaData
  deriving stock (Typeable)

newtype PeerMeta = PeerMeta { unPeerMeta :: [(Text, ByteString)] }
  deriving stock (Generic)
  deriving newtype (Semigroup, Monoid)

instance Serialise PeerMeta

annMetaFromPeerMeta :: PeerMeta -> AnnMetaData
annMetaFromPeerMeta =
    ShortMetadata . TE.decodeUtf8 . toBase58 . LBS.toStrict . serialise

parsePeerMeta :: Text -> Maybe PeerMeta
parsePeerMeta = either (const Nothing) Just . deserialiseOrFail . LBS.fromStrict <=< fromBase58 . TE.encodeUtf8
