module HBS2.Peer.Proto.PeerMeta where

import HBS2.Base58
import HBS2.Clock
import HBS2.Events
import HBS2.Hash
import HBS2.Merkle
import HBS2.Net.Proto
import HBS2.Peer.Proto.Peer
import HBS2.Net.Proto.Sessions
import HBS2.Prelude.Plated

import HBS2.Actors.Peer.Types

import HBS2.System.Logger.Simple

import Codec.Serialise
import Control.Monad
import Data.ByteString ( ByteString )
import Data.ByteString.Lazy qualified as LBS
import Data.Functor
import Data.Maybe
import Data.Text.Encoding qualified as TE

instance HasProtocol L4Proto (PeerMetaProto L4Proto) where
  type instance ProtocolId (PeerMetaProto L4Proto) = 9
  type instance Encoded L4Proto = LBS.ByteString
  decode = deserialiseCustom
  encode = serialise

  -- FIXME: real-period
  requestPeriodLim = ReqLimPerMessage 0.25

instance Expires (EventKey L4Proto (PeerMetaProto L4Proto)) where
  expiresIn _ = Just 600


data PeerMetaProto e
  = GetPeerMeta
  | ThePeerMeta AnnMetaData
  deriving stock (Eq,Generic,Show)

instance Serialise (PeerMetaProto e)


peerMetaProto :: forall e m proto  . ( MonadIO m
                                     , Response e proto m
                                     , HasDeferred proto e m
                                     , EventEmitter e proto m
                                     , Sessions e (KnownPeer e) m
                                     , Pretty (Peer e)
                                     , proto ~ PeerMetaProto e
                                     )
               => AnnMetaData
               -> PeerMetaProto e
               -> m ()

peerMetaProto peerMeta =
  \case
    GetPeerMeta -> do
      p <- thatPeer @proto
      auth <- find (KnownPeerKey p) id <&> isJust
      when auth do
        debug $ "PEER META: ANSWERING" <+> pretty p <+> viaShow peerMeta
        deferred @proto do
          response (ThePeerMeta @e peerMeta)

    ThePeerMeta meta -> do
      that <- thatPeer @proto
      debug $ "GOT PEER META FROM" <+> pretty that <+> viaShow meta
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
  deriving newtype (Semigroup, Monoid, Show)

instance Serialise PeerMeta

annMetaFromPeerMeta :: PeerMeta -> AnnMetaData
annMetaFromPeerMeta =
    ShortMetadata . TE.decodeUtf8 . toBase58 . LBS.toStrict . serialise

parsePeerMeta :: Text -> Maybe PeerMeta
parsePeerMeta = either (const Nothing) Just . deserialiseOrFail . LBS.fromStrict <=< fromBase58 . TE.encodeUtf8
