{-# Language TemplateHaskell #-}
{-# Language UndecidableInstances #-}
module HBS2.Net.Proto.BlockAnnounce where

import HBS2.Prelude.Plated
import HBS2.Net.Proto
import HBS2.Events
import HBS2.Hash

import Lens.Micro.Platform
import Type.Reflection (someTypeRep)
import Data.Hashable
import Data.ByteString.Lazy (ByteString)
import Data.Word
import Codec.Serialise()


data BlockInfoMeta = NoBlockInfoMeta
                   | BlockInfoMetaShort ByteString
                   | BlockInfoMetaRef (Hash HbSync)
                   deriving stock (Eq,Generic,Show)

instance Serialise BlockInfoMeta

data BlockAnnounceInfo e =
  BlockAnnounceInfo
  { _biNonce :: BlockInfoNonce
  , _biMeta  :: BlockInfoMeta
  , _biSize  :: Integer
  , _biHash  :: Hash HbSync
  }
  deriving stock (Eq,Generic,Show)

newtype BlockInfoNonce = BlockInfoNonce Word64
                         deriving newtype (Num,Enum,Real,Integral)
                         deriving stock (Ord,Eq,Generic,Show)

instance Serialise BlockInfoNonce
instance Serialise (BlockAnnounceInfo e)


data BlockAnnounce e = BlockAnnounce PeerNonce (BlockAnnounceInfo e)
                       deriving stock (Generic)

deriving instance (Show (Nonce ())) => Show (BlockAnnounce e)

instance Serialise PeerNonce => Serialise (BlockAnnounce e)


makeLenses ''BlockAnnounceInfo


blockAnnounceProto :: forall e m  . ( MonadIO m
                                    , EventEmitter e (BlockAnnounce e) m
                                    , Response e (BlockAnnounce e) m
                                    ) => BlockAnnounce e -> m ()
blockAnnounceProto =
  \case
    BlockAnnounce n info -> do
      that <- thatPeer @(BlockAnnounce e)
      emit @e BlockAnnounceInfoKey (BlockAnnounceEvent that info n)

data instance EventKey e (BlockAnnounce e) =
  BlockAnnounceInfoKey
  deriving stock (Typeable, Eq,Generic)

data instance Event e (BlockAnnounce e) =
  BlockAnnounceEvent (Peer e) (BlockAnnounceInfo e) PeerNonce
  deriving stock (Typeable)

instance Typeable (BlockAnnounceInfo e) => Hashable (EventKey e (BlockAnnounce e)) where
  hashWithSalt salt _ = hashWithSalt salt (someTypeRep p)
    where
      p = Proxy @(BlockAnnounceInfo e)

instance EventType ( Event e ( BlockAnnounce e) ) where
  isPersistent = True


