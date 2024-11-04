{-# Language UndecidableInstances #-}
module HBS2.Peer.Proto.BlockInfo where

import HBS2.Prelude.Plated
import HBS2.Net.Proto
import HBS2.Peer.Proto.Peer
import HBS2.Net.Proto.Sessions
import HBS2.Events
import HBS2.Hash

import HBS2.System.Logger.Simple

import Data.Hashable
import Data.Maybe
import Data.ByteString (ByteString)

data BlockInfo e = GetBlockSize (Hash HbSync)
                 | NoBlock (Hash HbSync)
                 | BlockSize (Hash HbSync) Integer
                 deriving stock (Eq,Generic,Show)

type HasBlockEvent h e m = (Peer e, Hash h, Maybe Integer) -> m ()


instance Serialise (BlockInfo e)

blockSizeProto :: forall e m proto . ( MonadIO m
                                     , Response e proto m
                                     , HasDeferred proto e m
                                     , EventEmitter e proto m
                                     , EventEmitter e (AnyBlockSizeEvent e) m
                                     , Sessions e (KnownPeer e) m
                                     , proto ~ BlockInfo e
                                     )
               => GetBlockSize  HbSync m
               -> HasBlockEvent HbSync e m
               -> ( (Peer e, Hash HbSync) -> m () )
               -> BlockInfo e
               -> m ()

-- FIXME: with-auth-combinator
blockSizeProto getBlockSize evHasBlock onNoBlock =
  \case
    GetBlockSize h -> do
      -- liftIO $ print "GetBlockSize"
      p <- thatPeer @proto
      auth <- find (KnownPeerKey p) id <&> isJust
      when auth do
        deferred @proto $ do
          getBlockSize h >>= \case
            Just size -> response (BlockSize @e h size)
            Nothing   -> do
              onNoBlock (p, h)
              response (NoBlock @e h)

    NoBlock h -> deferred @proto do
      that <- thatPeer @proto
      emit @e (BlockSizeEventKey that) (NoBlockEvent (that, h))
      emit @e AnyBlockSizeEventKey (AnyBlockSizeEvent h Nothing that)
      evHasBlock ( that, h, Nothing )

    BlockSize h sz  -> deferred @proto do
      that <- thatPeer @proto
      emit @e (BlockSizeEventKey @e that) (BlockSizeEvent (that, h, sz))
      emit @e AnyBlockSizeEventKey (AnyBlockSizeEvent h (Just sz) that)
      evHasBlock ( that, h, Just sz )

data AnyBlockSizeEvent e

data instance EventKey e (AnyBlockSizeEvent e) =
  AnyBlockSizeEventKey
  deriving stock (Typeable, Generic, Eq)

instance Hashable (EventKey e (AnyBlockSizeEvent e)) where
  hashWithSalt s _ = hashWithSalt s ("AnyBlockSizeEventKey_1730696922" :: ByteString)

data instance Event e (AnyBlockSizeEvent e) =
  AnyBlockSizeEvent
  { anyBlockSizeHash :: Hash HbSync
  , anyBlockSize     :: Maybe Integer
  , anyBlockSizePeer :: Peer e
  }
  deriving stock (Generic,Typeable)

newtype instance SessionKey e (BlockInfo e) =
  BlockSizeKey (Hash HbSync)
  deriving stock (Typeable,Eq,Show)
  deriving newtype (Hashable,IsString)

newtype instance EventKey e (BlockInfo e) =
  BlockSizeEventKey (Peer e)
  deriving stock (Typeable, Generic)

deriving stock instance Eq (Peer e) => Eq (EventKey e (BlockInfo e))

instance (Eq (Peer e), Hashable (Peer e)) => Hashable (EventKey e (BlockInfo e))

data instance Event e (BlockInfo e) =
    BlockSizeEvent (Peer e, Hash HbSync, Integer)
  | NoBlockEvent (Peer e, Hash HbSync)
  deriving stock (Typeable)


