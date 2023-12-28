module HBS2.Net.Proto.BlockInfo where

import HBS2.Prelude.Plated
import HBS2.Net.Proto
import HBS2.Net.Proto.Peer
import HBS2.Net.Proto.Sessions
import HBS2.Events
import HBS2.Hash

import Data.Maybe

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

    NoBlock h       -> do
      that <- thatPeer @proto
      emit @e (BlockSizeEventKey h) (NoBlockEvent that)
      evHasBlock ( that, h, Nothing )

    BlockSize h sz  -> do
      that <- thatPeer @proto
      emit @e (BlockSizeEventKey h) (BlockSizeEvent (that, h, sz))
      evHasBlock ( that, h, Just sz )

newtype instance SessionKey e (BlockInfo e) =
  BlockSizeKey (Hash HbSync)
  deriving stock (Typeable,Eq,Show)
  deriving newtype (Hashable,IsString)


newtype instance EventKey e (BlockInfo e) =
  BlockSizeEventKey (Hash HbSync)
  deriving stock (Typeable, Eq,Generic)

deriving  instance Hashable (EventKey e (BlockInfo e))

data instance Event e (BlockInfo e) =
    BlockSizeEvent (Peer e, Hash HbSync, Integer)
  | NoBlockEvent (Peer e)
  deriving stock (Typeable)


