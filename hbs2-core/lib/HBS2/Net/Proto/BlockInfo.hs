module HBS2.Net.Proto.BlockInfo where

import HBS2.Prelude.Plated
import HBS2.Net.Proto
import HBS2.Net.Proto.Sessions
import HBS2.Events
import HBS2.Hash


data BlockInfo e = GetBlockSize (Hash HbSync)
                 | NoBlock (Hash HbSync)
                 | BlockSize (Hash HbSync) Integer
                 deriving stock (Eq,Generic,Show)

type HasBlockEvent h e m = (Peer e, Hash h, Maybe Integer) -> m ()


instance Serialise (BlockInfo e)

blockSizeProto :: forall e m  . ( MonadIO m
                                , Response e (BlockInfo e) m
                                , EventEmitter e (BlockInfo e) m
                                )
               => GetBlockSize  HbSync m
               -> HasBlockEvent HbSync e m
               -> BlockInfo e
               -> m ()

blockSizeProto getBlockSize evHasBlock =
  \case
    GetBlockSize h -> do
      deferred (Proxy @(BlockInfo e))$ do
        getBlockSize h >>= \case
          Just size -> response (BlockSize @e h size)
          Nothing   -> response (NoBlock @e h)

    NoBlock h       -> do
      that <- thatPeer (Proxy @(BlockInfo e))
      evHasBlock ( that, h, Nothing )

    BlockSize h sz  -> do
      that <- thatPeer (Proxy @(BlockInfo e))
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

newtype instance Event e (BlockInfo e) =
  BlockSizeEvent (Peer e, Hash HbSync, Integer)
  deriving stock (Typeable)

