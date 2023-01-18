module HBS2.Net.Proto.BlockInfo where

import HBS2.Prelude.Plated
import HBS2.Net.Proto
import HBS2.Hash

import Codec.Serialise ()

data BlockSize e = GetBlockSize (Hash HbSync)
                 | NoBlock (Hash HbSync)
                 | BlockSize (Hash HbSync) Integer
                 deriving stock (Eq,Generic,Show)

type HasBlockEvent h e m = (Peer e, Hash h, Maybe Integer) -> m ()

type GetBlockSize h m = Hash h -> m (Maybe Integer)

instance Serialise (BlockSize e)

blockSizeProto :: forall e m  . ( MonadIO m
                                , Response e (BlockSize e) m
                                )
               => GetBlockSize  HbSync m
               -> HasBlockEvent HbSync e m
               -> BlockSize e
               -> m ()

blockSizeProto getBlockSize evHasBlock =
  \case
    GetBlockSize h -> do
      deferred (Proxy @(BlockSize e))$ do
        getBlockSize h >>= \case
          Just size -> response (BlockSize @e h size)
          Nothing   -> response (NoBlock @e h)

    NoBlock h       -> do
      that <- thatPeer (Proxy @(BlockSize e))
      evHasBlock ( that, h, Nothing )

    BlockSize h sz  -> do
      that <- thatPeer (Proxy @(BlockSize e))
      evHasBlock ( that, h, Just sz )


