{-# Language RankNTypes #-}
module HBS2.Net.Proto.BlockChunks where

import HBS2.Events
import HBS2.Hash
import HBS2.Net.Proto
import HBS2.Prelude.Plated
import HBS2.Storage

import Data.Word
import Prettyprinter
import Data.ByteString.Lazy (ByteString)
import Data.Foldable

newtype ChunkSize = ChunkSize Word16
                    deriving newtype (Num,Enum,Real,Integral,Pretty)
                    deriving stock (Eq,Ord,Show,Data,Generic)


newtype ChunkNum = ChunkNum Word16
                   deriving newtype (Num,Enum,Real,Integral,Pretty)
                   deriving stock (Eq,Ord,Show,Data,Generic)


type OnBlockReady h m = Hash h -> m ()


type GetBlockChunk h m = Hash h -> Offset -> Size -> m (Maybe ByteString)


type AcceptChunk h e m = Response e (BlockChunks e) m
                       => ( Cookie e, Peer e, Hash HbSync, ChunkNum, ByteString ) -> m ()

type GetBlockHash h e m = (Peer e, Cookie e) -> m (Maybe (Hash h))

data BlockChunksI e m =
  BlockChunksI
  { blkSize         :: GetBlockSize HbSync m
  , blkChunk        :: GetBlockChunk HbSync m
  , blkGetHash      :: GetBlockHash HbSync e m
  , blkAcceptChunk  :: AcceptChunk HbSync e m
  }


data BlockChunks e = BlockChunks (Cookie e) (BlockChunksProto e)
                     deriving stock (Generic)

data BlockChunksProto e = BlockGetAllChunks (Hash HbSync) ChunkSize
                        | BlockNoChunks
                        | BlockChunk ChunkNum ByteString
                        | BlockLost
                        deriving stock (Generic)


instance HasCookie e (BlockChunks e) where
  type instance Cookie e = Word32
  getCookie (BlockChunks c _) = Just c

instance Serialise ChunkSize
instance Serialise ChunkNum
instance Serialise (BlockChunksProto e)
instance Serialise (BlockChunks e)


newtype instance EventKey e (BlockChunks e) =
  BlockChunksEventKey (Hash HbSync)
  deriving stock (Typeable, Eq)
  deriving newtype (Hashable)

newtype instance Event e (BlockChunks e) =
  BlockReady (Hash HbSync)
  deriving stock (Typeable)

blockChunksProto :: forall e m  . ( MonadIO m
                                  , Response e (BlockChunks e) m
                                  , Pretty (Peer e)
                                  )
                 => BlockChunksI e m
                 -> BlockChunks e
                 -> m ()

blockChunksProto adapter (BlockChunks c p) =
  case p of
    BlockGetAllChunks h size -> deferred proto do
      bsz' <- blkSize adapter h

      maybe1 bsz' (pure ()) $ \bsz -> do

        let offsets' = calcChunks bsz (fromIntegral size) :: [(Offset, Size)]
        let offsets = zip offsets' [0..]

        for_ offsets $ \((o,sz),i) -> do
          chunk <- blkChunk adapter h o sz
          maybe (pure ()) (response_ . BlockChunk @e i) chunk

    BlockChunk n bs -> do
      who <- thatPeer proto
      h <- blkGetHash adapter (who, c)

      maybe1 h (response_ (BlockLost @e)) $ \hh -> do
        void $ blkAcceptChunk adapter (c, who, hh, n, bs)

    BlockNoChunks {} -> do
      -- TODO: notification
      pure ()

    BlockLost{} -> do
      pure ()

  where
    proto = Proxy @(BlockChunks e)
    response_ pt = response (BlockChunks c pt)



