{-# Language RankNTypes #-}
module HBS2.Net.Proto.BlockChunks where

import HBS2.Events
import HBS2.Hash
import HBS2.Clock
import HBS2.Net.Proto
import HBS2.Net.Proto.Peer
import HBS2.Prelude.Plated
import HBS2.Storage
import HBS2.Actors.Peer
import HBS2.Net.Proto.Sessions

import Data.Functor
import Data.Word
import Prettyprinter
import Data.ByteString.Lazy (ByteString)
import Data.Foldable hiding (find)
import Data.Maybe

import System.Random.Shuffle

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
                     deriving stock (Generic, Show)

data BlockChunksProto e = BlockGetAllChunks (Hash HbSync) ChunkSize
                        | BlockGetChunks (Hash HbSync) ChunkSize Word32 Word32
                        | BlockNoChunks
                        | BlockChunk ChunkNum ByteString
                        | BlockLost
                        deriving stock (Generic, Show)


instance HasCookie e (BlockChunks e) where
  type instance Cookie e = Word32
  getCookie (BlockChunks c _) = Just c

instance Serialise ChunkSize
instance Serialise ChunkNum
instance Serialise (BlockChunksProto e)
instance Serialise (BlockChunks e)


newtype instance EventKey e (BlockChunks e) =
  BlockChunksEventKey (Cookie e, Hash HbSync)
  deriving stock (Typeable, Eq, Generic)

deriving instance Hashable (EventKey e (BlockChunks e))

data instance Event e (BlockChunks e) =
    BlockReady      (Hash HbSync)
  | BlockChunksLost (Hash HbSync)
  deriving stock (Typeable)

blockChunksProto :: forall e m  . ( MonadIO m
                                  , Response e (BlockChunks e) m
                                  , HasDeferred e (BlockChunks e) m
                                  , HasOwnPeer e m
                                  , Sessions e (KnownPeer e) m
                                  , Pretty (Peer e)
                                  )
                 => BlockChunksI e m
                 -> BlockChunks e
                 -> m ()

blockChunksProto adapter (BlockChunks c p) = do

  peer <- thatPeer (Proxy @(BlockChunks e))
  auth <- find (KnownPeerKey peer) id <&> isJust

  case p of

    BlockGetChunks h size n1 num | auth -> do

      bsz' <- blkSize adapter h

      maybe1 bsz' (pure ()) $ \bsz -> do

        let offsets' = calcChunks bsz (fromIntegral size) :: [(Offset, Size)]
        let offsets = take (fromIntegral num) $ drop (fromIntegral n1) $ zip offsets' [0..]

        -- liftIO $ print $ "sending " <+> pretty (length offsets)
        --                             <+> "chunks for block"
        --                             <+> pretty h

        -- for_ offsets $ \((o,sz),i) -> deferred proto do
        for_ offsets $ \((o,sz),i) -> deferred proto do
          -- liftIO $ print $ "send chunk " <+> pretty i <+> pretty sz
          chunk <- blkChunk adapter h o sz
          maybe (pure ()) (response_ . BlockChunk @e i) chunk

    BlockGetAllChunks h size | auth -> do

      me <- ownPeer @e
      who <- thatPeer proto

      bsz' <- blkSize adapter h

      maybe1 bsz' (pure ()) $ \bsz -> do

        let offsets' = calcChunks bsz (fromIntegral size) :: [(Offset, Size)]
        let offsets = zip offsets' [0..]

        -- liftIO $ print $ "sending " <+> pretty (length offsets)
        --                             <+> "chunks for block"
        --                             <+> pretty h

        for_ offsets $ \((o,sz),i) -> deferred proto do
          chunk <- blkChunk adapter h o sz
          maybe (pure ()) (response_ . BlockChunk @e i) chunk

    BlockChunk n bs | auth -> deferred proto do
      who <- thatPeer proto
      me <- ownPeer @e
      h <- blkGetHash adapter (who, c)

      maybe1 h (response_ (BlockLost @e)) $ \hh -> do
        void $ blkAcceptChunk adapter (c, who, hh, n, bs)

    BlockNoChunks {} -> do
      -- TODO: notification
      pure ()

    BlockLost{} -> do
      -- liftIO $ print "GOT BLOCK LOST MESSAGE - means IO ERROR"
      pure ()

    _ -> do
      pure ()

  where
    proto = Proxy @(BlockChunks e)
    response_ pt = response (BlockChunks c pt)


