module HBS2.Tests.StoreAsMerkle where

import Control.Monad
import Data.ByteString.Lazy qualified as LBS

import HBS2.Data.Types.Refs
import HBS2.Hash
import HBS2.Merkle
import HBS2.Tests.MapStore

class (Monad m) => StoreAsMerkle m h b a where
    storeAsMerkle :: (b -> m h) -> a -> m MerkleHash

instance
    (Monad m, Serialise a)
    => StoreAsMerkle m (Hash HbSync) LBS.ByteString (PTree a)
    where
    storeAsMerkle putB ptree = fmap MerkleHash do
        makeMerkle 0 ptree \(_, _, bs) -> (void . putB) bs

-- (Monad m, Serialise a)
-- => StoreAsMerkle m (Hash HbSync) LBS.ByteString [a]
instance
    (Monad m)
    => StoreAsMerkle m (Hash HbSync) LBS.ByteString [HashRef]
    where
    storeAsMerkle putB hrs = storeAsMerkle putB do
        toPTree (MaxSize hashListChunk) (MaxNum treeChildNum) hrs
      where
        treeChildNum = 3
        hashListChunk = 2

instance
    (Monad m)
    => StoreAsMerkle m (Hash HbSync) LBS.ByteString [LBS.ByteString]
    where
    storeAsMerkle putB =
        storeAsMerkle putB . fmap HashRef <=< mapM putB
