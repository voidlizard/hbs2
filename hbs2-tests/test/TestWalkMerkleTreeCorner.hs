module Main where

import Control.Monad
import Control.Monad.State as State
import Data.ByteString.Lazy (ByteString)
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.String.Conversions (cs)

import HBS2.Base58
import HBS2.Data.Types.Refs
import HBS2.Hash
import HBS2.Merkle
import HBS2.Merkle.Walk

main :: IO ()
main = do
    -- Just "3.14"
    print $ runMapStoreM do
        hr <- mapStorePutBlock "3.14"
        mapStoreReadBlock hr

    print $ runMapStoreM do
        h <- mapStorePutBlock "1853635"
        (,)
            -- Left (MerkleDeserialiseFailure "1337jkagZ5Knihh82JaXqzB3qAX4K298DuNp7jx5tGmw" (DeserialiseFailure 0 "expected list len"))
            <$> catFromMerkle mapStoreReadBlock h
            -- Right ""
            <*> catFromMerkle1 mapStoreReadBlock h

    -- (Right "123456789abcdefgh",Right "123456789abcdefgh")
    -- (Left (MerkleHashNotFound "h3VBGX1u6JHDjR38z97xo6S3ruiynkc1kygGZgRVcit")
    -- ,Left (MerkleHashNotFound "h3VBGX1u6JHDjR38z97xo6S3ruiynkc1kygGZgRVcit"))
    mapM_ print $ runMapStoreM do
        MerkleHash h <- storeAsMerkle mapStorePutBlock do
            ((cs . (: [])) <$> "123456789abcdefgh") :: [ByteString]
        r1 <- (,) <$> catFromMerkle mapStoreReadBlock h <*> catFromMerkle1 mapStoreReadBlock h

        mapStoreDeleteBlock . (!! 2) =<< State.gets Map.keys
        r2 <- (,) <$> catFromMerkle mapStoreReadBlock h <*> catFromMerkle1 mapStoreReadBlock h

        pure [r1, r2]

type MapStore = Map (Hash HbSync) ByteString
newtype MapStoreM a = MapStoreM {unMapStoreM :: State MapStore a}
    deriving newtype (Functor, Applicative, Monad, MonadState MapStore)

runMapStoreM :: MapStoreM a -> a
runMapStoreM = flip evalState mempty . unMapStoreM

mapStorePutBlock :: ByteString -> MapStoreM (Hash HbSync)
mapStorePutBlock bs =
    h <$ State.modify (Map.insert h bs)
  where
    h = hashObject bs

mapStoreReadBlock :: (Hash HbSync) -> MapStoreM (Maybe ByteString)
mapStoreReadBlock h =
    State.gets (Map.lookup h)

mapStoreDeleteBlock :: (Hash HbSync) -> MapStoreM ()
mapStoreDeleteBlock h =
    State.modify (Map.delete h)

---

class (Monad m) => StoreAsMerkle m h b a where
    storeAsMerkle :: (b -> m h) -> a -> m MerkleHash

instance StoreAsMerkle MapStoreM (Hash HbSync) ByteString [ByteString] where
    storeAsMerkle = \putB bs -> do
        hashes <- mapM putB bs
        storeAsMerkle putB (HashRef <$> hashes)

instance StoreAsMerkle MapStoreM (Hash HbSync) ByteString [HashRef] where
    storeAsMerkle = \putB hrs -> do
        let
            treeChildNum = 3
            hashListChunk = 2
            ptree = toPTree (MaxSize hashListChunk) (MaxNum treeChildNum) hrs
        MerkleHash <$> do
            makeMerkle 0 ptree \(_, _, bs) -> (void . putB) bs
