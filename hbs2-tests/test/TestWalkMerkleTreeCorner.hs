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
import HBS2.Tests.MapStore
import HBS2.Tests.StoreAsMerkle

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
