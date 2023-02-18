module HBS2.Data.Detect where

import HBS2.Prelude.Plated
import HBS2.Hash
import HBS2.Data.Types
import HBS2.Merkle

import Codec.Serialise (deserialiseOrFail)
import Data.ByteString.Lazy (ByteString)
import Data.Either
import Data.Function
import Data.Functor

data BlobType =  Merkle (Hash HbSync)
               | MerkleAnn (MTreeAnn [HashRef])
               | Blob (Hash HbSync)
               deriving (Show,Data)


tryDetect :: Hash HbSync -> ByteString -> BlobType
tryDetect hash obj = rights [mbAnn, mbMerkle] & headDef orBlob

  where
    mbMerkle = deserialiseOrFail @(MTree [HashRef]) obj >> pure (Merkle hash)
    mbAnn   = deserialiseOrFail obj <&> MerkleAnn
    orBlob   = Blob hash

