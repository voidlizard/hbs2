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
               | AnnRef (Hash HbSync)
               | SeqRef SequentialRef
               | Blob (Hash HbSync)
               deriving (Show,Data)


tryDetect :: Hash HbSync -> ByteString -> BlobType
tryDetect hash obj = rights [mbAnn, mbLink, mbMerkle, mbSeq] & headDef orBlob

  where
    mbLink   = deserialiseOrFail @AnnotatedHashRef obj >> pure (AnnRef hash)
    mbMerkle = deserialiseOrFail @(MTree [HashRef]) obj >> pure (Merkle hash)
    mbSeq    = deserialiseOrFail @SequentialRef obj <&> SeqRef
    mbAnn    = deserialiseOrFail obj <&> MerkleAnn
    orBlob   = Blob hash

