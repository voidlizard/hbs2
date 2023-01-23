module HBS2.Data.Detect where

import HBS2.Prelude.Plated
import HBS2.Hash
import HBS2.Data.Types
import HBS2.Merkle

import Codec.Serialise (deserialiseOrFail)
import Data.ByteString.Lazy (ByteString)
import Data.Either
import Data.Function

data BlobType =  Merkle (Hash HbSync)
               | AnnRef (Hash HbSync)
               | Blob (Hash HbSync)
               deriving (Show,Data)


tryDetect :: Hash HbSync -> ByteString -> BlobType
tryDetect hash obj = rights [mbLink, mbMerkle] & headDef orBlob

  where
    mbLink   = deserialiseOrFail @AnnotatedHashRef obj >> pure (AnnRef hash)
    mbMerkle = deserialiseOrFail @(MTree [HashRef]) obj >> pure (Merkle hash)
    orBlob   = Blob hash

