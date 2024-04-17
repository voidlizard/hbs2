{-# Language AllowAmbiguousTypes #-}
module HBS2.Storage.Operations.Class where

import HBS2.Prelude.Plated
import HBS2.Hash
import HBS2.Storage

import Control.Exception
import Data.Kind

data OperationError =
    StorageError
  | CryptoError
  | SignCheckError
  | DecryptError
  | DecryptionError
  | MissedBlockError
  | UnsupportedFormat
  | IncompleteData
  | GroupKeyNotFound Int
  deriving (Generic,Show,Data,Typeable)

instance Exception OperationError

class (MonadIO m, Storage storage hash (ToBlockW s) m) => MerkleWriter s hash storage m where
  type family ToBlockW s :: Type
  writeAsMerkle :: storage -> s -> m (Hash hash)

class (MonadIO m, Storage storage h (ToBlockR s) m) => MerkleReader s storage h m where
  data family TreeKey s :: Type
  type family ToBlockR s :: Type
  type family ReadResult s :: Type
  readFromMerkle :: storage -> TreeKey s -> m (ReadResult s)

