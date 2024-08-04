module HBS2.Merkle.MetaData where

import HBS2.Prelude
import HBS2.OrDie
import HBS2.Data.Types.Refs
import HBS2.Merkle
import HBS2.Storage
import HBS2.Data.Types.SmallEncryptedBlock
import HBS2.Net.Auth.GroupKeySymm as G
import HBS2.Storage.Operations.Class

import Data.Coerce
import Data.ByteString.Lazy qualified as LBS
import Codec.Serialise
import Data.Text.Encoding qualified as TE
import Control.Monad.Except

{- HLINT ignore "Functor law" -}

extractMetaData :: forall s m . (MonadIO m, ForGroupKeySymm s, MonadError OperationError m)

                => (GroupKey 'Symm s -> m (Maybe GroupSecret))
                -> AnyStorage
                -> HashRef
                -> m Text
extractMetaData fk sto hash = do

    headBlock <- getBlock sto (coerce hash)
                   >>= orThrowError MissedBlockError
                   <&> deserialiseOrFail @(MTreeAnn [HashRef])
                   >>= orThrowError UnsupportedFormat

    case headBlock of
      MTreeAnn { _mtaMeta = ShortMetadata s } -> do
        pure  s

      MTreeAnn { _mtaMeta = AnnHashRef h, _mtaCrypt = NullEncryption } -> do
        getBlock sto h
           >>= orThrowError MissedBlockError
           <&> LBS.toStrict
           <&> TE.decodeUtf8

      MTreeAnn { _mtaMeta = AnnHashRef h } -> do
        getBlock sto h
           >>= orThrowError MissedBlockError
           <&> deserialiseOrFail @(SmallEncryptedBlock AnnMetaData)
           >>= orThrowError UnsupportedFormat
           >>= G.decryptBlock @_ @s sto fk
           >>= \case
            ShortMetadata s -> pure s
            _ -> throwError UnsupportedFormat

      _ -> throwError UnsupportedFormat


