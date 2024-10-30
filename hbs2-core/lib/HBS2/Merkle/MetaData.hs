{-# Language AllowAmbiguousTypes #-}
{-# Language UndecidableInstances #-}
{-# Language FunctionalDependencies #-}
module HBS2.Merkle.MetaData where

import HBS2.Prelude
import HBS2.OrDie
import HBS2.Data.Types.Refs
import HBS2.Merkle
import HBS2.Hash
import HBS2.Storage
import HBS2.Data.Types.SmallEncryptedBlock
import HBS2.Net.Auth.GroupKeySymm as G
import HBS2.Storage.Operations.Class
import HBS2.Storage.Operations.ByteString (readChunkedBS)

import Data.Coerce
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy (ByteString)
import Data.ByteString qualified as BS
import Codec.Serialise
import Data.Text.Encoding qualified as TE
import Control.Exception
import Control.Monad.Except
import Control.Monad.Trans.Maybe

-- import UnliftIO

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


loadGroupKeyForTree :: ( ForGroupKeySymm s
                       , MonadIO m
                       )
                  => AnyStorage
                  -> HashRef
                  -> m (Maybe (GroupKey 'Symm s))

loadGroupKeyForTree sto h = do

  runMaybeT do

    headBlock <- getBlock sto (fromHashRef h)
                  >>= toMPlus
                  <&> deserialiseOrFail @(MTreeAnn [HashRef])
                  >>= toMPlus

    gkh <- case _mtaCrypt headBlock of
             (EncryptGroupNaClSymm h1 _) -> pure (HashRef h1)
             _ -> mzero

    G.loadGroupKeyMaybe sto gkh >>= toMPlus

class ForGroupKeySymm s => ForEncryptedTree s a  | a -> s where
  getNonce     :: Monad m => a -> m BS.ByteString

  getContent   :: a -> ByteString
  getMetaData  :: a -> Text

  getBlockSize :: a -> Int
  getBlockSize _ = 256 * 1024

data DefaultEncryptedTreeSource (s :: CryptoScheme) = DefSource Text LBS.ByteString

instance ForGroupKeySymm s => ForEncryptedTree s (DefaultEncryptedTreeSource s) where
  getContent (DefSource _ lbs) = lbs

  getMetaData (DefSource m _) = m

  getNonce (DefSource _ lbs) = do
    let s0 = LBS.take ( 1024 * 1024 ) lbs
    let (HbSyncHash nonce) = hashObject @HbSync s0
    pure nonce

createEncryptedTree :: forall s a m . ( ForEncryptedTree s a, MonadIO m )
                     => AnyStorage
                     -> GroupSecret
                     -> GroupKey 'Symm s
                     -> a
                     -> m HashRef
createEncryptedTree sto gks gk what = do


  nonce <- getNonce @s what
  let lbs = getContent @s what

  let segments = readChunkedBS lbs (getBlockSize what)

  seb <- encryptBlock sto gks (Right gk) (Just nonce) (ShortMetadata (getMetaData @s what))

  hmeta <- putBlock sto (serialise seb)
             >>= orThrow StorageError

  let source = ToEncryptSymmBS gks (Right gk) nonce segments  (AnnHashRef hmeta) Nothing

  runExceptT (writeAsMerkle sto source <&> HashRef) >>= orThrowPassIO


