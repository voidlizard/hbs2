{-# OPTIONS_GHC -Wno-orphans #-}
module HBS2.Storage.NCQ3.Internal.Class where

import HBS2.Data.Types.Refs
import HBS2.Hash
import HBS2.Storage
import HBS2.Storage.NCQ3.Internal.Prelude
import HBS2.Storage.NCQ3.Internal.Types
import HBS2.Storage.NCQ3.Internal.Fossil
import HBS2.Storage.NCQ3.Internal.Index
import HBS2.Storage.NCQ3.Internal


import Data.ByteString.Lazy qualified as LBS
import Data.ByteString      qualified as BS
import Control.Monad.Trans.Maybe



instance MonadUnliftIO m => Storage NCQStorage HbSync LBS.ByteString m where
  hasBlock     sto h   = ncqStorageHasBlock sto (coerce h)

  putBlock     sto lbs = fmap coerce <$> ncqPutBlock sto lbs

  enqueueBlock sto lbs = fmap coerce <$> ncqTossBlock sto lbs

  getBlock sto h = runMaybeT $ do
    bs <- MaybeT (ncqStorageGetBlock sto (coerce h))
    pure (LBS.fromStrict bs)

  delBlock sto = ncqStorageDelBlock sto . coerce

  updateRef sto k v =
    ncqStorageSetRef sto (HashRef $ hashObject k) (coerce v)

  getRef sto k =
    ncqStorageGetRef sto (HashRef $ hashObject k) <&> fmap coerce

  delRef sto k =
    ncqStorageDelRef sto (HashRef $ hashObject k)

  getChunk sto h off size = runMaybeT $ do
    bs <- MaybeT (ncqStorageGetBlock sto (coerce h))
    let lbs   = LBS.fromStrict bs
        chunk = LBS.take (fromIntegral size) $ LBS.drop (fromIntegral off) lbs
    pure chunk


ncqStorageHasBlock :: MonadUnliftIO m
            => NCQStorage
            -> HashRef
            -> m (Maybe Integer)
ncqStorageHasBlock sto h = ncqLocate sto h >>= \case
    Nothing  -> pure Nothing
    Just (InMemory bs) -> blockSize bs
    Just (InFossil _ _ size) | ncqIsTombEntrySize size -> pure Nothing
    Just (InFossil _ _ size) -> do
      pure $ Just (ncqEntryPayloadSize (fromIntegral size))

  where
    {-# INLINE blockSize #-}
    blockSize bs =  case ncqEntryUnwrap bs  of
      (_, Left  _)        -> pure Nothing
      (_, Right (M, val)) -> pure (Just (fromIntegral $ BS.length val))
      (_, Right (T, _))   -> pure Nothing
      (_, Right (R, val)) -> pure (Just (fromIntegral $ BS.length val))
      (_, Right (B, val)) -> pure (Just (fromIntegral $ BS.length val))
{-# INLINE ncqStorageHasBlock #-}


-- | Returns strict ByteString
-- | It's up to user to perform BS.copy
-- | in order to free memory mapped file where located
-- | the found block.
-- | Dangling substrings prevent mmaped files from being released
ncqStorageGetBlock :: MonadUnliftIO m
                   => NCQStorage
                   -> HashRef
                   -> m (Maybe ByteString)

ncqStorageGetBlock sto h = runMaybeT do
  loc <- lift (ncqLocate sto h) >>= toMPlus
  guard (not $ ncqIsTomb loc)
  (_,what) <- lift (ncqGetEntryBS sto loc)
               >>= toMPlus
               <&> ncqEntryUnwrap

  case what of
    Left _ -> mzero
    Right (T, _) -> mzero
    Right (_, ebs) -> pure ebs

{-# INLINE ncqStorageGetBlock #-}

-- | Logically delete entry by hash (writes a tomb if present and not already tomb).
--   No-op if entry doesn't exist.
ncqStorageDelBlock :: MonadUnliftIO m
                   => NCQStorage
                   -> HashRef
                   -> m ()
ncqStorageDelBlock = ncqDelEntry
{-# INLINE ncqStorageDelBlock #-}

-- | Salted ref hash: H( ref || ncqSalt )
ncqRefHash :: NCQStorage -> HashRef -> HashRef
ncqRefHash NCQStorage{..} h =
  HashRef (hashObject (coerce @_ @ByteString h <> coerce ncqSalt))
{-# INLINE ncqRefHash #-}


-- R: Reference format
-- SALTED_HASH:BYTES(32) VALUE:BYTES(32) ORIG_HASH:BYTES(32)
-- ^^^^^^^^^^^^^^^^^^^^^ ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
--   KEY                  VALUE
--   LEN(PAYLOAD) = 2 * LEN(KEY)
--
-- We may need this ORIG_HASH in order to restore original
-- reference hash during migrations, fsck or something like
-- this, according to NCQv1 experience.

-- | Get ref value (hash) by logical ref key.
--   Returns Nothing for tomb/absent/invalid.
ncqStorageGetRef :: MonadUnliftIO m => NCQStorage -> HashRef -> m (Maybe HashRef)
ncqStorageGetRef ncq ref = runMaybeT $ do
  let rkey = ncqRefHash ncq ref
  loc <- lift (ncqLocate ncq rkey) >>= toMPlus
  guard (not $ ncqIsTomb loc)
  bs  <- lift (ncqGetEntryBS ncq loc) >>= toMPlus
  case snd (ncqEntryUnwrap bs) of
    Right (R, payload) | BS.length payload == 2*ncqKeyLen
      -> pure (coerce $ BS.take ncqKeyLen payload)
    _ -> mzero
{-# INLINE ncqStorageGetRef #-}

-- | Set ref value if changed. Writes section of type R with fixed key = rkey.
ncqStorageSetRef :: MonadUnliftIO m => NCQStorage -> HashRef -> HashRef -> m ()
ncqStorageSetRef ncq ref val = do
  cur <- ncqStorageGetRef ncq ref
  unless (cur == Just val) $ do
    let rkey    = ncqRefHash ncq ref
        orig    = coerce @_ @ByteString ref
        payload = coerce @_ @ByteString val
    -- Section type R, fixed key = rkey, payload = value hash bytes
    void $ ncqPutBS ncq (Just R) (Just rkey) (payload <> orig)
{-# INLINE ncqStorageSetRef #-}

-- | Delete ref (write tomb for ref key), no-op if absent.
ncqStorageDelRef :: MonadUnliftIO m => NCQStorage -> HashRef -> m ()
ncqStorageDelRef ncq ref =
  ncqDelEntry ncq (ncqRefHash ncq ref)
{-# INLINE ncqStorageDelRef #-}

