{-# Language TemplateHaskell #-}
{-# Language AllowAmbiguousTypes #-}
module HBS2.Git.Client.Progress where

import HBS2.Git.Client.Prelude
import HBS2.Git.Data.RefLog

import HBS2.Git.Data.Tx

data Progress a =
  Progress
  { _progressState :: a
  , _progressTotal :: Maybe a
  }
  deriving (Eq,Generic)

makeLenses 'Progress

class HasProgress a where
  onProgress :: MonadIO m => a -> ProgressEvent -> m ()

data ProgressEvent =
    ImportIdle
  | ImportRefLogStart RefLogId
  | ImportRefLogDone  RefLogId (Maybe HashRef)
  | ImportWaitTx HashRef
  | ImportScanTx HashRef
  | ImportApplyTx HashRef
  | ImportReadBundleChunk BundleMeta (Progress Int)
  | ImportSetQuiet Bool
  | ImportAllDone
  | ExportWriteObject (Progress Int)


data AnyProgress = forall a . HasProgress a => AnyProgress a

instance HasProgress AnyProgress where
  onProgress (AnyProgress e) = onProgress e

instance HasProgress () where
  onProgress _ _ = pure ()

newtype ProgressQ = ProgressQ (TQueue ProgressEvent)

instance HasProgress ProgressQ where
  onProgress (ProgressQ q) ev = atomically (writeTQueue q ev)

newProgressQ :: MonadUnliftIO m => m ProgressQ
newProgressQ = ProgressQ <$> newTQueueIO



