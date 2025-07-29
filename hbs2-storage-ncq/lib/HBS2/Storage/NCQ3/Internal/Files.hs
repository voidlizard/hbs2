{-# Language OverloadedRecordDot #-}
module HBS2.Storage.NCQ3.Internal.Files where

import HBS2.Storage.NCQ3.Internal.Prelude
import HBS2.Storage.NCQ3.Internal.Types

import System.Posix.Files qualified as PFS
import Data.List qualified as List


ncqGetFileName :: forall f . ToFileName f => NCQStorage3 -> f -> FilePath
ncqGetFileName ncq fp = ncqGetWorkDir ncq </> takeFileName (toFileName fp)

ncqGetWorkDir :: NCQStorage3 -> FilePath
ncqGetWorkDir NCQStorage3{..} = ncqRoot </> show ncqGen

ncqGetLockFileName :: NCQStorage3 -> FilePath
ncqGetLockFileName ncq = ncqGetFileName ncq ".lock"

ncqGetNewFileKey :: forall f m . (ToFileName f, MonadIO m)
                    => NCQStorage3
                    -> ( FileKey -> f )
                    -> m FileKey
ncqGetNewFileKey me@NCQStorage3{..} fnameOf = fix \next -> do
    n <- atomically $ stateTVar ncqState (\e -> (e.ncqStateFileSeq , succSeq e))
    here <- doesFileExist (ncqGetFileName me (fnameOf n))
    if here then next else pure n
  where
    succSeq e = e { ncqStateFileSeq = succ e.ncqStateFileSeq }

ncqListFilesBy :: forall m . MonadUnliftIO m => NCQStorage3 -> (FilePath -> Bool) -> m [(POSIXTime, FileKey)]
ncqListFilesBy  me@NCQStorage3{..} filt = do
  w <- dirFiles (ncqGetWorkDir me)
           <&> filter (filt .  takeFileName)

  r <- for w $ \fn -> do
         ts <- liftIO (PFS.getFileStatus fn) <&> PFS.modificationTimeHiRes
         pure (ts, fromString (takeBaseName fn))

  pure $ List.sortOn ( Down . fst ) r



