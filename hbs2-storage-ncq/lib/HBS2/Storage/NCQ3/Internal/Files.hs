{-# Language OverloadedRecordDot #-}
module HBS2.Storage.NCQ3.Internal.Files where

import HBS2.Storage.NCQ3.Internal.Prelude
import HBS2.Storage.NCQ3.Internal.Types

import System.Posix.Files qualified as PFS
import Data.List qualified as List


removeFile :: MonadIO m => FilePath -> m ()
removeFile fp = do
  debug $ "removeFile" <+> pretty fp
  rm fp

moveFile :: MonadIO m => FilePath -> FilePath -> m ()
moveFile a b = do
  debug $ "moveFile" <+> pretty a <+> pretty b
  mv a b

ncqGetFileName :: forall f . ToFileName f => NCQStorage -> f -> FilePath
ncqGetFileName ncq fp = ncqGetWorkDir ncq </> takeFileName (toFileName fp)

ncqGetWorkDir :: NCQStorage -> FilePath
ncqGetWorkDir NCQStorage{..} = ncqRoot </> show ncqGen

ncqGetLockFileName :: NCQStorage -> FilePath
ncqGetLockFileName ncq = ncqGetFileName ncq ".lock"

ncqGetNewFileKey :: forall f m . (ToFileName f, MonadIO m)
                    => NCQStorage
                    -> ( FileKey -> f )
                    -> m FileKey
ncqGetNewFileKey me@NCQStorage{..} fnameOf = fix \next -> do
    n <- atomically $ stateTVar ncqState (\e -> (e.ncqStateFileSeq , succSeq e))
    here <- doesFileExist (ncqGetFileName me (fnameOf n))
    if here then next else pure n
  where
    succSeq e = e { ncqStateFileSeq = succ e.ncqStateFileSeq }

ncqListFilesBy :: forall m . MonadUnliftIO m => NCQStorage -> (FilePath -> Bool) -> m [(POSIXTime, FileKey)]
ncqListFilesBy  me@NCQStorage{..} filt = do
  w <- dirFiles (ncqGetWorkDir me)
           <&> filter (filt .  takeFileName)

  r <- for w $ \fn -> do
         ts <- liftIO (PFS.getFileStatus fn) <&> PFS.modificationTimeHiRes
         pure (ts, fromString (takeBaseName fn))

  pure $ List.sortOn ( Down . fst ) r

ncqFindMinPairOf ::  forall fa m . (ToFileName fa, MonadUnliftIO m)
                 => NCQStorage
                 -> [fa]
                 -> m (Maybe (NCQFileSize, fa, fa))
ncqFindMinPairOf sto lst = do

  let files = fmap (\x -> (x, ncqGetFileName sto x)) lst

  flip fix (files, Nothing) $ \next (fs, r) -> do
    case fs of
      [] ->  pure r
      [ _ ] -> pure r
      ( s1 : s2 : ss ) -> do
        size1 <- fsize (snd s1)
        size2 <- fsize (snd s2)
        let size = fromIntegral $ size1 + size2

        case r of
          Nothing -> next (s2 : ss, Just (size, fst s1, fst s2) )
          e@(Just (size0, _, _)) | size0 > size -> next (s2 : ss, Just (size, fst s1, fst s2) )
                                 | otherwise -> next (s2:ss, e)

  where fsize s = liftIO (PFS.getFileStatus s) <&> PFS.fileSize



