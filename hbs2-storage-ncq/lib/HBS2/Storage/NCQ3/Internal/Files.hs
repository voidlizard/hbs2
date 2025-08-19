{-# Language OverloadedRecordDot #-}
module HBS2.Storage.NCQ3.Internal.Files where

import HBS2.Storage.NCQ3.Internal.Prelude
import HBS2.Storage.NCQ3.Internal.Types

import System.Posix.Files qualified as PFS
import Data.List qualified as List

{- HLINT ignore "Eta reduce" -}


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

ncqFindMinPairOfBy :: forall fa m . (ToFileName fa, MonadUnliftIO m)
                   => NCQStorage
                   -> (fa -> Bool)                         -- ^ eligible predicate
                   -> [fa]
                   -> m (Maybe (NCQFileSize, fa, fa))
ncqFindMinPairOfBy sto eligible lst =
  go lst Nothing
  where
    go :: [fa] -> Maybe (NCQFileSize, fa, fa) -> m (Maybe (NCQFileSize, fa, fa))
    go (a:b:rest) best = do
      best' <- if eligible a && eligible b
                 then do
                   let pa = ncqGetFileName sto a
                   let pb = ncqGetFileName sto b
                   s1 <- fsize pa
                   s2 <- fsize pb
                   let sz = fromIntegral (s1 + s2)
                   pure $ case best of
                     Nothing                -> Just (sz, a, b)
                     Just (sz0,_,_) | sz<sz0 -> Just (sz, a, b)
                     _                       -> best
                 else pure best
      go (b:rest) best'
    go _ best = pure best

    fsize s = liftIO (PFS.getFileStatus s) <&> PFS.fileSize

ncqFindMinPairOf ::  forall fa m . (ToFileName fa, MonadUnliftIO m)
                 => NCQStorage
                 -> [fa]
                 -> m (Maybe (NCQFileSize, fa, fa))
ncqFindMinPairOf sto lst = ncqFindMinPairOfBy sto (const True) lst

