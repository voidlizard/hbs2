module HBS2.Storage.NCQ3.Internal.State where

import HBS2.Storage.NCQ3.Internal.Prelude
import HBS2.Storage.NCQ3.Internal.Types
import HBS2.Storage.NCQ3.Internal.Files

import Data.Config.Suckless.Script

import Data.List qualified as List
import Control.Monad.Reader
import Control.Monad.Trans.Cont
import Data.HashSet qualified as HS
import Data.Set qualified as Set
import Data.ByteString qualified as BS
import UnliftIO.IO.File
import Network.ByteOrder qualified as N
import UnliftIO.IO
import System.IO qualified as IO

newtype StateOP a =
  StateOP { fromStateOp :: ReaderT NCQStorage3 STM a }
  deriving newtype (Functor,Applicative,Monad,MonadReader NCQStorage3)

{- HLINT ignore "Eta reduce"-}

ncqStateUpdate :: MonadIO m
               => NCQStorage3
               -> StateOP a
               -> m ()
ncqStateUpdate ncq@NCQStorage3{..} action = do
  snkFile <- ncqGetNewFileKey ncq StateFile <&> ncqGetFileName ncq . toFileName . StateFile
  (n,i,f,facts) <- atomically do
                      runReaderT (fromStateOp action) ncq
                      n  <- readTVar ncqStateFileSeq
                      i  <- readTVar ncqStateIndex
                      f  <- readTVar ncqStateFiles
                      fa <- readTVar ncqStateFacts
                      pure (n,i,f,fa)

  liftIO $ withBinaryFileDurableAtomic snkFile WriteMode $ \fh -> do
    for_ i $ \(Down p, fk) -> do
      IO.hPrint fh $ "i" <+> pretty fk <+> pretty (round @_ @Word64 p)

    for_ f $ \fk -> do
      IO.hPrint fh $  "f" <+> pretty fk

    for_ facts $ \(FI (DataFile a) (IndexFile b)) -> do
      IO.hPrint fh $  "fi" <+> pretty a <+> pretty b

    IO.hPrint fh $  "n" <+> pretty n

ncqStateAddDataFile :: FileKey -> StateOP ()
ncqStateAddDataFile fk = do
  NCQStorage3{..} <- ask
  StateOP $ lift do
    modifyTVar ncqStateFiles (HS.insert fk)

ncqStateAddFact :: Fact -> StateOP ()
ncqStateAddFact fact = do
  NCQStorage3{..} <- ask
  StateOP $ lift do
    modifyTVar ncqStateFacts (Set.insert fact)

ncqStateAddIndexFile :: POSIXTime
                     -> FileKey
                     -> StateOP ()

ncqStateAddIndexFile ts fk  = do
  NCQStorage3{..} <- ask
  StateOP $ lift do
    modifyTVar' ncqStateIndex $ \xs ->
      List.sortOn fst ((Down ts, fk) : xs)


ncqFileFastCheck :: MonadUnliftIO m => FilePath -> m ()
ncqFileFastCheck fp = do

  -- debug $ "ncqFileFastCheck" <+> pretty fp

  mmaped <- liftIO $ mmapFileByteString fp Nothing
  let size = BS.length mmaped
  let s = BS.drop (size - 8) mmaped & N.word64

  unless ( BS.length mmaped == fromIntegral s ) do
    throwIO $ NCQFsckIssueExt (FsckInvalidFileSize (fromIntegral s))


ncqTryLoadState :: forall m. MonadUnliftIO m
             => NCQStorage3
             -> m ()

ncqTryLoadState me@NCQStorage3{..} = do

  stateFiles <- ncqListFilesBy me ( List.isPrefixOf "s-" )

  flip  runContT pure $ callCC \exit -> do

    for stateFiles $ \(_,fn) -> do
        none

    none

  -- for_ stateFiles $ \(d,f) -> do
  --   notice $ "state-file" <+> pretty (toFileName (StateFile f))

-- tryLoadState :: forall m. MonadUnliftIO m
--              => NCQStorage3
--              -> StateFile FileKey
--              -> m (Maybe (HashSet FileKey, [(Down POSIXTime, FileKey)], FileKey))
-- tryLoadState me@NCQStorage3{..} fk = do
--   debug $ "tryLoadState" <+> pretty fk

--   (fset, idxList, n) <- liftIO (readState fk)

--   let checkFile :: DataFile FileKey -> m Bool
--       checkFile fo = flip fix 0 \next (i :: Int) -> do
--         let dataFile  = ncqGetFileName me (toFileName fo)
--         let indexFile = ncqGetFileName me (toFileName (IndexFile (coerce fo)))

--         doesFileExist dataFile >>= \case
--           False -> do
--             rm indexFile
--             pure False

--           True -> do
--             try @_ @SomeException (ncqFileFastCheck dataFile) >>= \case
--               Left e -> do
--                 err (viaShow e)
--                 stillThere <- doesFileExist dataFile
--                 when stillThere do
--                   let broken = dropExtension dataFile `addExtension` ".broken"
--                   mv dataFile broken
--                   rm indexFile
--                   warn $ red "renamed" <+> pretty dataFile <+> pretty broken
--                 pure False

--               Right{} | i > 1 -> pure False

--               Right{} -> do
--                 exists <- doesFileExist indexFile
--                 if exists
--                   then pure True
--                   else do
--                     debug $ "indexing" <+> pretty (toFileName fo)
--                     _ <- ncqIndexFile me fo
--                     debug $ "indexed" <+> pretty indexFile
--                     next (i + 1)

--   results <- forM (HS.toList fset) (checkFile . DataFile)

--   pure $
--     if and results
--       then Just (fset, idxList, n)
--       else Nothing



