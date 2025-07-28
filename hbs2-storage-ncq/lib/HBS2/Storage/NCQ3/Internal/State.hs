module HBS2.Storage.NCQ3.Internal.State where

import HBS2.Storage.NCQ3.Internal.Prelude
import HBS2.Storage.NCQ3.Internal.Types

import Data.List qualified as List
import Control.Monad.Reader
import Data.HashSet qualified as HS

import UnliftIO.IO.File
import UnliftIO.IO
import System.IO qualified as IO

newtype StateOP a =
  StateOP { fromStateOp :: ReaderT NCQStorage3 STM a }
  deriving newtype (Functor,Applicative,Monad,MonadReader NCQStorage3)

ncqGetFileName :: NCQStorage3 -> FilePath -> FilePath
ncqGetFileName ncq fp = ncqGetWorkDir ncq </> takeFileName fp

ncqGetWorkDir :: NCQStorage3 -> FilePath
ncqGetWorkDir NCQStorage3{..} = ncqRoot </> show ncqGen

ncqGetLockFileName :: NCQStorage3 -> FilePath
ncqGetLockFileName ncq = ncqGetFileName ncq ".lock"

ncqGetNewFileKey :: forall f m . (ToFileName f, MonadIO m)
                    => NCQStorage3
                    -> ( FileKey -> f )
                    -> m FileKey
ncqGetNewFileKey me@NCQStorage3{..} fnameOf = fix \next -> do
    n <- atomically $ stateTVar ncqStateFileSeq (\x -> (x, succ x))
    here <- doesFileExist (ncqGetFileName me (toFileName $ fnameOf n))
    if here then next else pure n

{- HLINT ignore "Eta reduce"-}

ncqStateUpdate :: MonadIO m
               => NCQStorage3
               -> StateOP a
               -> m ()
ncqStateUpdate ncq@NCQStorage3{..} action = do
  snkFile <- ncqGetNewFileKey ncq StateFile <&> ncqGetFileName ncq . toFileName . StateFile
  (n,i,f) <- atomically do
                runReaderT (fromStateOp action) ncq
                n <- readTVar ncqStateFileSeq
                i <- readTVar ncqStateIndex
                f <- readTVar ncqStateFiles
                pure (n,i,f)

  liftIO $ withBinaryFileDurableAtomic snkFile WriteMode $ \fh -> do
    for_ i $ \(Down p, fk) -> do
      IO.hPrint fh $ "i" <+> pretty fk <+> pretty (round @_ @Word64 p)

    for_ f $ \fk -> do
      IO.hPrint fh $  "f" <+> pretty fk

    IO.hPrint fh $  "n" <+> pretty n

ncqStateAddDataFile :: FileKey -> StateOP ()
ncqStateAddDataFile fk = do
  NCQStorage3{..} <- ask
  StateOP $ lift do
    modifyTVar ncqStateFiles (HS.insert fk)

ncqStateAddIndexFile :: POSIXTime
                     -> FileKey
                     -> StateOP ()

ncqStateAddIndexFile ts fk  = do
  NCQStorage3{..} <- ask
  StateOP $ lift do
    modifyTVar' ncqStateIndex $ \xs ->
      List.sortOn fst ((Down ts, fk) : xs)

