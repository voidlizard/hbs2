module HBS2.Storage.NCQ3.Internal.Sweep where


import HBS2.Storage.NCQ3.Internal.Prelude
import HBS2.Storage.NCQ3.Internal.Types
import HBS2.Storage.NCQ3.Internal.Files
import HBS2.Storage.NCQ3.Internal.State
import HBS2.Storage.NCQ3.Internal.Index

import Data.Generics.Uniplate.Operations
import Data.Generics.Uniplate.Data()
import Data.List qualified as List
import Data.HashSet qualified as HS
import System.Posix.Files qualified as PFS
import Control.Monad.Trans.Maybe
import Data.HashMap.Strict qualified as HM

ncqLiveKeysSTM :: NCQStorage3 -> STM (HashSet FileKey)
ncqLiveKeysSTM NCQStorage3{..} = do

  s0     <- readTVar ncqState
  merged <- readTVar ncqStateUse <&> (s0<>) . foldMap fst . HM.elems

  pure $ HS.fromList $ universeBi @_ @FileKey merged

ncqLiveKeys :: forall m . MonadIO m => NCQStorage3 -> m (HashSet FileKey)
ncqLiveKeys  ncq = atomically $ ncqLiveKeysSTM  ncq

ncqSweepFiles :: forall m . MonadUnliftIO m => NCQStorage3 -> m ()
ncqSweepFiles me@NCQStorage3{..} = withSem ncqServiceSem do

  debug "ncqSweepFiles"

  live <- ncqLiveKeys me


  debug $ "ALIVE" <+> pretty (HS.toList live)

  fossils <- ncqListFilesBy me (List.isPrefixOf "f-")
  indexes <- ncqListFilesBy me (List.isPrefixOf "i-")

  for_ indexes $ \(_, k) -> unless (HS.member k live) do
    let fn = ncqGetFileName me (IndexFile k)
    debug $ yellow "REMOVING" <+> pretty (takeFileName fn)
    rm fn

  for_ fossils $ \(_, k) -> unless (HS.member k live) do
    let fn = ncqGetFileName me (DataFile k)
    debug $ yellow "REMOVING" <+> pretty (takeFileName fn)
    rm fn


ncqSweepObsoleteStates :: forall m .  MonadUnliftIO m => NCQStorage3 -> m ()
ncqSweepObsoleteStates me@NCQStorage3{..} = withSem ncqServiceSem do
    debug $ "ncqSweepObsoleteStates"

    k <- readTVarIO ncqStateKey

    r <- liftIO $ try @_ @SomeException do
          ts <- PFS.getFileStatus (ncqGetFileName me (StateFile k)) <&> PFS.modificationTimeHiRes
          filez <- ncqListFilesBy me (List.isPrefixOf "s-")

          for_ filez $ \(t,f) -> do

            when (f /= k && t < ts) do
              debug $ yellow "TO REMOVE" <+> pretty (toFileName (StateFile f))
              rm (ncqGetFileName me (StateFile f))

    case r of
      Left e  -> err ("SweepStates failed" <+> viaShow e)
      Right{} -> none


