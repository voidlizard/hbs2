module HBS2.Storage.NCQ3.Internal.Sweep where


import HBS2.Storage.NCQ3.Internal.Prelude
import HBS2.Storage.NCQ3.Internal.Types
import HBS2.Storage.NCQ3.Internal.Files
import HBS2.Storage.NCQ3.Internal.State

import Control.Monad.Trans.Cont
import Data.Generics.Uniplate.Data()
import Data.Generics.Uniplate.Operations
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.List qualified as List
import System.Posix.Files qualified as PFS

ncqLiveKeysSTM :: NCQStorage -> STM (HashSet FileKey)
ncqLiveKeysSTM NCQStorage{..} = do

  s0      <- readTVar ncqState
  merged  <- readTVar ncqStateUse <&> (s0<>) . foldMap fst . HM.elems
  current <- readTVar ncqCurrentFossils

  pure $ current <> HS.fromList (universeBi @_ @FileKey merged)

ncqLiveKeys :: forall m . MonadIO m => NCQStorage -> m (HashSet FileKey)
ncqLiveKeys  ncq = atomically $ ncqLiveKeysSTM  ncq

{- HLINT ignore "Functor law"-}

ncqSweepFiles :: forall m . MonadUnliftIO m => NCQStorage -> m ()
ncqSweepFiles me@NCQStorage{..} = do

  debug "ncqSweepFiles"

  fossils <- ncqListFilesBy me (List.isPrefixOf "f-")
  indexes <- ncqListFilesBy me (List.isPrefixOf "i-")

  stateFiles  <- ncqListFilesBy me (List.isPrefixOf "s-") <&> fmap snd

  liveOnDisk <- for stateFiles (readStateMay me)
                  <&> mconcat . catMaybes
                  <&> HS.fromList . universeBi @_ @FileKey

  live <- ncqLiveKeys me <&> (<> liveOnDisk)

  debug $ "ALIVE" <+> pretty (HS.toList live)

  for_ indexes $ \(_, k) -> unless (HS.member k live) do
    let fn = ncqGetFileName me (IndexFile k)
    debug $ yellow "REMOVING" <+> pretty (takeFileName fn)
    removeFile fn

  for_ fossils $ \(_, k) -> unless (HS.member k live) do
    let fn = ncqGetFileName me (DataFile k)
    debug $ yellow "REMOVING" <+> pretty (takeFileName fn)
    removeFile fn


ncqSweepObsoleteStates :: forall m .  MonadUnliftIO m => NCQStorage -> m ()
ncqSweepObsoleteStates me@NCQStorage{..} = flip runContT pure $ callCC \exit -> do
    debug $ "ncqSweepObsoleteStates"

    k <- readTVarIO ncqStateKey

    when (k == ncqNullStateKey) $ exit ()

    r <- liftIO $ try @_ @SomeException do
          ts <- PFS.getFileStatus (ncqGetFileName me (StateFile k)) <&> PFS.modificationTimeHiRes

          filez <- ncqListFilesBy me (List.isPrefixOf "s-")
                      <&> List.drop 1 . List.sortOn (Down . snd) -- delete old 10 states

          for_ filez $ \(t,f) -> do

            when (f /= k && t < ts) do
              debug $ yellow "TO REMOVE" <+> pretty (toFileName (StateFile f))
              removeFile (ncqGetFileName me (StateFile f))

    case r of
      Left e  -> err ("SweepStates failed" <+> viaShow e)
      Right{} -> none


