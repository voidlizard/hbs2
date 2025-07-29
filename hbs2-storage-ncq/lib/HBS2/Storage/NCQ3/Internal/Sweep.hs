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

data SweepSt = SweepWaitIdle
             | SweepCheckEMA SweepSt
             | SweepSomething


ncqSweepLoop :: MonadUnliftIO m => NCQStorage3 -> m ()
ncqSweepLoop me@NCQStorage3{..} = flip fix SweepWaitIdle $ \next -> \case

  SweepWaitIdle -> do
    debug "SweepWaitIdle"
    pause @'Seconds 10
    next (SweepCheckEMA SweepSomething)

  SweepCheckEMA who -> do
    ema <- readTVarIO ncqWriteEMA
    debug $ "SweepCheckEMA" <+> pretty ema
    if ema < ncqIdleThrsh then do
      next who
    else
      next SweepWaitIdle

  SweepSomething -> do
    debug $ "SweepSomething"
    pause @'Seconds 10
    next SweepWaitIdle

ncqSweepObsoleteStates :: forall m .  MonadUnliftIO m => NCQStorage3 -> m ()
ncqSweepObsoleteStates me@NCQStorage3{..} = void $ runMaybeT do
    debug $ "ncqSweepObsoleteStates"

    k <- readTVarIO ncqStateKey >>= toMPlus

    r <- liftIO $ try @_ @SomeException do
          ts <- PFS.getFileStatus (ncqGetFileName me (StateFile k)) <&> PFS.modificationTimeHiRes
          filez <- ncqListFilesBy me (List.isPrefixOf "s-")

          for_ filez $ \(t,f) -> do

            when (f /= k && t < ts) do
              debug $ yellow "TO REMOVE" <+> pretty (toFileName (StateFile f))
              rm (ncqGetFileName me (StateFile f))
    lift do

      case r of
        Left e  -> err ("SweepStates failed" <+> viaShow e)
        Right{} -> none


