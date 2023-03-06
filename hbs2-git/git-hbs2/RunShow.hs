module RunShow where

import HBS2.Prelude
import HBS2.Data.Types.Refs

import HBS2.System.Logger.Simple
import HBS2Git.App
import HBS2Git.State

import Data.Foldable

runShow :: MonadIO m => HashRef -> App m ()
runShow h = do
  shutUp
  setLogging @INFO infoPrefix

  db <- makeDbPath h >>= dbEnv

  withDB db do

    hd <- stateGetHead
    imported <- stateGetLastImported 10

    info $ "current state for" <+> pretty h
    info $ "head:" <+> pretty hd
    info $ "last operations:" <> line

    for_ imported $ \(t,h1,h2) -> do
      info $ pretty t <+> pretty h1 <+> pretty h2

