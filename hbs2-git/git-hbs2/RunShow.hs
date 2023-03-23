module RunShow where

import HBS2.Prelude
import HBS2.Base58

import HBS2.System.Logger.Simple
import HBS2.Git.Types
import HBS2Git.App
import HBS2Git.State

import Data.Foldable

runShow :: MonadIO m => RepoRef -> App m ()
runShow h = do
  shutUp
  setLogging @INFO infoPrefix

  db <- makeDbPath h >>= dbEnv

  withDB db do

    hd <- stateGetHead
    imported <- stateGetLastImported 10

    info $ "current state for" <+> pretty (AsBase58 h)
    info $ "head:" <+> pretty hd
    info $ "last operations:" <> line

    for_ imported $ \(t,h1,h2) -> do
      info $ pretty t <+> pretty h1 <+> pretty h2

