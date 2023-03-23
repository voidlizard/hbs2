module HBS2Git.Update where

import HBS2.Prelude.Plated
import HBS2.OrDie

import HBS2.System.Logger.Simple

import HBS2.Git.Types
import HBS2Git.Types
import HBS2Git.App
import HBS2Git.State
import HBS2Git.Import


updateLocalState :: (MonadIO m, HasCatAPI m) => RepoRef -> m ()
updateLocalState ref = do

  dbPath <- makeDbPath ref

  trace $ "dbPath:" <+> pretty dbPath

  db <- dbEnv dbPath

  trace $ "updateLocalState" <+> pretty ref

  -- TODO: read-reflog
  -- TODO: update-reflog
  importRefLog db ref

  (n,hash) <- withDB db $ stateGetRefLogLast `orDie` "empty reflog"

  trace $ "got reflog" <+> pretty (n,hash)

  importObjects db hash

  pure ()

