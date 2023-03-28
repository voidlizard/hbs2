module HBS2Git.Update where

import HBS2.Prelude.Plated
import HBS2.OrDie

import HBS2.System.Logger.Simple

import HBS2.Git.Types
import HBS2Git.Types
import HBS2Git.App
import HBS2Git.State
import HBS2Git.Import

import Control.Monad.Catch


updateLocalState :: (MonadIO m, HasCatAPI m, MonadCatch m) => RepoRef -> m ()
updateLocalState ref = do

  dbPath <- makeDbPath ref

  trace $ "dbPath:" <+> pretty dbPath

  db <- dbEnv dbPath

  trace $ "updateLocalState" <+> pretty ref

  sp <- withDB db savepointNew

  withDB db $ savepointBegin sp

  r <- try $ do

    -- TODO: read-reflog
    -- TODO: update-reflog
    importRefLog db ref

    (n,hash) <- withDB db $ stateGetRefLogLast `orDie` "empty reflog"

    trace $ "got reflog" <+> pretty (n,hash)

    importObjects db hash

    withDB db (savepointRelease sp)

  case r of
    Left (e :: SomeException) -> do
      withDB db $ savepointRollback sp
      err (viaShow e)
      err "error happened. state rolled back"

    Right{} -> pure ()


