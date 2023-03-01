module HBS2Git.Export where

import HBS2.Prelude
import HBS2.System.Logger.Simple

runExport :: MonadIO m => m ()
runExport = liftIO do
  trace "Export"

  -- TODO: read-repo-head
  trace "read repository head"

  -- TODO: create-repo-head-block
  trace "create-repo-head-block"

  -- TODO: build-transitive-closure
  trace "build-transitive-closure"


  pure ()


