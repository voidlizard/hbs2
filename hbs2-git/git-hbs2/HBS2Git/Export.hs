module HBS2Git.Export where

import HBS2.Prelude

runExport :: MonadIO m => m ()
runExport = liftIO do
  print $ pretty "Export"
  pure ()


