module RunShow where

import HBS2.Prelude
import HBS2.Base58

import HBS2Git.App
import HBS2Git.State
import HBS2Git.Config
import HBS2Git.ListRefs

import Data.Foldable
import Prettyprinter.Render.Terminal

data ShowObject = ShowRef RepoRef | ShowConfig

showRef :: MonadIO m => RepoRef -> App m ()
showRef h = do
  db <- makeDbPath h >>= dbEnv
  -- FIXME: re-implement-showRef
  pure ()
  -- withDB db do
  --   hd <- stateGetHead
  --   imported <- stateGetLastImported 10
  --   liftIO $ do
  --     print $ "current state for" <+> pretty (AsBase58 h)
  --     print $ "head:" <+> pretty hd
  --     print $ pretty "last operations:"
  --     for_ imported (\(t,h1,h2) -> print $ pretty t <+> pretty h1 <+> pretty h2)

showRefs :: MonadIO m => App m ()
showRefs = do
  liftIO $ putDoc $ line <> green "References:" <> section
  runListRefs

showConfig :: MonadIO m => App m ()
showConfig = liftIO do
  ConfigPathInfo{..} <- getConfigPathInfo
  cfg <- readFile configFilePath
  putDoc $ green "Config file location:" <> section <> pretty configFilePath <> section
  putDoc $ green "Config contents:" <> line <> pretty cfg

showSummary :: MonadIO m => App m ()
showSummary = do
  showRefs
  liftIO $ putDoc section
  showConfig

runShow :: MonadIO m => Maybe ShowObject -> App m ()
runShow (Just (ShowRef h)) = showRef h
runShow (Just ShowConfig) = showConfig
runShow Nothing = showSummary
