module HBS2Git.Evolve (evolve) where

import HBS2.Prelude.Plated
import HBS2.System.Logger.Simple
import HBS2.OrDie

import HBS2.Git.Types
import HBS2Git.Config
import HBS2Git.PrettyStuff

import Control.Monad.Trans.Maybe
import Data.Functor
import Data.List qualified as List
import Prettyprinter.Render.Terminal
import System.Directory
import System.FilePath
import UnliftIO

-- NOTE: hbs2-git-evolve
--   выполняет идемпотентные миграции между старыми и
--   новыми версиями.
--   например, переносит конфиг

evolve :: MonadIO m => m ()
evolve = do
  trace "DO EVOLVE"

  migrateConfig

  shutUp

  pure ()


migrateConfig :: MonadIO m => m ()
migrateConfig = void $ runMaybeT do
  here   <- liftIO getCurrentDirectory
  rootDir <- (findGitDir here <&> fmap takeDirectory) `orDie` "*** hbs2-git: working directory not found"

  oldPath <- configPathOld here
  let oldConf = oldPath </> "config"

  guard =<< liftIO (doesDirectoryExist oldPath)
  guard =<< liftIO (doesFileExist oldConf)

  let newConfDir = rootDir </> ".hbs2"
  let newConfFile = newConfDir </> "config"

  guard =<< liftIO (not <$> doesFileExist newConfFile)

  trace $ "EVOLVE: root directory" <+> pretty newConfDir

  confFileHere <- liftIO $ doesFileExist newConfFile

  guard (not confFileHere)

  liftIO do
    hPutDoc stderr $ red "evolve: creating new config" <+> pretty newConfFile <> line
    createDirectoryIfMissing True newConfDir
    hPutDoc stderr $ red "evolve: moving config to" <+> pretty newConfFile <> line
    liftIO $ renameFile oldConf newConfFile

    anything <- liftIO $ listDirectory oldPath

    if List.null anything then do
      hPutDoc stderr $ red "evolve: removing"
                        <+> pretty oldPath <> line

      removeDirectory oldPath
    else do
      hPutDoc stderr $ red "evolve: not empty" <+> pretty oldPath <> line

      hPutDoc stderr $ yellow "evolve: remove"
                          <+> pretty oldPath
                          <+> yellow "on your own"
                          <> line


