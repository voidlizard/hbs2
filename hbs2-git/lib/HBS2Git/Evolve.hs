module HBS2Git.Evolve (evolve,makePolled) where

import HBS2.Prelude.Plated
import HBS2.System.Logger.Simple
import HBS2.Net.Proto.Service

import HBS2.Peer.RPC.API.Peer

import HBS2Git.Types
import HBS2Git.Config
import HBS2Git.PrettyStuff

import Control.Monad.Trans.Maybe
import Control.Monad.Catch (MonadThrow(..))
import Data.List qualified as List
import System.Directory
import System.Random
import System.FilePath
import UnliftIO

-- NOTE: hbs2-git-evolve
--   выполняет идемпотентные миграции между старыми и
--   новыми версиями.
--   например, переносит конфиг

evolve :: (MonadIO m, MonadThrow m) => m ()
evolve = void $ runMaybeT do

  here   <- liftIO getCurrentDirectory

  debug $ "evolve: current directory:" <+> pretty here

  cfg <- configPath ""

  debug $ "*** GIT DIRECTORY" <+> pretty cfg

  migrateConfig
  generateCookie


makePolled :: (MonadIO m, HasRPC m) => RepoRef -> m ()
makePolled ref = do
  rpc <- getRPC <&> rpcPeer
  n <- liftIO $ randomRIO (4,7)
  void $ callService @RpcPollAdd rpc (fromRefLogKey ref, "reflog", n)

generateCookie :: (MonadIO m, MonadThrow m) => m ()
generateCookie = void $ runMaybeT do
  file <- cookieFile

  guard =<< liftIO (not <$> doesFileExist file)

  -- NOTE: cookie-note
  --  поскольку куки должна быть уникальна в рамках БД,
  --  а тут мы пока не знаем, с какой БД мы работаем,
  --  то отложим генерацию куки до создания БД.
  --  В скором времени БД будет одна, но пока это не так
  liftIO $ writeFile file ""


migrateConfig :: (MonadIO m, MonadThrow m) => m ()
migrateConfig = void $ runMaybeT do
  here   <- liftIO getCurrentDirectory

  rootDir <- configPath "" <&> takeDirectory

  oldPath <- configPathOld here
  let oldConf = oldPath </> "config"

  let newConfDir = rootDir </> ".hbs2"
  let newConfFile = newConfDir </> "config"

  guard =<< liftIO (not <$> doesFileExist newConfFile)

  trace $ "EVOLVE: root directory" <+> pretty newConfDir

  confFileHere <- liftIO $ doesFileExist newConfFile

  guard (not confFileHere)

  liftIO do
    hPutDoc stderr $ red "evolve: creating new config" <+> pretty newConfFile <> line
    createDirectoryIfMissing True newConfDir

    appendFile newConfFile ""

    oldHere <- doesFileExist oldConf

    when oldHere do
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


