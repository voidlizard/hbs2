module HBS2.Git3.State.Types
  ( module HBS2.Git3.State.Types
  , pattern SignPubKeyLike
  ) where


import HBS2.Prelude.Plated
import HBS2.Git3.Config.Local
import HBS2.Net.Auth.Credentials

import DBPipe.SQLite

import System.FilePath

type DBRef w = ( Pretty w )

class MonadIO m => HasStateDB m where
  getStateDB :: m DBPipeEnv


unit :: FilePath
unit = "hbs2-git"

getStatePath :: (MonadIO m, DBRef db) => db -> m FilePath
getStatePath p = do
  d <- getConfigPath
  pure $ d </> show (pretty p)


