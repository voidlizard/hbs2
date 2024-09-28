module HBS2.Git.DashBoard.Fixme
  ( F.listFixme
  , F.HasPredicate(..)
  , F.SelectPredicate(..)
  , runInFixme
  , RunInFixmeError(..)
  ) where

import HBS2.Git.DashBoard.Prelude
import HBS2.Git.DashBoard.Types
import HBS2.Git.DashBoard.State

import HBS2.OrDie

import Fixme.State qualified as F
import Fixme.Types
import Fixme.Config

import DBPipe.SQLite (withDB, shutdown)

import Data.Generics.Product.Fields (field)

data RunInFixmeError =
  FixmeRefChanNotFound RepoLww
  deriving stock (Generic, Typeable, Show)

instance Exception RunInFixmeError

-- TODO: less-hacky-approach
--  этот код подразумевает, что мы знаем довольно много деталей
--  реализации про fixme-new
--
--  Хорошо бы как-то абстрагировать, изолировать и т.п.
--
runInFixme :: (DashBoardPerks m, MonadReader DashBoardEnv m) => RepoLww -> FixmeM m a ->  m a
runInFixme repo m = do

  denv <- ask

  fixmeRChan <- withDashBoardEnv denv $ selectRepoFixmeRefChan repo
                  >>= orThrow (FixmeRefChanNotFound repo)

  p <- fixmeDataPath fixmeRChan

  fenv <- fixmeEnvBare
  fo <- newTVarIO (FixmeOpts True)

  twd <- newTVarIO p
  let fenvNew  = fenv & set (field @"fixmeEnvWorkDir") twd
                      & set (field @"fixmeEnvOpts") fo

  -- TODO: close-fixme-database-garanteed
  --  похоже, что надо будет фиксить db-pipe

  flip runContT pure do
    dbe <- lift $ withFixmeEnv fenvNew $ F.withState ask

    void $ ContT $ bracket none (const $ shutdown False dbe)

    lift $ withFixmeEnv fenvNew do
      dbp <- localDBPath
      wd  <- fixmeWorkDir
      cfg <- localConfig
      trace $ "fixme:dir"    <+> pretty wd
      trace $ "fixme:config" <+> pretty cfg
      trace $ "fixme:db"     <+> pretty dbp

      m


