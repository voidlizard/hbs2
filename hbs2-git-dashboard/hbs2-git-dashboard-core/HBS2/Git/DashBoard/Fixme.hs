module HBS2.Git.DashBoard.Fixme
  ( F.HasPredicate(..)
  , F.SelectPredicate(..)
  , runInFixme
  , countFixme
  , listFixme
  , RunInFixmeError(..)
  , Fixme(..)
  , FixmeKey(..)
  , FixmeTitle(..)
  , FixmeTag(..)
  , FixmePlainLine(..)
  , FixmeAttrName(..)
  , FixmeAttrVal(..)
  , FixmeOpts(..)
  ) where

import HBS2.Git.DashBoard.Prelude
import HBS2.Git.DashBoard.Types
import HBS2.Git.DashBoard.State

import HBS2.OrDie

import Fixme.State qualified as F
import Fixme.State (HasPredicate(..))
import Fixme.Types
import Fixme.Config

import DBPipe.SQLite (shutdown)

import Data.Either
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

listFixme :: (DashBoardPerks m, MonadReader DashBoardEnv m, HasPredicate q) => RepoLww -> q -> m [Fixme]
listFixme repo q = do
  runInFixme repo $ F.listFixme q
    & try @_ @SomeException
    <&> fromRight mempty

countFixme :: (DashBoardPerks m, MonadReader DashBoardEnv m) => RepoLww -> m (Maybe Int)
countFixme repo = do
  runInFixme repo $ F.countFixme
    & try @_ @SomeException
    <&> either (const Nothing) Just


