module HBS2.Git.DashBoard.State where

import HBS2.Prelude.Plated

import HBS2.Git.DashBoard.Types

import HBS2.System.Logger.Simple.ANSI
import Data.Config.Suckless

import DBPipe.SQLite
import DBPipe.SQLite.Generic

import Text.InterpolatedString.Perl6 (qc)
import Control.Monad.Reader


evolveDB :: MonadIO m => DBPipeM m ()
evolveDB = do

  ddl [qc|
    create table if not exists project
      (  lww text not null
      ,  primary key (lww)
      )
  |]

  pure ()


updateIndex :: (MonadIO m, HasConf m, MonadReader DashBoardEnv m) => m ()
updateIndex = do
  debug "updateIndex"
  pure ()


