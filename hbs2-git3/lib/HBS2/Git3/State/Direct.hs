module HBS2.Git3.State.Direct
  ( module HBS2.Git3.State.Direct
  , module HBS2.Git3.State.Types
  ) where

import HBS2.Prelude.Plated
import HBS2.OrDie
import HBS2.System.Dir

import HBS2.Git3.State.Types

import DBPipe.SQLite

import System.Directory

import Text.InterpolatedString.Perl6 (qc)

unit :: FilePath
unit = "hbs2-git"

getStatePath :: (MonadIO m, DBRef db) => db -> m FilePath
getStatePath p = do
  dir <- liftIO $ getXdgDirectory XdgState unit
  pure $ dir </> show (pretty p)


getStatePathDB :: (MonadIO m, DBRef db) => db -> m FilePath
getStatePathDB p = do
  getStatePath p <&> (</> "state.db")


withState :: (MonadIO m, HasStateDB m) => DBPipeM m a -> m a
withState action = getStateDB >>= flip withDB action

evolveState :: (MonadIO m, HasStateDB m) => m ()
evolveState = do
  withState do

    ddl [qc|
create table if not exists
gitobject
  ( githash text not null primary key
  , type    text not null
  , cblock  text not null
  , pack    text not null
  )
|]

    pure ()


