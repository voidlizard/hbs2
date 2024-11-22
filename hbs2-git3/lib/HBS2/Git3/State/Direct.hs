{-# OPTIONS_GHC -fno-warn-orphans #-}
module HBS2.Git3.State.Direct
  ( module HBS2.Git3.State.Direct
  , module HBS2.Git3.State.Types
  ) where

import HBS2.Prelude.Plated
import HBS2.OrDie
import HBS2.Data.Types.Refs
import HBS2.System.Dir

import HBS2.Git.Local

import HBS2.Git3.State.Types

import DBPipe.SQLite

import System.Directory
import Data.Maybe

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
gitpack
  ( kommit  text not null primary key
  , pack    text not null
  )
|]


instance ToField GitHash where
  toField h = toField (show $ pretty h)

instance ToField GitRef where
  toField h = toField (show $ pretty h)

instance FromField GitRef where
  fromField = fmap fromString . fromField @String

instance FromField GitHash where
  fromField = fmap fromString . fromField @String

instance ToField HashRef where
  toField x = toField $ show $ pretty x

instance FromField HashRef where
  fromField = fmap (fromString @HashRef) . fromField @String

insertGitPack :: MonadIO m => GitHash -> HashRef -> DBPipeM m ()
insertGitPack co pack = do
  insert [qc|
    insert into gitpack (kommit,pack) values(?,?)
    on conflict (kommit) do update set pack = excluded.pack
         |] (co, pack)

selectGitPack :: MonadIO m => GitHash -> DBPipeM m (Maybe HashRef)
selectGitPack gh = do
  select [qc|select pack from gitpack where kommit = ? limit 1|] (Only gh)
   <&> listToMaybe . fmap fromOnly


