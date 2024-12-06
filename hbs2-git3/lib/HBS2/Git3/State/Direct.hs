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
cblock
  ( kommit  text not null primary key
  , cblock  text not null
  , unique (kommit,cblock)
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

insertCBlock :: MonadIO m => GitHash -> HashRef -> DBPipeM m ()
insertCBlock co cblk = do
  insert [qc|
    insert into cblock (kommit, cblock) values(?,?)
    on conflict (kommit) do update set cblock = excluded.cblock
    on conflict (kommit,cblock) do nothing
         |] (co, cblk)

selectCBlock :: MonadIO m => GitHash -> DBPipeM m (Maybe HashRef)
selectCBlock gh = do
  select [qc|select cblock from cblock where kommit = ? limit 1|] (Only gh)
   <&> listToMaybe . fmap fromOnly

