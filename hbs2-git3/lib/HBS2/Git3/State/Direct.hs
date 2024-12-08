{-# OPTIONS_GHC -fno-warn-orphans #-}
module HBS2.Git3.State.Direct
  ( module HBS2.Git3.State.Direct
  , module HBS2.Git3.State.Types
  ) where

import HBS2.Prelude.Plated
import HBS2.OrDie
import HBS2.Data.Types.Refs
import HBS2.System.Dir

import HBS2.Git3.Config.Local
import HBS2.Git.Local
import HBS2.Git.Local.CLI (findGitDir)

import HBS2.Git3.State.Types

import DBPipe.SQLite as SQL

import System.Directory
import Control.Monad.Trans.Maybe
import Data.Maybe
import Data.Word
import Data.List qualified as List

import Text.InterpolatedString.Perl6 (qc)

unit :: FilePath
unit = "hbs2-git"

getStatePath :: (MonadIO m, DBRef db) => db -> m FilePath
getStatePath p = do
  d <- getConfigPath
  pure $ d </> show (pretty p)

getStatePathDB :: (MonadIO m, DBRef db) => db -> m FilePath
getStatePathDB p = do
  getStatePath p <&> (</> "state" </> "state.db")

withState :: (MonadIO m, HasStateDB m) => DBPipeM m a -> m a
withState action = getStateDB >>= flip withDB action

evolveState :: (MonadIO m, HasStateDB m) => m ()
evolveState = do
  withState do

    ddl [qc|
create table if not exists
cblock
  ( id      integer primary key autoincrement
  , cblock  text not null
  , unique  (cblock)
  )
|]

    ddl [qc|
create table if not exists
kommit
  ( kommit  text primary key
  , cblock  integer not null
  )
|]

    ddl [qc|
create table if not exists
imported ( cblock integer primary key )
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

data DatabaseError =
  SomeDatabaseError
  deriving stock (Typeable,Show)

instance Exception DatabaseError

insertImported :: MonadUnliftIO m => HashRef -> DBPipeM m ()
insertImported cblock = void $ runMaybeT do
  (n,_) <- lift (selectCBlockByHash cblock) >>= toMPlus
  lift do
    insert [qc| insert into imported (cblock) values(?)
                on conflict (cblock) do nothing
              |] (Only n)

selectImported :: MonadUnliftIO m => HashRef -> DBPipeM m Bool
selectImported cblock = do
  select @(Only Bool)
      [qc| select true from imported i join cblock c on c.id = i.cblock
           where c.cblock = ?
           limit 1
         |] (Only cblock)
  <&> not . List.null

insertCBlock :: MonadUnliftIO m => GitHash -> HashRef -> DBPipeM m ()
insertCBlock co cblk = do
  transactional do
    n <- select @(Only Word32) [qc|
      insert into cblock (cblock) values(?)
      on conflict (cblock) do update set cblock = excluded.cblock
      returning id |]
            (Only cblk)
            <&> listToMaybe . fmap fromOnly
            >>= orThrow SomeDatabaseError

    insert [qc| insert into kommit (kommit,cblock) values(?,?)
                on conflict (kommit) do update set cblock = excluded.cblock
           |] (co,n)

selectCBlockByHash :: MonadIO m => HashRef -> DBPipeM m (Maybe (Word32, HashRef))
selectCBlockByHash cblock = do
  select [qc| select c.id, c.cblock
              from cblock c
              where c.cblock = ? limit 1|] (Only cblock)
   <&> listToMaybe

selectCBlock :: MonadIO m => GitHash -> DBPipeM m (Maybe (Word32, HashRef))
selectCBlock gh = do
  select [qc| select c.id, c.cblock
              from kommit k join cblock c on k.cblock = c.id
              where kommit = ? limit 1|] (Only gh)
   <&> listToMaybe

-- selectCBlockId :: MonadIO m => HashRef -> DBPipeM m (Maybe Word32)
-- selectCBlockId hh = do
--   select [qc|select id from cblock where cblock = ? limit 1|] (Only hh)
--    <&> fmap fromOnly . listToMaybe

-- selectCommitsByCBlock :: MonadIO m => HashRef -> DBPipeM m [GitHash]
-- selectCommitsByCBlock cb = do
--   select [qc|select kommit from cblock where cblock = ? limit 1|] (Only cb)
--    <&> fmap fromOnly

