module HBS2.Git.Oracle.State where

import HBS2.Git.Oracle.Prelude
import DBPipe.SQLite

import Data.Coerce
import Text.InterpolatedString.Perl6 (qc)
import Data.Word

evolveDB :: MonadUnliftIO m => DBPipeM m ()
evolveDB = do
  debug $ yellow "evolveDB"
  gitRepoTable
  gitRepoNameTable
  gitRepoBriefTable
  gitRepoHeadVersionTable
  txProcessedTable

txProcessedTable :: MonadUnliftIO m => DBPipeM m ()
txProcessedTable = do
  ddl [qc|
    create table if not exists txprocessed
    ( hash text not null primary key
    )
  |]

gitRepoTable :: MonadUnliftIO m => DBPipeM m ()
gitRepoTable = do
  ddl [qc|
    create table if not exists gitrepo
    ( ref text not null primary key
    )
  |]


gitRepoNameTable :: MonadUnliftIO m => DBPipeM m ()
gitRepoNameTable = do
  ddl [qc|
    create table if not exists gitreponame
    ( ref text not null
    , hash text not null
    , name text not null
    , primary key (ref, hash)
    )
  |]

gitRepoBriefTable :: MonadUnliftIO m => DBPipeM m ()
gitRepoBriefTable = do
  ddl [qc|
    create table if not exists gitrepobrief
    ( ref text not null
    , hash text not null
    , brief text not null
    , primary key (ref, hash)
    )
  |]

gitRepoHeadVersionTable :: MonadUnliftIO m => DBPipeM m ()
gitRepoHeadVersionTable = do
  ddl [qc|
    create table if not exists gitrepoheadversion
    ( hash text not null
    , version integer not null
    , primary key (hash)
    )
  |]

newtype GitRepoKey = GitRepoKey (LWWRefKey HBS2Basic)
                     deriving stock Generic

newtype HashVal    = HashVal HashRef
                     deriving stock Generic

instance ToField GitRepoKey where
  toField (GitRepoKey r) = toField $ show $ pretty $ AsBase58 r

instance ToField HashVal where
  toField (HashVal r) = toField $ show $ pretty $ AsBase58 r

insertGitRepo :: MonadUnliftIO m => GitRepoKey -> DBPipeM m ()
insertGitRepo repo = do
  insert [qc|
    insert into gitrepo(ref) values(?)
    on conflict (ref) do nothing
  |] (Only repo)

insertGitRepoName :: MonadUnliftIO m
                  => (GitRepoKey, HashVal)
                  -> Text
                  -> DBPipeM m ()
insertGitRepoName (r,h)  name = do
  insert [qc|
    insert into gitreponame (ref,hash,name) values(?,?,?)
    on conflict (ref,hash) do update set name = excluded.name
  |] (r,h,name)

insertGitRepoBrief :: MonadUnliftIO m
                   => (GitRepoKey, HashVal)
                   -> Text
                   -> DBPipeM m ()
insertGitRepoBrief (r,h) b = do
  insert [qc|
    insert into gitrepobrief (ref,hash,brief) values(?,?,?)
    on conflict (ref,hash) do update set brief = excluded.brief
  |] (r,h,b)


insertGitRepoHeadVersion :: MonadUnliftIO m => HashVal -> Word64 -> DBPipeM m ()
insertGitRepoHeadVersion hashVal version = do
  insert [qc|
    insert into gitrepoheadversion (hash, version) values(?,?)
    on conflict (hash) do update set version = excluded.version
  |] (hashVal, version)


insertTxProcessed :: MonadUnliftIO m => HashVal -> DBPipeM m ()
insertTxProcessed hash = do
  insert [qc|
    insert into txprocessed (hash) values (?)
    on conflict do nothing
  |] (Only hash)


isTxProcessed :: MonadUnliftIO m => HashVal -> DBPipeM m Bool
isTxProcessed hash = do
  results <- select [qc|
    select 1 from txprocessed where hash = ?
  |] (Only hash)
  pure $ not $ null (results :: [Only Int])


