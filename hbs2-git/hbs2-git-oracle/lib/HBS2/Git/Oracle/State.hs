{-# Language AllowAmbiguousTypes #-}
{-# Language UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}
module HBS2.Git.Oracle.State where

import HBS2.Git.Oracle.Prelude
import HBS2.Hash

import HBS2.Git.Oracle.Facts

import DBPipe.SQLite hiding (insert,columnName)
import DBPipe.SQLite qualified as SQL
import DBPipe.SQLite.Generic

import GHC.Generics
import Data.Aeson
import Text.InterpolatedString.Perl6 (qc)
import Data.Coerce
import Data.Word
import Data.Text qualified as Text

processedRepoTx :: (LWWRefKey HBS2Basic, HashRef) -> HashVal
processedRepoTx w = HashVal $ HashRef $ hashObject @HbSync (serialise w)

evolveDB :: MonadUnliftIO m => DBPipeM m ()
evolveDB = do
  debug $ yellow "evolveDB"
  gitRepoFactTable
  txProcessedTable

txProcessedTable :: MonadUnliftIO m => DBPipeM m ()
txProcessedTable = do
  ddl [qc|
    create table if not exists txprocessed
    ( hash text not null primary key
    )
  |]


gitRepoFactTable :: MonadUnliftIO m => DBPipeM m ()
gitRepoFactTable = do
  ddl [qc|
    create table if not exists gitrepofact
    ( lwwref        text not null
    , lwwseq        integer not null
    , reflog        text not null
    , tx            text not null
    , repohead      text not null
    , name          text null
    , brief         text null
    , encrypted     text null
    , primary key (lwwref,seq,reflog,tx,repohead)
    )
  |]

newtype GitRepoKey = GitRepoKey (LWWRefKey HBS2Basic)
                     deriving stock Generic

newtype HashVal    = HashVal HashRef
                     deriving stock Generic

instance ToJSON HashVal where
  toJSON (HashVal x) = toJSON (show $ pretty x)

instance ToField GitRepoKey where
  toField (GitRepoKey r) = toField $ show $ pretty $ AsBase58 r

instance ToField HashVal where
  toField (HashVal r) = toField $ show $ pretty $ AsBase58 r

instance FromField HashVal where
  fromField = fmap (HashVal . fromString @HashRef) . fromField @String

insertGitRepoFact :: MonadUnliftIO m => GitRepoKey -> HashVal -> DBPipeM m ()
insertGitRepoFact repo h = do
  SQL.insert [qc|
    insert into gitrepofact(ref,hash) values(?,?)
    on conflict (ref,hash) do nothing
  |] (repo,h)


insertTxProcessed :: MonadUnliftIO m => HashVal -> DBPipeM m ()
insertTxProcessed hash = do
  SQL.insert [qc|
    insert into txprocessed (hash) values (?)
    on conflict do nothing
  |] (Only hash)


isTxProcessed :: MonadUnliftIO m => HashVal -> DBPipeM m Bool
isTxProcessed hash = do
  results <- select [qc|
    select 1 from txprocessed where hash = ? limit 1
  |] (Only hash)
  pure $ not $ null (results :: [Only Int])



instance HasColumnName GitLwwRef where
  columnName = "lwwref"

instance HasColumnName GitLwwSeq where
  columnName = "lwwseq"

instance HasColumnName GitRefLog where
  columnName = "reflog"


-- instance HasTableName GitRepoFacts where
--   tableName = "gitrepofact"


insertRepoFacts :: (MonadUnliftIO m) => GitRepoFacts -> DBPipeM m ()
insertRepoFacts GitRepoFacts{..} = do
  let sql = insert ( gitLwwRef
                   , gitLwwSeq
                   , gitRefLog
                   )
                   -- ( gitLwwRef
                   -- , gitLwwSeq
                   -- , gitTx
                   -- , gitRepoHead
                   -- , gitName
                   -- , gitBrief
                   -- , gitEncrypted )
  pure ()


