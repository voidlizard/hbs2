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

data GitRepoPage =
  GitRepoPage
  { repoPageRef      :: GitLwwRef
  , repoPageHead     :: GitRepoHeadRef
  , repoPageName     :: GitName
  , repoPageBrief    :: GitBrief
  , repoPageManifest :: GitManifest
  , repoPageGK0      :: GitEncrypted
  }
  deriving stock (Generic,Data)

instance FromRow GitRepoPage


data GitRepoListEntry =
  GitRepoListEntry
  { listEntryRef   :: GitLwwRef
  , listEntrySeq   :: GitLwwSeq
  , listEntryHead  :: GitRepoHeadRef
  , listEntryName  :: GitName
  , listEntryBrief :: GitBrief
  , listEntryGK0   :: GitEncrypted
  }
  deriving stock (Generic,Data)

instance ToJSON GitRepoListEntry

instance FromRow GitRepoListEntry

processedRepoTx :: (LWWRefKey HBS2Basic, HashRef) -> HashVal
processedRepoTx w = HashVal $ HashRef $ hashObject @HbSync (serialise w)

evolveDB :: MonadUnliftIO m => DBPipeM m ()
evolveDB = do
  debug $ yellow "evolveDB"
  gitRepoFactTable
  gitRepoManifestTable
  gitRepoFactView
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
    , repoheadseq   integer not null
    , name          text null
    , brief         text null
    , gk            text null
    , primary key (lwwref,lwwseq,reflog,tx,repohead)
    )
  |]


gitRepoManifestTable :: MonadUnliftIO m => DBPipeM m ()
gitRepoManifestTable = do
  ddl [qc|
    create table if not exists gitrepomanifest
    ( repohead      text not null
    , manifest      text
    , primary key (repohead)
    )
  |]


gitRepoFactView :: MonadUnliftIO m => DBPipeM m ()
gitRepoFactView = do
  ddl [qc|DROP VIEW IF EXISTS vrepofact|]
  ddl [qc|
    CREATE VIEW IF NOT EXISTS vrepofact AS
    SELECT
      lwwref,
      repoheadseq as lwwseq,
      repohead,
      name,
      brief,
      gk
    FROM (
      SELECT
        lwwref,
        repohead,
        name,
        brief,
        repoheadseq,
        gk,
        ROW_NUMBER() OVER (PARTITION BY lwwref ORDER BY lwwseq DESC, repoheadseq DESC) as rn
      FROM gitrepofact
    ) as s0
    WHERE rn = 1;
  |]

newtype GitRepoKey = GitRepoKey (LWWRefKey HBS2Basic)
                     deriving stock Generic

newtype HashVal    = HashVal HashRef
                     deriving stock Generic
                     deriving newtype (Pretty,Show)

instance ToJSON HashVal where
  toJSON (HashVal x) = toJSON (show $ pretty x)

instance ToField GitRepoKey where
  toField (GitRepoKey r) = toField $ show $ pretty $ AsBase58 r

instance ToField HashVal where
  toField (HashVal r) = toField $ show $ pretty $ AsBase58 r

instance FromField HashVal where
  fromField = fmap (HashVal . fromString @HashRef) . fromField @String


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


insertRepoFacts :: (MonadUnliftIO m) => GitRepoFacts -> DBPipeM m ()
insertRepoFacts facts@GitRepoFacts{..} = do
 insert @GitRepoFacts $
   onConflictIgnore @GitRepoFacts
    ( gitLwwRef
    , gitLwwSeq
    , gitRefLog
    , gitTx
    , gitRepoHead
    , gitRepoHeadSeq
    , gitName
    , gitBrief
    , gitEncrypted
    )
 let mf = [ m | m :: GitManifest <- universeBi facts ]
 for_ mf $ \m@GitManifest{} -> do
  insert @GitManifest $ onConflictIgnore @GitManifest (gitRepoHead, m)
  pure ()

  -- insert @GitManifest ( gitRepoHead,


