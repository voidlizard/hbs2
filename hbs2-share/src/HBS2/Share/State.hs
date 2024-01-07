{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language TemplateHaskell #-}
module HBS2.Share.State where

import HBS2.Prelude
import HBS2.Hash
import HBS2.Share.App.Types
import HBS2.Share.Keys
import HBS2.Share.LocalHash
import HBS2.Share.MetaData

import DBPipe.SQLite

import Text.InterpolatedString.Perl6 (qc)
import Data.Maybe
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.List qualified as List

data LocalFile =
  LocalFile
  { _localFileKey     :: EntryKey
  , _localFileModTime :: UTCTime
  , _localFileHash    :: LocalHash
  }
  deriving stock (Generic)

makeLenses 'LocalFile

data RemoteFile =
  RemoteFile
  { _remoteFileKey   :: EntryKey
  , _remoteFileTime  :: UTCTime
  , _remoteLocalHash :: LocalHash
  , _remoteTree      :: HashRef
  }
  deriving stock (Generic)

makeLenses 'RemoteFile

instance FromRow LocalFile

instance FromRow RemoteFile

class HasHash a where
  toHash :: a -> Hash HbSync

instance HasHash (Hash HbSync) where
  toHash = id

instance HasHash HashRef  where
  toHash = fromHashRef

newtype HashVal = HashVal { fromHashVal :: HashRef }
                  deriving newtype (IsString)

instance ToField GK0Key where
  toField (GK0Key hs) = toField (show (pretty hs))

instance ToField HashVal where
  toField (HashVal v) = toField (show (pretty v))

instance FromField HashVal where
  fromField = fmap fromString . fromField @String

instance ToField EntryKey where
  toField p = toField (show $ pretty p)

instance FromField EntryKey where
  fromField = fmap (makeEntryKey mempty) . fromField @String

instance ToField LocalHash where
  toField (LocalHash l) = toField (HashVal (HashRef l))

instance FromField LocalHash where
  fromField = fmap (LocalHash . fromHashRef . fromHashVal) . fromField @HashVal

instance FromField HashRef where
  fromField = fmap fromHashVal . fromField @HashVal

populateState :: MonadUnliftIO m => DBPipeM m ()
populateState = do
  ddl [qc|create table if not exists gk0
          ( hash text not null
          , gk0  text not null
          , ts  datetime default current_timestamp
          , primary key (hash)
          )
        |]

  ddl [qc|create table if not exists localfile
          ( key        text not null
          , modtime    datetime not null
          , localhash  text not null
          , primary key (key)
          )
         |]

  ddl [qc|create table if not exists localtree
          ( key        text not null
          , tree       text not null
          , primary key (key)
          )
         |]

  ddl [qc|create table if not exists accept
          ( accept     text not null
          , propose    text not null
          , epoch      int not null
          , primary key (accept)
          )
         |]

  ddl [qc|create table if not exists propose
          ( propose    text not null
          , tx         text not null
          , primary key (propose)
          )
         |]


  ddl [qc|create table if not exists missed
          ( hash   text not null
          , missed bool not null
          , primary key (hash)
          )
         |]

  createRemoteFileTable

  commitAll


insertGK0 :: MonadUnliftIO m => GK0Key -> HashRef -> DBPipeM m ()
insertGK0 gk0 val = do
  insert [qc|
    insert into gk0 (hash, gk0) values (?,?)
    on conflict do update set gk0 = excluded.gk0
  |] (gk0, HashVal val)


selectGK0 :: MonadUnliftIO m => GK0Key -> DBPipeM m (Maybe HashRef)
selectGK0 gk0 = do
  -- FIXME: time-hardcode
  select [qc|
    select gk0 from gk0
    where hash = ? and ts > datetime('now', '-30 days');
    limit 1
    |] (Only gk0)
    <&> listToMaybe . fmap (fromHashVal . fromOnly)

insertLocalFile :: MonadUnliftIO m
                => EntryKey
                -> UTCTime
                -> LocalHash
                -> DBPipeM m ()

insertLocalFile fkey modtime localhash = do
  insert [qc|
    insert into localfile (key, modtime, localhash) values (?,?,?)
    on conflict (key) do update set modtime = excluded.modtime
                                  , localhash = excluded.localhash
  |] (fkey, modtime, localhash)


selectLocalFile :: MonadUnliftIO m => EntryKey -> DBPipeM m (Maybe LocalFile)
selectLocalFile fkey = do
  select [qc|
    select key
         , modtime
         , localhash
    from localfile
    where key = ?;
    limit 1
  |] (Only fkey)
  <&> listToMaybe

selectLocalFiles :: MonadUnliftIO m => DBPipeM m [LocalFile]
selectLocalFiles = do
  select_ [qc|
    select key, modtime, localhash
    from localfile
  |]

insertLocalTree :: forall hx m . (MonadUnliftIO m, HasHash hx)
                => EntryKey
                -> hx
                -> DBPipeM m ()
insertLocalTree fkey tree = do
  insert [qc|
    insert into localtree (key, tree) values (?,?)
    on conflict (key) do update set tree = excluded.tree
  |] (fkey, HashVal (HashRef (toHash tree)))


selectLocalTrees :: forall m . ( MonadUnliftIO m )
                 => DBPipeM m [(EntryKey, LocalHash, HashRef)]
selectLocalTrees = do
  select_ [qc| select t.key
                    , f.localhash
                    , t.tree
               from localtree t join localfile f on t.key = f.key|]
   <&> fmap (over _3 fromHashVal)


insertAccept :: forall hx m . ( MonadUnliftIO m, HasHash hx )
             => hx
             -> hx
             -> Integer
             -> DBPipeM m ()

insertAccept k p t = do
  insert [qc|
    insert into accept (accept,propose,epoch) values (?,?,?)
    on conflict (accept) do nothing
  |] (HashVal (HashRef $ toHash k), HashVal (HashRef $ toHash p), t)

insertPropose :: forall hx m . ( MonadUnliftIO m, HasHash hx )
             => hx
             -> hx
             -> DBPipeM m ()

insertPropose k tx = do
  insert [qc|
    insert into propose (propose,tx) values (?,?)
    on conflict (propose) do nothing
  |] (HashVal (HashRef $ toHash k), HashVal (HashRef $ toHash tx))


selectProposes :: forall m . MonadUnliftIO m => DBPipeM m [(HashRef, Integer)]
selectProposes = do
  let q = [qc|
WITH RankedAccept AS (
    SELECT a.propose,
           a.epoch,
           ROW_NUMBER() OVER (PARTITION BY a.propose ORDER BY a.epoch) AS rn,
           COUNT(*) OVER (PARTITION BY a.propose) AS cnt
    FROM accept a
),
T0 AS (
SELECT p.propose,
       p.tx,
       cast(AVG(a.epoch) as int) AS epoch
FROM propose p
JOIN RankedAccept a ON p.propose = a.propose
WHERE a.rn IN ((a.cnt + 1) / 2, (a.cnt / 2) + 1)
GROUP BY p.propose, p.tx )

SELECT T0.tx, T0.epoch
FROM T0
ORDER BY T0.epoch DESC|]

  select_ q <&> fmap (over _1 fromHashVal)

selectMissed :: MonadUnliftIO m => HashRef -> DBPipeM m (Maybe Bool)
selectMissed hash = do
  select [qc|
    select missed from missed where hash = ? limit 1
  |] (Only (HashVal hash)) <&> fmap fromOnly . listToMaybe

insertMissed :: MonadUnliftIO m => HashRef -> Bool -> DBPipeM m ()
insertMissed hash miss = do
  insert [qc|
    insert into missed (hash,missed) values (?,?)
    on conflict (hash) do update set missed = excluded.missed
  |] (HashVal hash, miss)

deleteMissed :: MonadUnliftIO m => HashRef -> DBPipeM m ()
deleteMissed hash = do
  insert [qc|
    delete from missed where hash = ?
  |] (Only (HashVal hash))


createRemoteFileTable :: MonadUnliftIO m => DBPipeM m ()
createRemoteFileTable = do
  ddl [qc|create table if not exists remotefile
          ( propose    text not null
          , key        text not null
          , localhash  text not null
          , tree       text not null
          , time       datetime not null
          , primary key (propose,key)
          )
         |]

insertRemoteFile :: ( MonadUnliftIO m
                    , Real epoch
                    , Fractional epoch
                    )
                 => HashRef
                 -> epoch
                 -> MetaData
                 -> FileEntry
                 -> DBPipeM m ()
insertRemoteFile px epoch _ fe = do
  insert [qc|
    insert into remotefile
      ( propose
      , key
      , localhash
      , tree
      , time
      )
    values (?,?,?,?,?)
    on conflict (propose,key)
       do update
          set localhash = excluded.localhash
            , tree      = excluded.tree
            , time      = excluded.time

    |] ( HashVal px
       , _feKey fe
       , _feLocalHash fe
       , HashVal (_feTree fe)
       , posixSecondsToUTCTime $ realToFrac epoch
       )

selectRemoteFiles :: (MonadUnliftIO m)
                  => HashRef
                  -> DBPipeM m [RemoteFile]
selectRemoteFiles px = do
  select [qc|
    select key
         , time
         , localhash
         , tree
    from remotefile where propose = ?
  |] (Only (HashVal px))


selectRemoteFile :: (MonadUnliftIO m)
                  => HashRef
                  -> EntryKey
                  -> DBPipeM m (Maybe RemoteFile)
selectRemoteFile px k = do
  select [qc|
    select key
         , time
         , localhash
         , tree
    from remotefile where propose = ? and key = ?
    limit 1
  |] (HashVal px, k) <&> listToMaybe

