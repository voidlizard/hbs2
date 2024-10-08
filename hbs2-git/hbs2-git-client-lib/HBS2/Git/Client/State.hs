{-# Language UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module HBS2.Git.Client.State
  ( module HBS2.Git.Client.State
  , transactional
  , commitAll
  ) where

import HBS2.Git.Client.Prelude
import HBS2.Git.Client.App.Types
import HBS2.Git.Client.Config

import HBS2.Peer.Proto.RefLog
import HBS2.Storage.Operations.ByteString

import HBS2.Git.Data.RepoHead
import HBS2.Git.Data.RefLog
import HBS2.Git.Data.LWWBlock
import HBS2.Git.Data.Tx.Index
import HBS2.Git.Data.Tx.Git qualified as TX
-- import HBS2.Git.Data.Tx qualified as TX

import DBPipe.SQLite
import Data.Maybe
import Data.List qualified as List
import Text.InterpolatedString.Perl6 (qc)
import Data.Text qualified as Text
import Data.Word
import Data.Coerce

import Streaming.Prelude qualified as S

data Limit = Limit Integer

data SortOrder = ASC | DESC

newtype SQL a = SQL a

instance Pretty (SQL SortOrder) where
  pretty (SQL ASC) = "ASC"
  pretty (SQL DESC) = "DESC"

newtype Base58Field a = Base58Field { fromBase58Field :: a }
                        deriving stock (Eq,Ord,Generic)

instance Pretty (AsBase58 a) => ToField (Base58Field a) where
  toField (Base58Field x) = toField @String (show $ pretty (AsBase58 x))

instance IsString a => FromField (Base58Field a) where
  fromField = fmap (Base58Field . fromString) . fromField @String

instance FromField (RefLogKey 'HBS2Basic) where
  fromField = fmap fromString . fromField @String

instance ToField HashRef where
  toField h = toField @String (show $ pretty h)

instance FromField HashRef where
  fromField = fmap fromString . fromField @String

deriving newtype instance FromField (TaggedHashRef t)

instance ToField GitHash where
  toField h = toField (show $ pretty h)

instance ToField GitRef where
  toField h = toField (show $ pretty h)

instance FromField GitRef where
  fromField = fmap fromString . fromField @String

instance FromField GitHash where
  fromField = fmap fromString . fromField @String

instance FromField (LWWRefKey 'HBS2Basic) where
  fromField = fmap fromString . fromField @String

createStateDir :: (GitPerks m, MonadReader GitEnv m)  => m ()
createStateDir = do
  void $ readConfig True

initState :: (GitPerks m, MonadReader GitEnv m) => m ()
initState = do
  createStateDir
  evolveDB

class WithState m a where
  withState :: DBPipeM m a -> m a

instance (MonadIO m, MonadReader GitEnv m) => WithState m a where
  withState action = do
    env <-  asks _db
    withDB env action


evolveDB :: (GitPerks m, MonadReader GitEnv m) => m ()
evolveDB = withState do
  createTxTable
  createTxDoneTable
  createTxBundleTable
  createBundleDoneTable
  createBundleKeyTable
  createBundleObjectTable
  createNewGK0Table
  createLwwTable
  commitAll

createTxTable :: MonadIO m => DBPipeM m ()
createTxTable = do
  ddl [qc|
create table if not exists tx
  ( reflog   text not null
  , tx       text not null
  , seq      int not null
  , head     text not null
  , bundle   text not null
  , primary key (reflog,tx)
  )
  |]

  ddl [qc|
CREATE INDEX IF NOT EXISTS idx_tx_seq ON tx(seq)
  |]


createTxDoneTable :: MonadIO m => DBPipeM m ()
createTxDoneTable = do
  ddl [qc|
create table if not exists txdone
  ( tx text not null primary key
  )
  |]

createBundleDoneTable :: MonadIO m => DBPipeM m ()
createBundleDoneTable = do
  ddl [qc|
create table if not exists bundledone
  ( hash text primary key
  )
  |]

createBundleKeyTable :: MonadIO m => DBPipeM m ()

createBundleKeyTable = do
  ddl [qc|
create table if not exists bundlekey
  ( reflog text not null
  , key text not null
  , bundle text not null
  , primary key (reflog, key)
  )
  |]


createTxBundleTable :: MonadIO m => DBPipeM m ()
createTxBundleTable = do
  ddl [qc|
create table if not exists txbundle
  ( tx text not null
  , num integer not null
  , bundle text not null
  , primary key (tx, num)
  )
  |]

createBundleObjectTable :: MonadIO m => DBPipeM m ()
createBundleObjectTable = do
  ddl [qc|
create table if not exists bundleobject
  ( bundle text not null
  , object text not null
  , primary key (bundle, object)
  )
  |]

createNewGK0Table :: MonadIO m => DBPipeM m ()
createNewGK0Table = do
  ddl [qc|
create table if not exists newgk0
  ( reflog     text not null
  , tx         text not null
  , ts         int  not null default (strftime('%s','now'))
  , gk0        text not null
  , primary key (reflog,tx)
  )
  |]


createLwwTable :: MonadIO m => DBPipeM m ()
createLwwTable = do
  ddl [qc|
create table if not exists lww
  ( hash      text not null
  , seq       int not null
  , reflog    text not null
  , primary key (hash,seq,reflog)
  )
  |]


existsTx :: MonadIO m => HashRef -> DBPipeM m Bool
existsTx txHash = do
  select @(Only Bool) [qc|
SELECT true FROM tx WHERE tx = ? LIMIT 1
  |] (Only txHash)
  <&> not . List.null

insertTx :: MonadIO m
         => RefLogId
         -> HashRef
         -> Integer
         -> HashRef
         -> HashRef
         -> DBPipeM m ()
insertTx puk tx sn h bundle = do
  insert [qc|
insert into tx (reflog,tx,seq,head,bundle)
values (?,?,?,?,?)
on conflict (reflog,tx) do nothing
  |] (Base58Field puk,tx,sn,h,bundle)


selectTxForRefLog :: MonadIO m
                  => RefLogId
                  -> HashRef
                  -> DBPipeM m (Maybe (HashRef, Epoch))
selectTxForRefLog puk tx = do
  select [qc|
  select head,seq
  from tx where reflog = ? and tx = ?
  limit 1
  |] (Base58Field puk, tx) <&> listToMaybe

selectTxHead :: MonadIO m => HashRef -> DBPipeM m (Maybe HashRef)
selectTxHead txHash = do
  result <- select [qc|
select head from tx where TX = ? limit 1
  |] (Only txHash)
  pure $ listToMaybe $ fmap fromOnly result

selectMaxTxSeq :: MonadIO m => RefLogId -> DBPipeM m Integer
selectMaxTxSeq puk = do
  select [qc|
select max(seq) as seq from tx where reflog = ?
  |] (Only (Base58Field puk))
    <&> maybe 0 fromOnly . listToMaybe

insertTxDone :: MonadIO m => HashRef -> DBPipeM m ()
insertTxDone txHash = do
  insert [qc|
INSERT INTO txdone (tx) VALUES (?)
ON CONFLICT (tx) DO NOTHING
  |] (Only txHash)


existsTxDone :: MonadIO m => HashRef -> DBPipeM m Bool
existsTxDone txHash = do
  select @(Only Bool) [qc|
SELECT true FROM txdone WHERE tx = ? LIMIT 1
  |] (Only txHash)
  <&> not . null

existsAnyTxDone :: MonadIO m => DBPipeM m Bool
existsAnyTxDone = do
  select_ @_ @(Only (Maybe Bool)) [qc|
SELECT true FROM txdone LIMIT 1
  |] <&> not . null

selectMaxSeqTxNotDone :: MonadIO m => RefLogId -> DBPipeM m (Maybe HashRef)
selectMaxSeqTxNotDone puk = do
  select [qc|
WITH MaxDoneSeq AS (
  SELECT MAX(tx.seq) as maxSeq
  FROM tx
  JOIN txdone ON tx.tx = txdone.tx
  WHERE tx.reflog = ?
),
FilteredTx AS (
  SELECT tx.tx, tx.seq
  FROM tx
  LEFT JOIN txdone ON tx.tx = txdone.tx
  WHERE tx.reflog = ? AND txdone.tx IS NULL
)
SELECT ft.tx FROM FilteredTx ft
JOIN MaxDoneSeq mds ON ft.seq > COALESCE(mds.maxSeq, 0)
ORDER BY ft.seq DESC
LIMIT 1
  |] (Base58Field puk, Base58Field puk)
  <&> listToMaybe . fmap fromOnly


selectMaxAppliedTx :: MonadIO m => DBPipeM m (Maybe (HashRef, Integer))
selectMaxAppliedTx = do
  select [qc|
SELECT t.tx, t.seq FROM txdone d JOIN tx t ON d.tx = t.tx ORDER BY t.seq DESC LIMIT 1
  |] ()
  <&> listToMaybe


selectMaxAppliedTxForRepo :: MonadIO m => LWWRefKey 'HBS2Basic -> DBPipeM m (Maybe (HashRef, Integer))
selectMaxAppliedTxForRepo lww = do
  select [qc|
    with rl as (
      select l.hash, l.reflog from lww l where l.hash = ?
      order by seq desc limit 1
      )
    select t.tx, t.seq
    from txdone d join tx t on d.tx = t.tx
                  join rl on rl.reflog = t.reflog
    order by t.seq desc limit 1
  |] (Only (Base58Field lww))
  <&> listToMaybe

insertBundleDone :: MonadIO m => HashRef -> DBPipeM m ()
insertBundleDone hashRef = do
  insert [qc|
INSERT INTO bundledone (hash) VALUES (?)
ON CONFLICT (hash) DO NOTHING
  |] (Only hashRef)

existsBundleDone :: MonadIO m => HashRef -> DBPipeM m Bool
existsBundleDone hashRef = do
  select @(Only Bool) [qc|
SELECT true FROM bundledone WHERE hash = ? LIMIT 1
  |] (Only hashRef)
  <&> not . null


insertBundleKey :: MonadIO m => RefLogId -> HashRef -> HashRef -> DBPipeM m ()
insertBundleKey reflogId keyHash bundleHash = do
  insert [qc|
INSERT INTO bundlekey (reflog, key, bundle) VALUES (?, ?, ?)
ON CONFLICT (reflog,key) DO NOTHING
  |] (Base58Field reflogId, keyHash, bundleHash)

selectBundleByKey :: MonadIO m => RefLogId -> HashRef -> DBPipeM m (Maybe HashRef)
selectBundleByKey reflogId keyHash = do
  select [qc|
SELECT bundle FROM bundlekey WHERE reflog = ? AND key = ? LIMIT 1
  |] (Base58Field reflogId, keyHash)
  <&> listToMaybe . fmap fromOnly

insertTxBundle :: MonadIO m => HashRef -> Int -> HashRef -> DBPipeM m ()
insertTxBundle tx num bundleHash = do
  insert [qc|
INSERT INTO txbundle (tx, num, bundle) VALUES (?, ?, ?)
ON CONFLICT (tx, num) DO UPDATE SET bundle = EXCLUDED.bundle
  |] (tx, num, bundleHash)

insertBundleObject :: MonadIO m => HashRef -> GitHash -> DBPipeM m ()
insertBundleObject bundle object = do
  insert [qc|
insert into bundleobject (bundle, object) values (?, ?)
on conflict (bundle, object) do nothing
  |] (bundle, object)


selectBundleObjects :: MonadIO m => HashRef -> DBPipeM m [GitHash]
selectBundleObjects bundle = do
  select [qc|
select object from bundleobject where bundle = ?
  |] (Only bundle)
  <&> fmap fromOnly


selectObjectsForTx:: MonadIO m => HashRef -> DBPipeM m [GitHash]
selectObjectsForTx txHash = do
  select [qc|
select distinct bundleobject.object
from txbundle
join bundleobject on txbundle.bundle = bundleobject.bundle
where txbundle.tx = ?
  |] (Only txHash) <&> fmap fromOnly


isObjectInTx :: MonadIO m => HashRef -> GitHash -> DBPipeM m Bool
isObjectInTx txHash objectHash = do
  result <- select @(Only Int) [qc|
select 1
from txbundle
join bundleobject on txbundle.bundle = bundleobject.bundle
where txbundle.tx = ? and bundleobject.object = ?
limit 1
  |] (txHash, objectHash)
  pure $ not (null result)


insertNewGK0 :: MonadIO m => RefLogId -> HashRef -> HashRef -> DBPipeM m ()
insertNewGK0 reflog tx gk0 = do
  insert [qc|
insert into newgk0 (reflog, tx, gk0) values (?, ?, ?)
on conflict (reflog,tx) do update set gk0 = excluded.gk0
  |] (Base58Field reflog, tx, gk0)

selectNewGK0 :: MonadIO m => RefLogId -> DBPipeM m (Maybe (HashRef,Epoch))
selectNewGK0 reflog = do
  select [qc|
select gk0, ts
from newgk0 g
where g.reflog = ?
order by ts desc
limit 1
  |] (Only (Base58Field reflog)) <&> listToMaybe


insertLww :: MonadIO m => LWWRefKey 'HBS2Basic -> Word64 -> RefLogId -> DBPipeM m ()
insertLww lww snum reflog = do
  insert [qc|
INSERT INTO lww (hash, seq, reflog) VALUES (?, ?, ?)
ON CONFLICT (hash,seq,reflog) DO NOTHING
  |] (Base58Field lww, snum, Base58Field reflog)

selectAllLww :: MonadIO m => DBPipeM m [(LWWRefKey 'HBS2Basic, Word64, RefLogId)]
selectAllLww = do
  select_  [qc|
SELECT hash, seq, reflog FROM lww
  |] <&> fmap (over _3 (fromRefLogKey @'HBS2Basic))



selectRepoHeadsFor :: (MonadIO m)
                    => SortOrder
                    -> LWWRefKey 'HBS2Basic
                    -> DBPipeM m [TaggedHashRef RepoHead]

selectRepoHeadsFor order what = do
  let q = [qc|
SELECT t.head
FROM lww l join tx t on l.reflog = t.reflog
WHERE l.hash = ?
ORDER BY t.seq {pretty (SQL order)}
|]

  select @(Only (TaggedHashRef RepoHead)) q (Only $ Base58Field what)
   <&> fmap fromOnly

selectLastRepoHeadFor :: (MonadIO m)
                    => LWWRefKey 'HBS2Basic
                    -> DBPipeM m (Maybe (TaggedHashRef RepoHead))

selectLastRepoHeadFor what = do
  let q = [qc|
SELECT t.head
FROM lww l join tx t on l.reflog = t.reflog
WHERE l.hash = ?
ORDER BY t.seq DESC
LIMIT 1
|]

  select @(Only (TaggedHashRef RepoHead)) q (Only $ Base58Field what)
   <&> (fmap fromOnly . listToMaybe)

instance (Monad m, HasStorage m) => HasStorage (DBPipeM m) where
  getStorage = lift getStorage

selectRepoIndexEntryFor :: (MonadIO m, HasStorage m)
                        => LWWRefKey 'HBS2Basic
                        -> DBPipeM m (Maybe GitIndexRepoDefineData)

selectRepoIndexEntryFor what = runMaybeT do

  headz <- lift $ selectRepoHeadsFor DESC what

  rhh <- S.head_ do
            for_ headz $ \ha -> do
              rh' <- lift $ loadRepoHead ha
              for_ rh' $ \rh -> do
                when (notEmpty $ _repoManifest  rh) do
                  S.yield rh


  repohead <- toMPlus rhh

  pure $ GitIndexRepoDefineData (GitIndexRepoName $ _repoHeadName repohead)
                                (GitIndexRepoBrief $ _repoHeadBrief repohead)


  where
    notEmpty s = maybe 0 Text.length s > 0

loadRepoHead :: (HasStorage m, MonadIO m) => TaggedHashRef RepoHead -> m (Maybe RepoHead)
loadRepoHead rh = do
  sto <- getStorage
  runMaybeT do
    runExceptT (readFromMerkle sto (SimpleKey (coerce rh)))
       >>= toMPlus
       <&> deserialiseOrFail @RepoHead
       >>= toMPlus

readActualRepoHeadFor :: ( HasStorage m
                         , MonadReader GitEnv m
                         , MonadIO m
                         )
                      => LWWRefKey 'HBS2Basic -> m (Maybe RepoHead)

readActualRepoHeadFor lww = do
  sto <- getStorage
  runMaybeT do
    tx <- lift ( withState $
             selectMaxAppliedTxForRepo lww
             <&> fmap fst
             ) >>= toMPlus

    (_,rh) <- TX.readRepoHeadFromTx sto tx >>= toMPlus
    pure rh

