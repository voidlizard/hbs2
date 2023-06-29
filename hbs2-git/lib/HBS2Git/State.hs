module HBS2Git.State where

import HBS2Git.Types
import HBS2.Data.Types.Refs
import HBS2.Git.Types
import HBS2.Hash

import HBS2.System.Logger.Simple

import Data.Functor
import Data.Function
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import Control.Monad.Reader
import Text.InterpolatedString.Perl6 (qc)
import Data.String
import Data.ByteString.Lazy.Char8 qualified as LBS
import System.Directory
import System.FilePath
import Data.Maybe
import Data.Text (Text)
import Prettyprinter
import Data.UUID.V4 qualified as UUID
import Control.Monad.Catch
import Control.Concurrent.STM
import System.IO.Unsafe
import Data.Graph (graphFromEdges, topSort)
import Data.Map qualified as Map

-- FIXME: move-orphans-to-separate-module

instance ToField GitHash where
  toField h = toField (show $ pretty h)

instance ToField GitRef where
  toField h = toField (show $ pretty h)

instance FromField GitRef where
  fromField = fmap fromString . fromField @String

instance FromField GitHash where
  fromField = fmap fromString . fromField @String

instance FromField GitObjectType where
  fromField = fmap fromString . fromField @String

instance ToField HashRef where
  toField h = toField (show $ pretty h)

instance ToField GitObjectType where
  toField h = toField (show $ pretty h)

instance FromField HashRef where
  fromField = fmap fromString . fromField @String


newtype DB m a =
  DB { fromDB :: ReaderT DBEnv m a }
  deriving newtype ( Applicative
                   , Functor
                   , Monad
                   , MonadIO
                   , MonadReader Connection
                   , MonadTrans
                   , MonadThrow
                   , MonadCatch
                   )

instance (HasRefCredentials m) => HasRefCredentials (DB m) where
  getCredentials = lift . getCredentials
  setCredentials r s  = lift (setCredentials r s)

dbConnTV :: TVar (Maybe DBEnv)
dbConnTV = unsafePerformIO $ newTVarIO Nothing
{-# NOINLINE dbConnTV #-}

dbEnv :: MonadIO m => FilePath -> m DBEnv
dbEnv fp = do
  trace  "dbEnv called"
  let dir = takeDirectory fp
  liftIO $ createDirectoryIfMissing True dir
  mbDb <- liftIO $ readTVarIO dbConnTV

  case mbDb of
    Nothing -> do
      co <- liftIO $ open fp
      liftIO $ atomically $ writeTVar dbConnTV (Just co)
      withDB co stateInit
      pure co

    Just db -> pure db

withDB :: DBEnv -> DB m a -> m a
withDB env action = runReaderT (fromDB action) env

stateInit :: MonadIO m => DB m ()
stateInit = do
  conn <- ask
  liftIO $ execute_ conn [qc|
  create table if not exists logrefval
  ( loghash text not null
  , refname text not null
  , refval  text not null
  , primary key (loghash, refname)
  )
  |]

  liftIO $ execute_ conn [qc|
  create table if not exists logobject
  ( loghash text not null
  , type text not null
  , githash text not null
  , primary key (loghash, githash)
  )
  |]

  liftIO $ execute_ conn [qc|
  create table if not exists logcommitparent
  ( kommit text not null
  , parent text not null
  , primary key (kommit,parent)
  )
  |]

  liftIO $ execute_ conn [qc|
  create table if not exists logimported
  ( hash text not null
  , primary key (hash)
  )
  |]

  liftIO $ execute_ conn [qc|
  create table if not exists refimported
  ( hash text not null
  , timestamp DATETIME DEFAULT CURRENT_TIMESTAMP
  , primary key (hash)
  )
  |]

  liftIO $ execute_ conn [qc|
  create table if not exists tranimported
  ( hash text not null
  , timestamp DATETIME DEFAULT CURRENT_TIMESTAMP
  , primary key (hash)
  )
  |]

  liftIO $ execute_ conn [qc|
  DROP VIEW IF EXISTS v_refval_actual;
  |]

  liftIO $ execute_ conn [qc|
    CREATE view v_refval_actual AS
    WITH a1 as (
      SELECT
         l.refname
       , l.refval
       , vd.depth

      FROM logrefval l
      JOIN v_log_depth vd on vd.loghash = l.loghash )

    SELECT a1.refname, a1.refval, MAX(a1.depth) from a1
    GROUP by a1.refname
    HAVING a1.refval <> '0000000000000000000000000000000000000000' ;
  |]

  liftIO $ execute_ conn [qc|
  CREATE TABLE IF NOT EXISTS logcommitdepth
  ( kommit text not null
  , depth integer not null
  , primary key (kommit)
  );
  |]

  liftIO $ execute_ conn [qc|
  DROP VIEW IF EXISTS v_log_depth;
  |]

  liftIO $ execute_ conn [qc|
  CREATE VIEW v_log_depth AS
  SELECT
      lo.loghash,
      MAX(ld.depth) AS depth
  FROM logobject lo
  JOIN logcommitdepth ld ON lo.githash = ld.kommit
  WHERE lo.type in ( 'commit', 'context' )
  GROUP BY lo.loghash;
  |]


newtype Savepoint =
  Savepoint String
  deriving newtype (IsString)
  deriving stock (Eq,Ord)

savepointNew :: forall m . MonadIO m => DB m Savepoint
savepointNew  = do
  uu <- liftIO UUID.nextRandom
  let s = LBS.pack (show uu) & hashObject @HbSync & pretty & show
  pure $ fromString ("sp" <> s)

savepointBegin :: forall m . MonadIO m => Savepoint -> DB m ()
savepointBegin (Savepoint sp) = do
  conn <- ask
  liftIO $ execute_ conn [qc|SAVEPOINT {sp}|]

savepointRelease:: forall m . MonadIO m => Savepoint -> DB m ()
savepointRelease (Savepoint sp) = do
  conn <- ask
  liftIO $ execute_ conn [qc|RELEASE SAVEPOINT {sp}|]

savepointRollback :: forall m . MonadIO m => Savepoint -> DB m ()
savepointRollback (Savepoint sp) = do
  conn <- ask
  liftIO $ execute_ conn [qc|ROLLBACK TO SAVEPOINT {sp}|]

transactional :: forall a m . (MonadCatch m, MonadIO m) => DB m a -> DB m a
transactional action = do

  sp <- savepointNew

  savepointBegin sp
  r <- try action

  case r of
    Left (e :: SomeException) -> do
      savepointRollback sp
      throwM e

    Right x -> do
      savepointRelease sp
      pure x

-- TODO: backlog-head-history
--   можно сделать таблицу history, в которую
--   писать журнал всех изменений голов.
--   тогда можно будет откатиться на любое предыдущее
--   состояние репозитория


statePutLogRefVal :: MonadIO m => (HashRef, GitRef, GitHash) -> DB m ()
statePutLogRefVal row = do
  conn <- ask
  liftIO $ execute conn [qc|
  insert into logrefval (loghash,refname,refval) values(?,?,?)
  on conflict (loghash,refname) do nothing
  |] row


statePutLogObject :: MonadIO m => (HashRef, GitObjectType, GitHash) -> DB m ()
statePutLogObject row = do
  conn <- ask
  liftIO $ execute conn [qc|
  insert into logobject (loghash,type,githash) values(?,?,?)
  on conflict (loghash,githash) do nothing
  |] row

stateIsLogObjectExists :: MonadIO m => GitHash -> DB m Bool
stateIsLogObjectExists h = do
  conn <- ask
  liftIO $ query conn [qc|
  SELECT NULL FROM logobject WHERE githash = ? LIMIT 1
  |] (Only h) <&> isJust . listToMaybe . fmap (fromOnly @(Maybe Int))

statePutLogContextCommit :: MonadIO m => HashRef -> GitHash -> DB m ()
statePutLogContextCommit loghash ctx = do
  conn <- ask
  liftIO $ execute conn [qc|
  insert into logobject (loghash,type,githash) values(?,'context',?)
  on conflict (loghash,githash) do nothing
  |] (loghash,ctx)

statePutLogCommitParent :: MonadIO m => (GitHash, GitHash) -> DB m ()
statePutLogCommitParent row = do
  conn <- ask
  liftIO $ execute conn [qc|
  insert into logcommitparent (kommit,parent) values(?,?)
  on conflict (kommit,parent) do nothing
  |] row


statePutLogImported :: MonadIO m => HashRef -> DB m ()
statePutLogImported h = do
  conn <- ask
  liftIO $ execute conn [qc|
  insert into logimported (hash) values(?)
  on conflict (hash) do nothing
  |] (Only h)


stateGetLogImported :: MonadIO m => HashRef -> DB m Bool
stateGetLogImported h = do
  conn <- ask
  r <- liftIO $ query @_ @(Only Int) conn [qc|
    select 1 from logimported where hash = ? limit 1
  |] (Only h)
  pure $ not $ null r


statePutRefImported :: MonadIO m => HashRef -> DB m ()
statePutRefImported h = do
  conn <- ask
  liftIO $ execute conn [qc|
  insert into refimported (hash) values(?)
  on conflict (hash) do nothing
  |] (Only h)

stateGetRefImported :: MonadIO m => HashRef -> DB m Bool
stateGetRefImported h = do
  conn <- ask
  r <- liftIO $ query @_ @(Only Int) conn [qc|
    select 1 from refimported where hash = ? limit 1
  |] (Only h)
  pure $ not $ null r

statePutTranImported :: MonadIO m => HashRef -> DB m ()
statePutTranImported h = do
  conn <- ask
  liftIO $ execute conn [qc|
  insert into tranimported (hash) values(?)
  on conflict (hash) do nothing
  |] (Only h)

stateGetTranImported :: MonadIO m => HashRef -> DB m Bool
stateGetTranImported h = do
  conn <- ask
  r <- liftIO $ query @_ @(Only Int) conn [qc|
    select 1 from tranimported where hash = ? limit 1
  |] (Only h)
  pure $ not $ null r

stateGetAllTranImported :: MonadIO m => DB m [HashRef]
stateGetAllTranImported = do
  conn <- ask
  results <- liftIO $ query_ conn [qc|
    select hash from tranimported
  |]
  pure $ map fromOnly results

stateGetImportedCommits :: MonadIO m => DB m [GitHash]
stateGetImportedCommits = do
  conn <- ask
  liftIO $ query_ conn [qc|
    select distinct(githash) from logobject where type = 'commit'
  |] <&> fmap fromOnly

stateGetActualRefs :: MonadIO m => DB m [(GitRef, GitHash)]
stateGetActualRefs = do
  conn <- ask
  liftIO $ query_ conn [qc|
    select refname,refval from v_refval_actual
  |]

stateGetActualRefValue :: MonadIO m => GitRef -> DB m (Maybe GitHash)
stateGetActualRefValue ref = do
  conn <- ask
  liftIO $ query conn [qc|
    select refval from v_refval_actual
    where refname = ?
  |] (Only ref) <&> fmap fromOnly . listToMaybe

stateGetLastKnownCommits :: MonadIO m => Int -> DB m [GitHash]
stateGetLastKnownCommits n  = do
  conn <- ask
  liftIO $ query conn [qc|
    select kommit from logcommitdepth order by depth asc limit ?;
  |] (Only n) <&> fmap fromOnly

stateUpdateCommitDepths :: MonadIO m => DB m ()
stateUpdateCommitDepths = do
  conn <- ask
  sp <- savepointNew

  rows <- liftIO $ query_ @(GitHash, GitHash) conn [qc|SELECT kommit, parent FROM logcommitparent|]

  -- TODO: check-it-works-on-huge-graphs
  let commitEdges = rows
  let (graph, nodeFromVertex, _) = graphFromEdges [(commit, commit, [parent]) | (commit, parent) <- commitEdges]
  let sortedVertices = topSort graph
  let sortedCommits = reverse [commit | vertex <- sortedVertices, let (commit, _, _) = nodeFromVertex vertex]
  let ordered = zip sortedCommits [1..]

  savepointBegin sp
  liftIO $ execute_ conn [qc|DELETE FROM logcommitdepth|]
  forM_ ordered $ \(co, n) -> do
    liftIO $ execute conn
      [qc| INSERT INTO logcommitdepth(kommit,depth)
           VALUES(?,?)
           ON CONFLICT(kommit)
           DO UPDATE SET depth = ?
      |] (co,n,n)
    pure ()
  savepointRelease sp

