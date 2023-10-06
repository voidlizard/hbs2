{-# OPTIONS_GHC -fno-warn-orphans #-}
module HBS2Git.State where

import HBS2.Prelude
import HBS2Git.Types
import HBS2.Data.Types.Refs
import HBS2.Git.Types
import HBS2.Hash

import HBS2.System.Logger.Simple

import HBS2Git.Config (cookieFile)

import Control.Monad.Trans.Resource
import Data.Functor
import Data.Function
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import Control.Monad.Reader
import Text.InterpolatedString.Perl6 (qc)
import Data.ByteString.Lazy.Char8 qualified as LBS
import System.Directory
import System.FilePath
import Data.Maybe
import Data.UUID.V4 qualified as UUID
import Control.Monad.Catch
import Control.Concurrent.STM
import Data.Graph (graphFromEdges, topSort)
import Lens.Micro.Platform

-- FIXME: move-orphans-to-separate-module

instance ToField Cookie where
  toField (Cookie lbs) = toField lbs

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
                   , MonadReader DBEnv
                   , MonadTrans
                   , MonadThrow
                   , MonadCatch
                   )

instance (HasRefCredentials m) => HasRefCredentials (DB m) where
  getCredentials = lift . getCredentials
  setCredentials r s  = lift (setCredentials r s)

stateConnection :: MonadIO m => DB m Connection
stateConnection = do
  env <- ask
  initConnection env

initConnection :: MonadIO m => DBEnv -> m Connection
initConnection env = do
  mco <- liftIO $ readTVarIO (view dbConn env)
  case mco of
    Just co -> pure co
    Nothing -> do
      co <- liftIO $ open (view dbFilePath env)
      liftIO $ atomically $ writeTVar (view dbConn env) (Just co)
      pure co

dbEnv0 :: (MonadIO m, MonadMask m) => DB m () -> FilePath -> m DBEnv
dbEnv0 dbInit fp = do
  trace  "dbEnv called"
  let dir = takeDirectory fp
  liftIO $ createDirectoryIfMissing True dir
  env0 <- DBEnv fp "" <$> liftIO (newTVarIO Nothing)
  void $ withDB env0 dbInit
  cookie <- withDB env0 $ readOrCreateCookie
  DBEnv fp cookie <$> liftIO (newTVarIO Nothing)

dbEnv :: (MonadIO m, MonadMask m) => FilePath -> m DBEnv
dbEnv = dbEnv0 stateInit

dbEnvReadOnly :: (MonadIO m, MonadMask m) => FilePath -> m DBEnv
dbEnvReadOnly = dbEnv0 none

withDB :: (MonadIO m, MonadMask m) => DBEnv -> DB m a -> m a
withDB env action = do
  trace $ "** DB run with COOKIE" <+> viaShow (view dbCookie env)
  conn <- initConnection env
  finally (runReaderT (fromDB action) env) $ do
    -- NOTE: we could not close connection here.
    pure ()

shutdownDB :: MonadIO m => DBEnv -> m ()
shutdownDB env = liftIO do
  co <- atomically do
    conn <- readTVar (view dbConn env)
    writeTVar (view dbConn env) Nothing
    pure conn
  maybe1 co none close

stateInit :: MonadIO m => DB m ()
stateInit = do
  conn <- stateConnection
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

  forM_ ["logimported", "tranimported", "refimported"] $ \t -> do
    here <- colExists conn t "cookie"
    unless here $ liftIO do
      liftIO $ execute_ conn [qc|
      DROP TABLE IF EXISTS {t};
      |]

  liftIO $ execute_ conn [qc|
  create table if not exists logimported
  ( hash text not null
  , cookie text not null
  , primary key (hash, cookie)
  )
  |]

  liftIO $ execute_ conn [qc|
  create table if not exists refimported
  ( hash text not null
  , cookie text not null
  , timestamp DATETIME DEFAULT CURRENT_TIMESTAMP
  , primary key (hash, cookie)
  )
  |]

  liftIO $ execute_ conn [qc|
  create table if not exists tranimported
  ( hash text not null
  , cookie text not null
  , timestamp DATETIME DEFAULT CURRENT_TIMESTAMP
  , primary key (hash, cookie)
  )
  |]

  liftIO $ execute_ conn [qc|
  DROP VIEW IF EXISTS v_refval_actual;
  |]

  liftIO $ execute_ conn [qc|
  CREATE TABLE IF NOT EXISTS logcommitdepth
  ( kommit text not null
  , depth integer not null
  , primary key (kommit)
  );
  |]

  liftIO $ execute_ conn [qc|
  CREATE TABLE IF NOT EXISTS logrank
  ( hash text not null
  , rank int not null
  , primary key (hash)
  );
  |]

  liftIO $ execute_ conn [qc|
  CREATE TABLE IF NOT EXISTS cookie
  ( cookie text not null
  , primary key (cookie)
  );
  |]

  liftIO $ execute_ conn [qc|
  DROP VIEW IF EXISTS v_log_depth;
  |]

  liftIO $ execute_ conn [qc|
  DROP VIEW IF EXISTS v_refval_actual;
  |]

  liftIO $ execute_ conn [qc|
  CREATE VIEW v_refval_actual AS
  WITH ranks AS (
      SELECT  rv.refname,
              MAX(COALESCE(d.depth, 0)) as max_depth,
              MAX(COALESCE(r.rank, 0)) as max_rank
      FROM logrefval rv
      LEFT JOIN logcommitdepth d ON rv.refval = d.kommit
      LEFT JOIN logrank r ON r.hash = rv.loghash
      GROUP BY rv.refname
  )
  SELECT r.refname, rv.refval, r.max_rank as r, r.max_depth as d
  FROM logrefval rv
  JOIN ranks r ON r.refname = rv.refname
  WHERE
      (
          (r.max_rank > 0 AND rv.loghash IN (SELECT hash FROM logrank WHERE rank = r.max_rank))
       OR (r.max_rank = 0 AND rv.refval IN (SELECT kommit FROM logcommitdepth WHERE depth = r.max_depth))
      )
  AND rv.refval <> '0000000000000000000000000000000000000000'
  ORDER BY r.refname;
  |]

  void $ readOrCreateCookie

  where
    colExists :: MonadIO m => Connection -> String -> String -> m Bool
    colExists conn table col = do
      let sql =[qc|PRAGMA table_info({table})|]
      fields <- liftIO $ query_ conn sql
      let fs = [x | ((_, x, _, _, _, _) :: (Int, String, String, Int, Maybe String, Int)) <- fields ]
      pure ( col `elem` fs )

readOrCreateCookie :: MonadIO m => DB m Cookie
readOrCreateCookie = do
  cfn <- cookieFile
  cf <- liftIO $ readFile cfn <&> take 4096

  if null cf then do
    cookie <- stateGenCookie
    liftIO $ LBS.writeFile cfn (fromCookie cookie)
    pure cookie
  else do
    let cookie = Cookie (fromString cf)
    statePutCookie cookie
    pure cookie

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
  conn <- stateConnection
  liftIO $ execute_ conn [qc|SAVEPOINT {sp}|]

savepointRelease:: forall m . MonadIO m => Savepoint -> DB m ()
savepointRelease (Savepoint sp) = do
  conn <- stateConnection
  liftIO $ execute_ conn [qc|RELEASE SAVEPOINT {sp}|]

savepointRollback :: forall m . MonadIO m => Savepoint -> DB m ()
savepointRollback (Savepoint sp) = do
  conn <- stateConnection
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
  conn <- stateConnection
  liftIO $ execute conn [qc|
  insert into logrefval (loghash,refname,refval) values(?,?,?)
  on conflict (loghash,refname) do nothing
  |] row


statePutLogObject :: MonadIO m => (HashRef, GitObjectType, GitHash) -> DB m ()
statePutLogObject row = do
  conn <- stateConnection
  liftIO $ execute conn [qc|
  insert into logobject (loghash,type,githash) values(?,?,?)
  on conflict (loghash,githash) do nothing
  |] row

stateIsLogObjectExists :: MonadIO m => GitHash -> DB m Bool
stateIsLogObjectExists h = do
  conn <- stateConnection
  liftIO $ query conn [qc|
  SELECT NULL FROM logobject WHERE githash = ? LIMIT 1
  |] (Only h) <&> isJust . listToMaybe . fmap (fromOnly @(Maybe Int))


stateGetGitLogObject :: MonadIO m => GitHash -> DB m (Maybe HashRef)
stateGetGitLogObject h = do
  conn <- stateConnection
  liftIO $ query conn [qc|
  SELECT loghash FROM logobject
  WHERE githash = ? and type in ('commit', 'tree', 'blob')
  LIMIT 1
  |] (Only h) <&> listToMaybe . fmap fromOnly

statePutLogContextCommit :: MonadIO m => HashRef -> GitHash -> DB m ()
statePutLogContextCommit loghash ctx = do
  conn <- stateConnection
  liftIO $ execute conn [qc|
  insert into logobject (loghash,type,githash) values(?,'context',?)
  on conflict (loghash,githash) do nothing
  |] (loghash,ctx)


statePutLogContextRank :: MonadIO m => HashRef -> Int -> DB m ()
statePutLogContextRank loghash rank = do
  conn <- stateConnection
  liftIO $ execute conn [qc|
  insert into logrank (hash,rank) values(?,?)
  on conflict (hash) do nothing
  |] (loghash,rank)

statePutLogCommitParent :: MonadIO m => (GitHash, GitHash) -> DB m ()
statePutLogCommitParent row = do
  conn <- stateConnection
  liftIO $ execute conn [qc|
  insert into logcommitparent (kommit,parent) values(?,?)
  on conflict (kommit,parent) do nothing
  |] row


statePutLogImported :: MonadIO m => HashRef -> DB m ()
statePutLogImported h = do
  conn <- stateConnection
  cookie <- asks (view dbCookie)
  liftIO $ execute conn [qc|
  insert into logimported (hash,cookie) values(?,?)
  on conflict (hash,cookie) do nothing
  |] (h,cookie)

stateGetLogImported :: MonadIO m => HashRef -> DB m Bool
stateGetLogImported h = do
  conn <- stateConnection
  cookie <- asks (view dbCookie)
  r <- liftIO $ query @_ @(Only Int) conn [qc|
    select 1 from logimported where hash = ? and cookie = ? limit 1
  |] (h, cookie)
  pure $ not $ null r

statePutRefImported :: MonadIO m => HashRef -> DB m ()
statePutRefImported h = do
  conn <- stateConnection
  cookie <- asks (view dbCookie)
  liftIO $ execute conn [qc|
  insert into refimported (hash,cookie) values(?,?)
  on conflict (hash,cookie) do nothing
  |] (h,cookie)

stateGetRefImported :: MonadIO m => HashRef -> DB m Bool
stateGetRefImported h = do
  conn <- stateConnection
  cookie <- asks (view dbCookie)
  r <- liftIO $ query @_ @(Only Int) conn [qc|
    select 1 from refimported where hash = ? and cookie = ? limit 1
  |] (h, cookie)
  pure $ not $ null r

statePutTranImported :: MonadIO m => HashRef -> DB m ()
statePutTranImported h = do
  conn <- stateConnection
  cookie <- asks (view dbCookie)
  liftIO $ execute conn [qc|
  insert into tranimported (hash, cookie) values(?, ?)
  on conflict (hash, cookie) do nothing
  |] (h, cookie)

stateGetTranImported :: MonadIO m => HashRef -> DB m Bool
stateGetTranImported h = do
  conn <- stateConnection
  cookie <- asks (view dbCookie)
  r <- liftIO $ query @_ @(Only Int) conn [qc|
    select 1 from tranimported where hash = ? and cookie = ? limit 1
  |] (h, cookie)
  pure $ not $ null r

stateGetAllTranImported :: MonadIO m => DB m [HashRef]
stateGetAllTranImported = do
  conn <- stateConnection
  cookie <- asks (view dbCookie)
  results <- liftIO $ query conn [qc|
    select hash from tranimported where cookie = ?
  |] (Only cookie)
  pure $ map fromOnly results

stateGetImportedCommits :: MonadIO m => DB m [GitHash]
stateGetImportedCommits = do
  conn <- stateConnection
  liftIO $ query_ conn [qc|
    select distinct(githash) from logobject where type = 'commit'
  |] <&> fmap fromOnly

stateGetActualRefs :: MonadIO m => DB m [(GitRef, GitHash)]
stateGetActualRefs = do
  conn <- stateConnection
  liftIO $ query_ conn [qc|
    select refname,refval from v_refval_actual
  |]

stateGetActualRefValue :: MonadIO m => GitRef -> DB m (Maybe GitHash)
stateGetActualRefValue ref = do
  conn <- stateConnection
  liftIO $ query conn [qc|
    select refval from v_refval_actual
    where refname = ?
  |] (Only ref) <&> fmap fromOnly . listToMaybe

stateGetLastKnownCommits :: MonadIO m => Int -> DB m [GitHash]
stateGetLastKnownCommits n  = do
  conn <- stateConnection
  liftIO $ query conn [qc|
    select kommit from logcommitdepth order by depth asc limit ?;
  |] (Only n) <&> fmap fromOnly

stateUpdateCommitDepths :: MonadIO m => DB m ()
stateUpdateCommitDepths = do
  conn <- stateConnection
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


statePutCookie :: MonadIO m => Cookie -> DB m ()
statePutCookie cookie = do
  conn <- stateConnection
  let sql = [qc|INSERT INTO cookie (cookie) values(?) ON CONFLICT(cookie) DO NOTHING|]
  liftIO $ execute conn sql (Only cookie)

stateGenCookie :: (MonadIO m) => DB m Cookie
stateGenCookie = do
  conn <- stateConnection
  fix \next -> do
    cookie <- liftIO (UUID.nextRandom <&> (fromString @Cookie. show))

    here <- liftIO $ query conn [qc|select 1 from cookie where cookie = ? limit 1|] (Only cookie)
                        <&> listToMaybe @(Only Int)

    if isJust here then do
      next
    else liftIO do
      void $ execute conn [qc|insert into cookie (cookie) values(?)|] (Only cookie)
      pure cookie


