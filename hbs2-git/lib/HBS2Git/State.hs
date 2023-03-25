module HBS2Git.State where

import HBS2Git.Types
import HBS2.Data.Types.Refs
import HBS2.Git.Types

import Data.Functor
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import Control.Monad.IO.Class
import Control.Monad.Reader
import Text.InterpolatedString.Perl6 (qc)
import Data.String
import System.Directory
import System.FilePath
import Data.Maybe
import Data.Text (Text)
import Prettyprinter

instance ToField GitHash where
  toField h = toField (show $ pretty h)

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
                   )

instance (HasRefCredentials m) => HasRefCredentials (DB m) where
  getCredentials = lift . getCredentials

dbEnv :: MonadIO m => FilePath -> m DBEnv
dbEnv fp = do
  let dir = takeDirectory fp
  liftIO $ createDirectoryIfMissing True dir
  co <- liftIO $ open fp
  withDB co stateInit
  pure co

withDB :: DBEnv -> DB m a -> m a
withDB env action = runReaderT (fromDB action) env

stateInit :: MonadIO m => DB m ()
stateInit = do
  conn <- ask
  liftIO $ execute_ conn [qc|
  create table if not exists dep
  ( object text not null
  , parent text not null
  , primary key (object, parent)
  )
  |]

  liftIO $ execute_ conn [qc|
  create table if not exists object
  ( githash text not null
  , hash text not null unique
  , type text not null
  , primary key (githash,hash)
  )
  |]

  liftIO $ execute_ conn [qc|
  create table if not exists head
  ( key text not null primary key
  , hash text not null unique
  )
  |]

  liftIO $ execute_ conn [qc|
  create table if not exists imported
  ( seq integer primary key autoincrement
  , ts DATE DEFAULT (datetime('now','localtime'))
  , merkle text not null
  , head text not null
  , unique (merkle,head)
  )
  |]

  liftIO $ execute_ conn [qc|
  create table if not exists reflog
  ( seq integer primary key
  , ts DATE DEFAULT (datetime('now','localtime'))
  , merkle text not null
  , unique (merkle)
  )
  |]


transactional :: forall a m . MonadIO m => DB m a -> DB m a
transactional action = do
  conn <- ask
  liftIO $ execute_ conn "begin"
  x <- action
  liftIO $ execute_ conn "commit"
  pure x

-- TODO: backlog-head-history
--   можно сделать таблицу history, в которую
--   писать журнал всех изменений голов.
--   тогда можно будет откатиться на любое предыдущее
--   состояние репозитория

statePutImported :: MonadIO m => HashRef -> HashRef -> DB m ()
statePutImported merkle hd = do
  conn <- ask
  liftIO $ execute conn [qc|
  insert into imported (merkle,head) values(?,?)
  on conflict (merkle,head) do nothing
  |] (merkle,hd)

stateUpdateRefLog :: MonadIO m => Integer -> HashRef -> DB m ()
stateUpdateRefLog seqno merkle = do
  conn <- ask
  liftIO $ execute conn [qc|
  insert into reflog (seq,merkle) values(?,?)
  on conflict (merkle) do nothing
  on conflict (seq) do nothing
  |] (seqno,merkle)

stateGetRefLogLast :: MonadIO m => DB m (Maybe (Integer, HashRef))
stateGetRefLogLast = do
  conn <- ask
  liftIO $ query_ conn [qc|
  select seq, merkle from reflog
  order by seq desc
  limit 1
  |] <&> listToMaybe

statePutHead :: MonadIO m => HashRef -> DB m ()
statePutHead h = do
  conn <- ask
  liftIO $ execute conn [qc|
  insert into head (key,hash) values('head',?)
  on conflict (key) do update set hash = ?
  |] (h,h)

stateGetHead :: MonadIO m => DB m (Maybe HashRef)
stateGetHead = do
  conn <- ask
  liftIO $ query_ conn [qc|
  select hash from head where key = 'head'
  limit 1
  |] <&> listToMaybe . fmap fromOnly

stateAddDep :: MonadIO m => GitHash -> GitHash -> DB m ()
stateAddDep h1 h2 = do
  conn <- ask
  void $ liftIO $ execute conn [qc|
    insert into dep (object,parent) values(?,?)
    on conflict (object,parent) do nothing
    |] (h1,h2)

stateGetDeps :: MonadIO m => GitHash -> DB m [GitHash]
stateGetDeps h = do
  conn <- ask
  liftIO $ query conn [qc|
  select parent from dep where object = ?
  |] (Only h) <&> fmap fromOnly


statePutHash :: MonadIO m => GitObjectType -> GitHash -> HashRef -> DB m ()
statePutHash t g h = do
  conn <- ask
  liftIO $ execute conn [qc|
  insert into object (githash,hash,type) values(?,?,?)
  on conflict (githash,hash) do nothing
  |] (g,h,t)

stateGetHash :: MonadIO m => GitHash -> DB m (Maybe HashRef)
stateGetHash h = do
  conn <- ask
  liftIO $ query conn [qc|
  select hash from object where githash = ?
  limit 1
  |] (Only h) <&> fmap fromOnly <&> listToMaybe


stateGetGitHash :: MonadIO m => HashRef -> DB m (Maybe GitHash)
stateGetGitHash h = do
  conn <- ask
  liftIO $ query conn [qc|
  select githash from object where hash = ?
  limit 1
  |] (Only h) <&> fmap fromOnly <&> listToMaybe

stateGetAllHashes :: MonadIO m => DB m [HashRef]
stateGetAllHashes = do
  conn <- ask
  liftIO $ query_ conn [qc|
  select distinct(hash) from object
  |] <&> fmap fromOnly

stateGetAllObjects:: MonadIO m => DB m [(HashRef,GitHash,GitObjectType)]
stateGetAllObjects = do
  conn <- ask
  liftIO $ query_ conn [qc|
  select hash, githash, type from object
  |]

stateGetLastImported :: MonadIO m => Int -> DB m [(Text,HashRef,HashRef)]
stateGetLastImported n = do
  conn <- ask
  liftIO $ query conn [qc|
  select ts, merkle, head from imported
  order by seq desc
  limit  ?
  |] (Only n)

stateGetSequence :: MonadIO m => DB m Integer
stateGetSequence = do
  conn <- ask
  liftIO $ query_ conn [qc|
    select coalesce(max(seq),0) from reflog;
  |] <&> fmap fromOnly
     <&> listToMaybe
     <&> fromMaybe 0



