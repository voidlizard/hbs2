{-# Language AllowAmbiguousTypes #-}
module DBPipe.SQLite
  ( module Database.SQLite.Simple
  , ToField(..)
  , FromField(..)
  , ToRow(..)
  , FromRow(..)
  , DBPipeEnv
  , DBPipeOpts(..)
  , dbPipeOptsDef
  , runPipe
  , newDBPipeEnv
  , DBPipeM
  , select, select_
  , update, update_
  , insert, insert_
  , ddl
  , transactional
  , transactional_
  , commitAll
  , withDB
  , shutdown
  ) where

import Control.Concurrent
import Control.Concurrent.STM (flushTQueue)
import Control.Monad
import Control.Monad.Reader
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.FromField
import Data.Fixed
import System.Clock
import Text.InterpolatedString.Perl6 (qc)
import System.IO (hPrint)
import Data.Kind()
import Data.String
import UnliftIO

data DBPipeOpts =
  DBPipeOpts
  { dbPipeBatchTime :: Fixed E2
  , dbLogger        :: String -> IO ()
  }

data DBPipeEnv =
  DBPipeEnv
  { opts         :: DBPipeOpts
  , connPath     :: FilePath
  , connection   :: TVar (Maybe Connection)
  , transNum     :: TVar Int
  , updates      :: TQueue (IO ())
  , updatesCount :: TVar Int
  , updatedLast  :: TVar (Maybe TimeSpec)
  }

newtype DBPipeM m a = DBPipeM { fromDBPipeM :: ReaderT DBPipeEnv m a }
                      deriving newtype ( Applicative
                                       , Functor
                                       , Monad
                                       , MonadReader DBPipeEnv
                                       , MonadIO
                                       , MonadUnliftIO
                                       , MonadTrans
                                       )

dbPipeOptsDef :: DBPipeOpts
dbPipeOptsDef = DBPipeOpts 1 (liftIO . hPrint stderr)

newDBPipeEnv :: MonadIO m => DBPipeOpts -> FilePath -> m DBPipeEnv
newDBPipeEnv opts fp = liftIO $ do
  DBPipeEnv opts fp <$> newTVarIO Nothing
                    <*> newTVarIO 0
                    <*> newTQueueIO
                    <*> newTVarIO 0
                    <*> newTVarIO Nothing

withDB :: forall a m . MonadIO m => DBPipeEnv -> DBPipeM m a -> m a
withDB env action = runReaderT (fromDBPipeM action) env

runPipe :: forall m . MonadIO m => DBPipeEnv -> m ()
runPipe env@(DBPipeEnv{..}) = do
  forever $ do
    liftIO $ threadDelay (round (dbPipeBatchTime opts * 1_000_000))
    _ <- atomically $ peekTQueue updates
    withDB env commitAll


shutdown :: forall m . MonadIO m => Bool -> DBPipeEnv -> m ()
shutdown doCommit env = do
  when doCommit $ withDB env commitAll
  mco <- readTVarIO (connection env)
  atomically $ writeTVar (connection env) Nothing
  maybe (pure ()) (liftIO . close) mco

transactional :: forall a m . (MonadUnliftIO m) => DBPipeM m a -> DBPipeM m ()
transactional what = do
  conn <- withConn pure
  env <- ask
  transactional_ env conn what

transactional_ :: forall a m . MonadUnliftIO m => DBPipeEnv -> Connection -> m a -> m ()
transactional_ DBPipeEnv{..} conn action = do
  tnum <- liftIO $ atomically $ stateTVar transNum $ \s -> (s, succ s)
  let sp = [qc|sp{tnum}|] :: String

  liftIO $ execute_ conn [qc|SAVEPOINT {sp}|]

  try action >>= \case

    Right{} -> do
      liftIO $ execute_ conn [qc|RELEASE SAVEPOINT {sp}|]

    Left ( e :: SomeException ) -> liftIO do
      dbLogger opts (show e)
      execute_ conn [qc|ROLLBACK TO SAVEPOINT {sp}|]
      throwIO e

class ToQuery a b where
  toSQL :: a -> String

withConn :: forall a m . MonadIO m => (Connection -> IO a) -> DBPipeM m a
withConn action = do
  DBPipeEnv{..} <- ask
  conn <- readTVarIO connection >>= \case
      Just conn -> pure conn
      Nothing -> do
        conn <- liftIO $ open connPath
        atomically (writeTVar connection (Just conn))
        pure conn
  liftIO $ action conn


commitAll :: MonadIO m => DBPipeM m ()
commitAll = do
  env@(DBPipeEnv{..}) <- ask
  ops <- atomically $ flushTQueue updates
  withDB env $ withConn $ \conn -> do
    transactional_ env conn $ sequence_ ops

select :: forall b args a m . (ToQuery a b, FromRow b, ToRow args, MonadIO m) => a -> args -> DBPipeM m [b]
select q wtf = withConn $ \conn -> do
  liftIO $ query conn (fromString (toSQL @a @b q)) wtf

select_ :: (ToQuery a b, FromRow b, MonadIO m) => a -> DBPipeM m [b]
select_ a = select a ()

update_ :: forall a m . (ToQuery a (), MonadIO m) => a -> DBPipeM m ()
update_ a = update @a @() @() a ()

insert_ :: forall a m . (ToQuery a (), MonadIO m) => a -> DBPipeM m ()
insert_ a = insert @a @() @() a ()

update :: forall a args b m . (ToQuery a b, ToRow args, MonadIO m) => a -> args -> DBPipeM m ()
update q args = withConn $ \conn -> do
  execute conn  (fromString (toSQL @a @b q)) args

insert :: forall a args b m . (ToQuery a b, ToRow args, MonadIO m) => a -> args -> DBPipeM m ()
insert = update @a @_ @b

ddl :: forall a m . (ToQuery a (), MonadIO m) => a -> DBPipeM m ()
ddl a = update @a @() @() a ()

instance ToQuery String r where
  toSQL a = a

test1 :: IO ()
test1 = do
  env <- newDBPipeEnv dbPipeOptsDef ":memory:"

  a <- async $ runPipe env

  withDB env do
    ddl "create table wtf (k int primary key, v int)"
    commitAll

  withDB env $ do

    transactional do
      update "insert into wtf (k,v) values(1,1)" ()

    commitAll

    wtf <- select @(Int,Int) "select k,v from wtf" ()
    liftIO $ print wtf

  cancel a


