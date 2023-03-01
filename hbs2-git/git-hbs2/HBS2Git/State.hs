module HBS2Git.State where

import HBS2Git.Types

import Database.SQLite.Simple
import Control.Monad.IO.Class
import Control.Monad.Reader
import Text.InterpolatedString.Perl6 (qc)

type DBEnv = Connection

newtype DB m a =
  DB { fromDB :: ReaderT DBEnv m a }
  deriving newtype ( Applicative
                   , Functor
                   , Monad
                   , MonadIO
                   , MonadReader Connection
                   )


dbEnv :: MonadIO m => FilePath -> m DBEnv
dbEnv fp = liftIO $ open fp

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

-- stateInit :: MonadIO m => FilePath -> App m ()
-- stateInit fp = do
--   undefined

