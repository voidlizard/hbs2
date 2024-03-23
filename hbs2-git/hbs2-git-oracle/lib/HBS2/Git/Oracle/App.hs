module HBS2.Git.Oracle.App where

import HBS2.Git.Oracle.Prelude

import Control.Monad.Reader

data OracleEnv =
  OracleEnv
  {
  }
  deriving stock (Generic)

newtype Oracle m a =
  Oracle { fromOracle :: ReaderT OracleEnv m a }
  deriving newtype ( Applicative
                   , Functor
                   , Monad
                   , MonadTrans
                   , MonadReader OracleEnv
                   , MonadIO
                   , MonadUnliftIO
                   )

newOracleEnv :: MonadIO m => m OracleEnv
newOracleEnv = pure OracleEnv

withOracleEnv ::  MonadIO m => OracleEnv -> Oracle m a -> m a
withOracleEnv env m = runReaderT (fromOracle m) env



