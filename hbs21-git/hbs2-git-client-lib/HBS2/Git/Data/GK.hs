module HBS2.Git.Data.GK where

import HBS2.Git.Client.Prelude

import HBS2.Net.Auth.GroupKeySymm
import HBS2.Storage.Operations.ByteString

import Data.ByteString.Lazy qualified as LBS

type GK0 = GroupKey 'Symm HBS2Basic

readGK0 :: (MonadIO m, MonadError OperationError m) => AnyStorage -> HashRef -> m GK0
readGK0 sto h = do
  runExceptT (readFromMerkle sto (SimpleKey (fromHashRef h)))
   >>= orThrowError MissedBlockError
   <&> deserialiseOrFail @GK0
   >>= orThrowError UnsupportedFormat

loadGK0FromFile :: MonadIO m => FilePath -> m (Maybe GK0)
loadGK0FromFile fp = runMaybeT do

  content <- liftIO (try @_ @IOError (LBS.readFile fp))
               >>= toMPlus

  toMPlus $ parseGroupKey @HBS2Basic (AsGroupKeyFile  content)

