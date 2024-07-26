module HBS2.CLI.Run.Internal.GroupKey where

import HBS2.CLI.Prelude hiding (mapMaybe)

import HBS2.Data.Types.Refs
import HBS2.Storage.Operations.Class
import HBS2.Storage.Operations.ByteString
import Data.Maybe
import HBS2.CLI.Run.Internal
import HBS2.Net.Auth.GroupKeySymm as Symm

import Control.Monad.Trans.Cont
import Control.Monad.Except
import Codec.Serialise

groupKeyFromKeyList :: MonadUnliftIO m => [String] -> m (GroupKey 'Symm HBS2Basic)
groupKeyFromKeyList ks = do
  let members = mapMaybe (fromStringMay @(PubKey 'Encrypt 'HBS2Basic)) ks
  Symm.generateGroupKey @'HBS2Basic Nothing members


loadGroupKey :: (IsContext c, MonadUnliftIO m) => HashRef -> RunM c m (Maybe (GroupKey 'Symm HBS2Basic))
loadGroupKey h = do

  flip runContT pure do
    sto <- ContT withPeerStorage

    raw <- runExceptT (readFromMerkle sto (SimpleKey (fromHashRef h)))
            <&> either (const Nothing) Just

    bs <- ContT (maybe1 raw (pure Nothing))

    let gk = deserialiseOrFail bs
              & either (const Nothing) Just

    pure gk
