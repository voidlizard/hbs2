module HBS2.CLI.Run.GroupKey where


import HBS2.CLI.Prelude hiding (mapMaybe)
import Data.List qualified as L
import Data.Maybe
import HBS2.CLI.Run.Internal
import HBS2.Net.Auth.GroupKeySymm as Symm

import HBS2.Net.Auth.Credentials

{- HLINT ignore "Functor law" -}

groupKeyFromKeyList :: MonadUnliftIO m => [String] -> m (GroupKey 'Symm HBS2Basic)
groupKeyFromKeyList ks = do
  let members = mapMaybe (fromStringMay @(PubKey 'Encrypt 'HBS2Basic)) ks
  Symm.generateGroupKey @'HBS2Basic Nothing members

groupKeyEntries :: forall c m . (MonadUnliftIO m, IsContext c) => MakeDictM c m ()
groupKeyEntries = do
  entry $ bindMatch "hbs2:groupkey:create" $ \syn -> do
    case syn of
      [ListVal (StringLikeList keys)] -> do
        s <- groupKeyFromKeyList  keys
                <&> AsGroupKeyFile
                <&> show . pretty

        pure $ mkStr s

      StringLikeList keys -> do
        s <- groupKeyFromKeyList  keys
                <&> AsGroupKeyFile
                <&> show . pretty

        pure $ mkStr s

      _ -> throwIO $ BadFormException @C nil

