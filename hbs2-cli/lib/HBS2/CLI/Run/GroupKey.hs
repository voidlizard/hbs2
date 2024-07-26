module HBS2.CLI.Run.GroupKey where


import HBS2.CLI.Prelude hiding (mapMaybe)
import HBS2.System.Logger.Simple.ANSI as All
import HBS2.Base58
import Data.List qualified as L
import Data.Maybe
import HBS2.CLI.Run.Internal
import HBS2.Net.Auth.GroupKeySymm as Symm

import HBS2.Net.Auth.Credentials

import Data.Text qualified as Text
import Data.ByteString.Lazy.Char8 as LBS8
import Data.HashMap.Strict qualified as HM
import Lens.Micro.Platform

{- HLINT ignore "Functor law" -}

groupKeyFromKeyList :: MonadUnliftIO m => [String] -> m (GroupKey 'Symm HBS2Basic)
groupKeyFromKeyList ks = do
  let members = mapMaybe (fromStringMay @(PubKey 'Encrypt 'HBS2Basic)) ks
  Symm.generateGroupKey @'HBS2Basic Nothing members

groupKeyEntries :: forall c m . (MonadUnliftIO m, IsContext c) => MakeDictM c m ()
groupKeyEntries = do

  entry $ bindMatch "hbs2:groupkey:store" $ \case
      [LitStrVal s] -> do
        error "FUCK"

      _ -> throwIO $ BadFormException @C nil

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


  entry $ bindMatch "hbs2:groupkey:list-public-keys" $ \syn -> do
    case syn of
      [LitStrVal s] -> do

        let lbs = LBS8.pack (Text.unpack s)
        gk <- pure (Symm.parseGroupKey @'HBS2Basic $ AsGroupKeyFile lbs)
                `orDie` "invalid group key"

        let rcpt = recipients gk & HM.keys & fmap (mkStr . show . pretty . AsBase58)

        pure $ mkList @c rcpt

      _ -> throwIO $ BadFormException @C nil



