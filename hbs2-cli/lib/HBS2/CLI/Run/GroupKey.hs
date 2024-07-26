module HBS2.CLI.Run.GroupKey where

import HBS2.CLI.Prelude hiding (mapMaybe)

import HBS2.Data.Types.Refs
import HBS2.System.Logger.Simple.ANSI as All
import HBS2.Storage.Operations.Class
import HBS2.Storage.Operations.ByteString
import HBS2.Base58
import Data.List qualified as L
import Data.Maybe
import HBS2.CLI.Run.Internal
import HBS2.CLI.Run.Internal.GroupKey
import HBS2.Net.Auth.GroupKeySymm as Symm

import HBS2.Net.Auth.Credentials

import Data.Text qualified as Text
import Data.ByteString.Lazy.Char8 as LBS8
import Data.HashMap.Strict qualified as HM
import Control.Monad.Trans.Cont
import Control.Monad.Except
import Codec.Serialise
import Lens.Micro.Platform

{- HLINT ignore "Functor law" -}


groupKeyEntries :: forall c m . (MonadUnliftIO m, IsContext c) => MakeDictM c m ()
groupKeyEntries = do

  entry $ bindMatch "hbs2:groupkey:load" $ \case
      [StringLike s] -> do
        flip runContT pure do
          sto <- ContT withPeerStorage

          gk <- runExceptT (readFromMerkle sto (SimpleKey (fromString s)))
                  >>= orThrowUser "can't load group key"
                  <&> deserialiseOrFail @(GroupKey 'Symm 'HBS2Basic)
                  >>= orThrowUser "invalid group key"

          pure $ mkStr (show $ pretty $ AsGroupKeyFile gk)

      _ -> throwIO $ BadFormException @C nil


  entry $ bindMatch "hbs2:groupkey:store" $ \case
      [LitStrVal s] -> do
        flip runContT pure do

          let lbs = LBS8.pack (Text.unpack s)
          gk <- pure (Symm.parseGroupKey @'HBS2Basic $ AsGroupKeyFile lbs)
                  `orDie` "invalid group key"

          sto <- ContT withPeerStorage
          ha <- writeAsMerkle sto (serialise gk)
          pure $ mkStr (show $ pretty ha)

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



