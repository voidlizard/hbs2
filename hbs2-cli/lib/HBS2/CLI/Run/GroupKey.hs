module HBS2.CLI.Run.GroupKey
  ( module HBS2.CLI.Run.GroupKey
  , loadGroupKey
  ) where

import HBS2.CLI.Prelude hiding (mapMaybe)

import HBS2.Data.Types.Refs
import HBS2.Storage.Operations.Class
import HBS2.Storage.Operations.ByteString
import HBS2.Base58
import HBS2.CLI.Run.Internal
import HBS2.CLI.Run.Internal.GroupKey as G
import HBS2.Net.Auth.GroupKeySymm as Symm
import HBS2.Storage

import HBS2.KeyMan.Keys.Direct

import HBS2.Peer.RPC.API.Storage
import HBS2.Peer.RPC.Client
import HBS2.Peer.RPC.Client.Unix

import Data.Text qualified as Text
import Data.ByteString.Lazy.Char8 as LBS8
import Data.ByteString.Lazy as LBS
import Data.ByteString.Char8 as BS8
import Data.HashMap.Strict qualified as HM
import Control.Monad.Except
import Codec.Serialise

{- HLINT ignore "Functor law" -}


groupKeyEntries :: forall c m . ( MonadUnliftIO m
                                , IsContext c
                                , Exception (BadFormException c)
                                , HasClientAPI StorageAPI UNIX m
                                , HasStorage m
                                ) => MakeDictM c m ()
groupKeyEntries = do

  entry $ bindMatch "hbs2:groupkey:load" $ \case
      [HashLike h] -> do
        sto <- getStorage

        gk <- loadGroupKey h
                >>= orThrowUser "can not load groupkey"

        pure $ mkStr (show $ pretty $ AsGroupKeyFile gk)

      _ -> throwIO $ BadFormException @C nil


  brief "stores groupkey to the peer's storage" $
   args [arg "string" "groupkey"] $
   returns "string" "hash" $
    entry $ bindMatch "hbs2:groupkey:store" $ \case
        [LitStrVal s] -> do
          let lbs = LBS8.pack (Text.unpack s)
          gk <- pure (Symm.parseGroupKey @'HBS2Basic $ AsGroupKeyFile lbs)
                  `orDie` "invalid group key"

          sto <- getStorage
          ha <- writeAsMerkle sto (serialise gk)
          pure $ mkStr (show $ pretty ha)

        _ -> throwIO $ BadFormException @c nil


  brief "publish groupkey to the given refchan" $
    args [arg "string" "refchan", arg "string" "groupkey-blob|groupkey-hash"] $
    desc "groupkey may be also hash of te stored groupkey" $
      entry $ bindMatch "hbs2:groupkey:publish" $ nil_ $ \case

      [SignPubKeyLike rchan, LitStrVal gk] -> do
        -- get
        -- check
        -- store
        -- find refchan
        -- post tx as metadata
        notice $ red "not implemented yet"

      [SignPubKeyLike rchan, HashLike gkh] -> do
        notice $ red "not implemented yet"

      _ -> throwIO $ BadFormException @c nil


-- $ hbs2-cli print [hbs2:groupkey:update [hbs2:groupkey:load 6XJGpJszP6f68fmhF17AtJ9PTgE7BKk8RMBHWQ2rXu6N] \
--             [list [remove . CcRDzezX1XQdPxRMuMKzJkfHFB4yG7vGJeTYvScKkbP8] \
--                   [add . 5sJXsw7qhmq521hwhE67jYvrD6ZNVazc89rFwfWaQPyY]] ]
--
  entry $ bindMatch "hbs2:groupkey:update" $ \case
    [LitStrVal s, ListVal ins] -> do

      sto <- getStorage

      let lbs = LBS8.pack (Text.unpack s)
      gk <- pure (Symm.parseGroupKey @'HBS2Basic $ AsGroupKeyFile lbs)
              `orDie` "invalid group key"

      gk1 <- modifyGroupKey gk ins

      pure $ mkStr (show $ pretty $ AsGroupKeyFile gk1)

    _ -> throwIO $ BadFormException @C nil

  brief "create group key" $
    args [ arg "keys" "list" ] $
    desc "list of encryption public keys of members" $
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


  entry $ bindMatch "hbs2:groupkey:dump" $ nil_ $ \syn -> do
    case syn of
      -- TODO: from-file
      -- TODO: from-stdin
      -- TODO: base58 file
      [HashLike gkh] -> do
        gk <- loadGroupKey gkh
        liftIO $ print $ pretty gk

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

  brief "find groupkey secret in hbs2-keyman" $
    args [arg "string" "group-key-hash"] $
    returns "secret-key-id" "string" $
    entry $ bindMatch "hbs2:groupkey:find-secret" $ \case
      [HashLike gkh] -> do

        sto <- getStorage

        gk <- loadGroupKey gkh >>= orThrowUser "can't load groupkey"

        what <- runKeymanClient $ findMatchedGroupKeySecret sto gk
                   >>= orThrowUser "groupkey secret not found"

        let gid = generateGroupKeyId GroupKeyIdBasic1 what

        pure $ mkStr (show $ pretty gid)

      _ -> throwIO $ BadFormException @c nil


  entry $ bindMatch "hbs2:groupkey:decrypt-block" $ \case
    [BlobLike bs] -> do

      sto <- getStorage

      let lbs = LBS.fromStrict bs

      seb <- pure (deserialiseOrFail lbs)
               `orDie` "invalid SmallEncryptedBlock"

      decrypted <- G.decryptBlock sto seb

      pure $ mkForm @c "blob" [mkStr (BS8.unpack decrypted)]

    _ -> throwIO $ BadFormException @C nil

  entry $ bindMatch "hbs2:groupkey:encrypt-block" $ \case
    [StringLike gkh, BlobLike what] -> do
        sto <- getStorage
        gk <- loadGroupKey (fromString gkh)
                `orDie` "can't load group key"
        seb <- G.encryptBlock sto gk what
        pure $ mkForm "blob" [mkStr (LBS8.unpack (serialise seb))]

    _ -> throwIO $ BadFormException @C nil

