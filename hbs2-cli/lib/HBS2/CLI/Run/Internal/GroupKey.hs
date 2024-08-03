module HBS2.CLI.Run.Internal.GroupKey
  ( module HBS2.CLI.Run.Internal.GroupKey
  , SmallEncryptedBlock(..)
  ) where

import HBS2.CLI.Prelude hiding (mapMaybe)
import HBS2.CLI.Run.Internal

import HBS2.Base58
import HBS2.Hash
import HBS2.Storage
import HBS2.Data.Types.Refs
import HBS2.Data.Types.SmallEncryptedBlock
import HBS2.Storage.Operations.Class
import HBS2.Storage.Operations.ByteString
import HBS2.KeyMan.Keys.Direct
import HBS2.Net.Auth.GroupKeySymm as Symm

import HBS2.Peer.RPC.Client.Unix
import HBS2.Peer.RPC.Client
import HBS2.Peer.RPC.API.Storage

import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.Maybe
import Control.Monad.Trans.Cont
import Control.Monad.Except
import Codec.Serialise
import Data.ByteString (ByteString)

groupKeyFromKeyList :: MonadUnliftIO m => [String] -> m (GroupKey 'Symm HBS2Basic)
groupKeyFromKeyList ks = do
  let members = mapMaybe (fromStringMay @(PubKey 'Encrypt 'HBS2Basic)) ks
  Symm.generateGroupKey @'HBS2Basic Nothing members


encryptBlock :: (MonadUnliftIO m, Serialise t)
             => AnyStorage
             -> GroupKey 'Symm 'HBS2Basic
             -> t
             -> m (SmallEncryptedBlock t)

encryptBlock sto gk x = do

  let HbSyncHash non = hashObject (serialise x)

  gks <- runKeymanClient (extractGroupKeySecret gk)
           >>= orThrowUser "can't extract group key secret"

  Symm.encryptBlock sto gks (Right gk) (Just non) x

decryptBlock :: (MonadUnliftIO m, Serialise t)
             => AnyStorage
             -> SmallEncryptedBlock t
             -> m t
decryptBlock sto seb = do
  let find gk = runKeymanClient (extractGroupKeySecret gk)

  -- FIXME: improve-error-diagnostics
  runExceptT (Symm.decryptBlock sto find seb)
     >>= orThrowUser "can't decrypt block"

loadGroupKey :: ( IsContext c
                , MonadUnliftIO m
                , HasStorage m
                , HasClientAPI StorageAPI UNIX m
                ) => HashRef -> RunM c m (Maybe (GroupKey 'Symm HBS2Basic))
loadGroupKey h = do

  flip runContT pure do
    sto <- getStorage

    raw <- runExceptT (readFromMerkle sto (SimpleKey (fromHashRef h)))
            <&> either (const Nothing) Just

    bs <- ContT (maybe1 raw (pure Nothing))

    let gk = deserialiseOrFail bs
              & either (const Nothing) Just

    pure gk

modifyGroupKey :: (IsContext c, MonadUnliftIO m)
               => GroupKey 'Symm 'HBS2Basic
               -> [Syntax c]
               -> m (GroupKey 'Symm HBS2Basic)
modifyGroupKey gk ins = do

  gks <- runKeymanClient do
          extractGroupKeySecret gk
         `orDie` "can't extract group key secret"

  let r = catMaybes [ fromStringMay @(PubKey 'Encrypt HBS2Basic) k
                    | ListVal [SymbolVal "remove", StringLike k] <- ins
                    ] & HS.fromList

  let a = catMaybes [ fromStringMay @(PubKey 'Encrypt HBS2Basic) k
                    | ListVal [SymbolVal "add", StringLike k] <- ins
                    ] & HS.fromList

  let x = recipients gk & HM.keysSet

  let new = x `HS.difference` r `mappend` a & HS.toList

  generateGroupKey @'HBS2Basic (Just gks) new


