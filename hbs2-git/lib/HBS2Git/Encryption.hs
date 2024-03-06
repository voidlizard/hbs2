module HBS2Git.Encryption
  ( module HBS2Git.Encryption
  , module HBS2Git.Encryption.KeyInfo
  , module HBS2.Net.Auth.GroupKeySymm
  ) where

import HBS2Git.Prelude

import HBS2.Net.Auth.Credentials
import HBS2.Net.Proto.Types hiding (Cookie)
import HBS2.Net.Auth.GroupKeySymm hiding (Cookie)


import HBS2Git.Encryption.KeyInfo

import Data.Config.Suckless.Syntax
import Data.Config.Suckless.KeyValue

import Data.HashSet qualified as HashSet
import Data.Maybe
import Data.Text qualified as Text
import Data.Time.Clock.POSIX

-- type ForEncryption ?

isRefEncrypted :: (MonadIO m, HasConf m) => PubKey 'Sign HBS2Basic -> m Bool
isRefEncrypted ref = do
  conf <- getConf

  let ee = [ True
           | (ListVal (SymbolVal "encrypted" : (LitStrVal r) : _))  <- conf
           , fromStringMay (Text.unpack r) == Just ref
           ]

  -- liftIO $ hPutDoc stderr $ "isRefEncrypted" <+> pretty (AsBase58 ref) <+> pretty ee <+> pretty (not (null ee)) <> line

  pure $ not $ null ee

getKeyInfo :: (MonadIO m, HasConf m) => PubKey 'Sign HBS2Basic -> m (Maybe KeyInfo)
getKeyInfo ref = do
  conf <- getConf

  now <- liftIO getPOSIXTime
  let every = [ keyInfoFrom now syn | syn <- conf
              , isJust (keyInfoFrom now syn)
              ] & catMaybes

  pure (lastMay [ x | x <- every, keyInfoRef x == ref ])


genGK0 :: (MonadIO m) => KeyInfo -> m (GroupKey 'Symm  HBS2Basic)
genGK0 ki = generateGroupKey @HBS2Basic Nothing members
  where
    members = HashSet.toList ( keyInfoOwner ki `HashSet.insert` keyInfoMembers ki )

