{-# Language UndecidableInstances #-}
{-# Language AllowAmbiguousTypes #-}

module HBS2.KeyMan.State
  ( module HBS2.KeyMan.State
  , commitAll
  ) where

import HBS2.Prelude.Plated
import HBS2.Base58
import HBS2.Net.Auth.Credentials
import HBS2.Net.Proto.Types

import HBS2.KeyMan.Config

import DBPipe.SQLite

-- import Crypto.Saltine.Core.Sign qualified as Sign
-- import Crypto.Saltine.Core.Box qualified as Encrypt
import System.Directory
import System.FilePath
import Control.Monad.Trans.Maybe
import Text.InterpolatedString.Perl6 (qc)
import Data.Maybe

-- newtype ToDB a = ToDB a
class SomePubKeyType a where
  somePubKeyType :: a -> String

type SomePubKeyPerks a = (Pretty (AsBase58 a))

data SomePubKey (c :: CryptoAction) = forall a . SomePubKeyPerks a => SomePubKey a

newtype PubKeyAlias = PubKeyAlias { fromPubKeyAlias :: Text }
                      deriving newtype (Eq,Ord,IsString)
                      deriving stock (Generic)

deriving newtype instance FromField PubKeyAlias
deriving newtype instance ToField PubKeyAlias

instance SomePubKeyType (SomePubKey 'Sign) where
  somePubKeyType _ = "sign"

instance SomePubKeyType (SomePubKey 'Encrypt) where
  somePubKeyType _ = "encrypt"

populateState :: MonadIO m => DBPipeM m ()
populateState = do

  getStatePath <&> takeDirectory
    >>= liftIO . createDirectoryIfMissing True

  ddl [qc|
    create table if not exists keyfile
    ( key   text not null
    , file  text not null
    , primary key (key)
    )
    |]

  ddl [qc|
    create table if not exists keytype
    ( key   text not null
    , type  text not null
    , primary key (key)
    )
    |]

  ddl [qc|
    create table if not exists keyalias
    ( alias text not null
    , key   text not null
    , primary key (alias)
    )
    |]

  commitAll

instance ToField (SomePubKey a) where
  toField (SomePubKey s) = toField $ show $ pretty (AsBase58 s)

updateKeyFile :: forall a m . (SomePubKeyType (SomePubKey a), MonadIO m)
              => SomePubKey a
              -> FilePath
              -> DBPipeM m ()

updateKeyFile pk fp = do
  insert [qc|
  insert into keyfile (key,file)
  values (?,?)
  on conflict (key) do update set file = excluded.file
  |] (pk, fp)
  pure ()


updateKeyType :: forall a m . (SomePubKeyType (SomePubKey a), MonadIO m)
              => SomePubKey a
              -> DBPipeM m ()
updateKeyType pk = do
  insert [qc|
  insert into keytype (key, type)
  values (?, ?)
  on conflict (key) do update set type = excluded.type
  |] (pk, somePubKeyType pk)
  pure ()

updateKeyAlias :: forall a m . (SomePubKeyType (SomePubKey a), MonadIO m)
               => PubKeyAlias
               -> SomePubKey a
               -> DBPipeM m ()
updateKeyAlias alias pk = do
  insert [qc|
  insert into keyalias (alias, key)
  values (?, ?)
  on conflict (alias) do update set key = excluded.key
  |] (alias, pk)
  pure ()



selectKeyFile :: (MonadIO m, SomePubKeyPerks a)
              => a
              -> DBPipeM m (Maybe FilePath)
selectKeyFile pk = do
  listToMaybe . fmap fromOnly
    <$> select @(Only FilePath) [qc|
          select f.file
          from  keyfile f
          where f.key = ?
          limit 1
          |]  (Only (SomePubKey pk))


data KeyListView =
  KeyListView
  { keyId    :: Text
  , keyType  :: Text
  , keyAlias :: Maybe Text
  , keyFile  :: Maybe Text
  }
  deriving stock (Show,Generic)

instance FromRow KeyListView

instance Pretty KeyListView where
  pretty KeyListView{..} =   fill (-32) (pretty keyId)
                             <+>
                             fill 10 (pretty keyType)
                             <+>
                             pretty keyFile

listKeys :: MonadIO m
         => DBPipeM m [KeyListView]
listKeys = select_ [qc|
  select t.key, t.type, a.alias, f.file
    from keytype t left join keyalias a on a.key = t.key
                   left join keyfile f on f.key = t.key
  |]


