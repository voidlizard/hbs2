{-# Language UndecidableInstances #-}
{-# Language AllowAmbiguousTypes #-}

module HBS2.KeyMan.State
  ( module HBS2.KeyMan.State
  , commitAll
  , transactional
  , module Exported
  ) where

import HBS2.Prelude.Plated
import HBS2.Base58
import HBS2.Hash
import HBS2.Net.Auth.Credentials
import HBS2.Data.Types.Refs
import HBS2.Net.Auth.GroupKeySymm as Exported

import HBS2.KeyMan.Config

import DBPipe.SQLite

-- import Crypto.Saltine.Core.Sign qualified as Sign
-- import Crypto.Saltine.Core.Box qualified as Encrypt
import System.Directory
import System.FilePath
import Text.InterpolatedString.Perl6 (qc)
import Data.Maybe
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.HashMap.Strict qualified as HM
import Data.Coerce

import UnliftIO


newtype SomeHash a = SomeHash a
                     deriving stock Generic

instance ToField (SomeHash HashRef) where
  toField (SomeHash x) = toField $ show $ pretty x

instance FromField (SomeHash HashRef) where
  fromField = fmap (SomeHash . fromString @HashRef) . fromField @String

instance ToField (SomeHash GroupKeyId) where
  toField (SomeHash x) = toField $ show $ pretty x

instance FromField (SomeHash GroupKeyId) where
  fromField = do
    fmap (SomeHash . convert . fromString @HashRef) . fromField @String
    where
      convert ha = GroupKeyId (coerce ha)

-- newtype ToDB a = ToDB a
class SomePubKeyType a where
  somePubKeyType :: a -> String

type SomePubKeyPerks a = (Pretty (AsBase58 a), FromStringMaybe a)

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

  ddl [qc|
    create table if not exists keyweight
    ( key    text not null
    , weight int not null
    , primary key (key)
    )
    |]


  ddl [qc|
    create table if not exists gkseentx
    ( hash  text not null
    , primary key (hash)
    )
    |]

  ddl [qc|
    create table if not exists gktrack
    ( secret  text not null
    , gkhash  text not null
    , primary key (secret,gkhash)
    )
    |]

  ddl [qc|
    create table if not exists gkaccess
    ( gkhash  text not null
    , key     text not null
    , primary key (gkhash,key)
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
  { keyId     :: Text
  , keyType   :: Text
  , keyWeight :: Maybe Word
  , keyAlias  :: Maybe Text
  , keyFile   :: Maybe Text
  }
  deriving stock (Show,Generic)

instance FromRow KeyListView

instance Pretty KeyListView where
  pretty KeyListView{..} =   fill 44 (pretty keyId)
                             <+> fill 5 (pretty keyWeight)
                             <+>
                             fill 10 (pretty keyType)
                             <+>
                             pretty keyFile

listKeys :: MonadIO m => DBPipeM m [KeyListView]
listKeys = select_ [qc|
  select t.key, t.type, w.weight, a.alias, f.file
    from keytype t
    left join keyalias a on a.key = t.key
    left join keyfile f on f.key = t.key
    left join keyweight w on w.key = t.key
    order by w.weight ASC, f.file ASC
  |]


deleteKey :: (MonadUnliftIO m, ToField a) => a -> DBPipeM m ()
deleteKey keyId = transactional do
  insert [qc|delete from keyfile where key = ?|] (Only keyId)
  insert [qc|delete from keytype where key = ?|] (Only keyId)
  insert [qc|delete from keyalias where key = ?|] (Only keyId)
  insert [qc|delete from keyweight where key = ?|] (Only keyId)
  commitAll



updateKeyWeight :: (MonadIO m, ToField a) => a -> Int -> DBPipeM m ()
updateKeyWeight key weight = do
  insert [qc|
    insert into keyweight (key, weight)
    values (?, ?)
    on conflict (key) do update set weight = excluded.weight
    |] (key, weight)
  pure ()

selectKeyWeight :: (MonadIO m, SomePubKeyPerks a)
                => a
                -> DBPipeM m Word

selectKeyWeight key = do
  select [qc|
    select coalesce(weight,0) as weight
    from keyweight
    where key = ?
    limit 1
    |] (Only (SomePubKey key)) <&> maybe 0 fromOnly . listToMaybe


deleteAllSeenGKTx :: MonadIO m => DBPipeM m ()
deleteAllSeenGKTx = do
  insert_ [qc|delete from gkseentx|]

insertSeenGKTx :: (MonadIO m) => HashRef  -> DBPipeM m ()
insertSeenGKTx hash = do
  insert [qc|
    insert into gkseentx (hash) values(?)
       on conflict (hash) do nothing
           |] (Only (SomeHash hash))

selectAllSeenGKTx :: (MonadIO m) => DBPipeM m (HashSet HashRef)
selectAllSeenGKTx = do
  select_ [qc|select hash from gkseentx|] <&> HS.fromList . fmap (coerce . fromOnly @(SomeHash HashRef))


insertGKTrack :: MonadIO m => GroupKeyId -> HashRef -> DBPipeM m ()
insertGKTrack s g = do
  insert [qc|
    insert into gktrack (secret,gkhash)
    values(?,?)
    on conflict (secret,gkhash) do nothing
    |] (SomeHash s, SomeHash g)

insertGKAccess:: MonadIO m => HashRef -> GroupKey 'Symm 'HBS2Basic -> DBPipeM m ()
insertGKAccess gkh gk = do
  let rcpt = recipients gk & HM.keys
  for_ rcpt $ \k -> do
    insert [qc|
      insert into gkaccess (gkhash,key)
      values(?,?)
      on conflict (gkhash,key) do nothing
              |] (SomeHash gkh, SomePubKey k)





