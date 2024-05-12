{-# OPTIONS_GHC -fno-warn-orphans #-}
module Fixme.State
  ( evolve
  , withState
  , insertFixme
  , insertCommit
  , selectCommit
  , newCommit
  ) where

import Fixme.Prelude
import Fixme.Types
import Fixme.Config

import HBS2.System.Dir

import DBPipe.SQLite

import Data.HashMap.Strict qualified as HM
import Text.InterpolatedString.Perl6 (qc)
import Data.Maybe

instance ToField HashRef where
  toField x = toField $ show $ pretty x

instance FromField HashRef where
  fromField = fmap (fromString @HashRef) . fromField @String

evolve :: FixmePerks m => FixmeM  m ()
evolve = do
  dbpath <- localDBPath
  debug $ "evolve" <+> pretty dbpath
  mkdir (takeDirectory dbpath)

  db <- newDBPipeEnv dbPipeOptsDef dbpath

  withDB db do
    createTables

withState :: forall m a . (FixmePerks m, MonadReader FixmeEnv m) => DBPipeM m a ->  m a
withState what = do
  db <- asks fixmeEnvDb
  withDB db what


createTables :: FixmePerks m => DBPipeM m ()
createTables = do

  -- тут все таблицы будут называться с префиксом
  -- fixme, что бы может быть можно было встроить
  -- в другую бд, если вдруг понадобится

  ddl [qc|
        create table if not exists fixmecommit
          ( hash text not null
          , primary key (hash)
          )
      |]

  ddl [qc|
        create table if not exists fixme
          ( id    text not null
          , fixme blob not null
          , primary key (id)
          )
      |]

  ddl [qc|
        create table if not exists fixmeattr
          ( fixme   text not null
          , ts      integer null
          , name    text not null
          , value   text
          , primary key (fixme,ts,name)
          )
      |]

  ddl [qc| drop view if exists fixmeattrview |]

  ddl [qc|
    create view fixmeattrview as
      with ranked as (
        select
          fixme,
          name,
          value,
          row_number() over (partition by fixme, name order by ts desc nulls first) as rn
        from fixmeattr
      )
      select
        fixme,
        name,
        value
      from ranked
      where rn = 1;
    |]


insertCommit :: FixmePerks m => GitHash -> DBPipeM m ()
insertCommit gh = do
  insert [qc|
    insert into fixmecommit (hash) values(?)
    on conflict (hash) do nothing
            |] (Only gh)

selectCommit :: FixmePerks m => GitHash -> DBPipeM m (Maybe GitHash)
selectCommit gh = do
  select [qc|select hash from fixmecommit where hash = ?|] (Only gh)
    <&> fmap fromOnly . listToMaybe

newCommit :: (FixmePerks m, MonadReader FixmeEnv m) => GitHash -> m Bool
newCommit gh = isNothing <$> withState (selectCommit gh)

insertFixme :: FixmePerks m => Fixme -> DBPipeM m ()
insertFixme fx@Fixme{..} = do
  let fixme = serialise fx
  let fxId = hashObject @HbSync fixme & HashRef
  insert [qc|insert into fixme (id, fixme) values (?,?)
             on conflict(id) do nothing
            |] (fxId, fixme)

  for_ (HM.toList fixmeAttr) $ \(n,v) -> do
    insert [qc|
      insert into fixmeattr(fixme,ts,name,value)
      values (?,?,?,?)
      on conflict (fixme,ts,name) do update set value = excluded.value
              |] (fxId, fixmeTs, n, v)

    insert [qc|
      insert into fixmeattr(fixme,ts,name,value)
      values (?,?,?,?)
      on conflict (fixme,ts,name) do update set value = excluded.value
              |] (fxId, fixmeTs, "fixme-tag", fixmeTag)

    insert [qc|
      insert into fixmeattr(fixme,ts,name,value)
      values (?,?,?,?)
      on conflict (fixme,ts,name) do update set value = excluded.value
              |] (fxId, fixmeTs, "fixme-title", fixmeTitle)


