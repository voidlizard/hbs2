{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Fixme.State
  ( evolve
  , withState
  , insertFixme
  , selectFixmeThin
  , selectFixmeHash
  , selectFixmeHashes
  , selectFixme
  , deleteFixme
  , updateFixme
  , insertCommit
  , insertBlob
  , selectObjectHash
  , newCommit
  , cleanupDatabase
  , updateIndexes
  , insertFixmeDelStaged
  , insertFixmeModStaged
  , selectStageModified
  , selectStageDeleted
  , selectStage
  , cleanStage
  , insertProcessed
  , isProcessed
  , HasPredicate(..)
  ) where

import Fixme.Prelude
import Fixme.Types
import Fixme.Config

import HBS2.System.Dir
import Data.Config.Suckless
import Data.Config.Suckless.Syntax
import DBPipe.SQLite hiding (field)

import Data.Aeson as Aeson
import Data.HashMap.Strict qualified as HM
import Text.InterpolatedString.Perl6 (q,qc)
import Data.Text qualified as Text
import Data.Maybe
import Data.Either
import Data.List (sortBy,sortOn)
import Data.Ord
import Lens.Micro.Platform
import Data.Generics.Product.Fields (field)
import Control.Monad.Trans.Maybe
import Data.Coerce
import Data.Fixed
import Data.Word (Word64)
import System.TimeIt


pattern Operand :: forall {c} . Text -> Syntax c
pattern Operand what <- (operand -> Just what)

pattern BinOp :: forall {c} . Id -> Syntax c
pattern BinOp what <- (binOp -> Just what)

binOp :: Syntax c -> Maybe Id
binOp = \case
  SymbolVal "~"  -> Just "like"
  SymbolVal "&&" -> Just "and"
  SymbolVal "||" -> Just "or"
  _              -> Nothing

operand :: Syntax c -> Maybe Text
operand = \case
  SymbolVal c -> Just (coerce c)
  LitStrVal s -> Just s
  LitIntVal i -> Just (Text.pack (show i))
  LitScientificVal v -> Just (Text.pack (show v))
  _ -> Nothing


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
        create table if not exists fixmegitobject
          ( hash text not null
          , type text null
          , primary key (hash)
          )
      |]

  ddl [qc|
        create table if not exists fixme
          ( id    text not null
          , ts    integer
          , fixme blob not null
          , primary key (id)
          )
      |]

  ddl [qc|
        create table if not exists fixmedeleted
          ( id      text not null
          , ts      integer not null
          , deleted bool not null
          , primary key (id,ts)
          )
      |]

  ddl [qc|
        create table if not exists fixmerel
          ( origin  text not null
          , related text not null
          , ts      integer not null
          , reason  text not null
          , primary key (origin,related,ts)
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

  let commits = [qc|name in ('commit','committer','committer-name','committer-email','commit-time')|] :: Text

  ddl [qc|
    create view fixmeattrview as
      with ranked1 as (
        select
          fixme,
          name,
          value,
          row_number() over (partition by fixme, name order by ts desc nulls first) as rn
        from fixmeattr
        where not ({commits})
      )
      , ranked2 as (
        select
          fixme,
          name,
          value,
          row_number() over (partition by fixme, name order by ts asc nulls last) as rn
        from fixmeattr
        where ({commits})
      )

      select distinct fixme,name,value
      from
      (
        select
          fixme,
          name,
          value
        from ranked1
        where rn = 1

        union

        select
          fixme,
          name,
          value
        from ranked2
        where rn = 1
      )
    |]

  ddl [qc|drop view if exists fixmeactualview|]

  ddl [qc|
    create view fixmeactualview as
    with a1 as (
      select
        a.fixme,
        f.ts,
        a.name,
        a.value
      from
        fixmeattrview a
        join fixme f on a.fixme = f.id
      where
        a.name = 'fixme-key'
        and not exists (select null from fixmedeleted d where d.id = f.id)
    ),
    rn AS (
      select
        f.id,
        f.ts,
        a.value AS fixmekey,
        row_number() over (partition by a.value order by f.ts desc) as rn
      from
        fixme f
        join a1 a on f.id = a.fixme and a.name = 'fixme-key'
    )
    select id as fixme, fixmekey, ts from rn
      where rn = 1
      and not exists (
      select null
        from fixmeattr a
             join fixmedeleted d on d.id = a.fixme
        where a.name = 'fixme-key'
              and a.value = rn.fixmekey
      )

    |]


  ddl [qc|
        create table if not exists fixmeactual
          ( fixme   text not null
          , primary key (fixme)
          )
      |]

  ddl [qc|
        create table if not exists fixmejson
          ( fixme     text not null
          , fixmekey  text
          , json      blob
          , primary key (fixme)
          )
      |]

  ddl [qc|
        create index if not exists idx_fixmekey ON fixmejson(fixmekey)
      |]

  ddl [qc| create table if not exists fixmestagedel
           ( hash text    not null primary key
           , ts   integer not null
           )
         |]

  ddl [qc| create table if not exists fixmestagemod
           ( hash   text    not null
           , ts     integer not null
           , attr   text    not null
           , value  text
           , primary key (hash,attr)
           )
         |]

  ddl [qc| create table if not exists fixmeprocessed
           ( hash   text  not null
           , primary key (hash)
           )
         |]

-- .fixme-new/state.db
-- and not exists (select null from fixmedeleted d where a.fixme = id limit 1)

insertCommit :: FixmePerks m => GitHash -> DBPipeM m ()
insertCommit gh = do
  insert [qc|
    insert into fixmegitobject (hash,type) values(?,'commit')
    on conflict (hash) do nothing
            |] (Only gh)

insertBlob :: FixmePerks m => GitHash -> DBPipeM m ()
insertBlob gh = do
  insert [qc|
    insert into fixmegitobject (hash,type) values(?,'blob')
    on conflict (hash) do nothing
            |] (Only gh)

selectObjectHash :: FixmePerks m => GitHash -> DBPipeM m (Maybe GitHash)
selectObjectHash gh = do
  select [qc|select hash from fixmegitobject where hash = ?|] (Only gh)
    <&> fmap fromOnly . listToMaybe

newCommit :: (FixmePerks m, MonadReader FixmeEnv m) => GitHash -> m Bool
newCommit gh = isNothing <$> withState (selectObjectHash gh)

insertFixme :: FixmePerks m => Fixme -> DBPipeM m ()
insertFixme fx@Fixme{..} = do
  let fixme = serialise fx
  let fxId = hashObject @HbSync fixme & HashRef
  insert [qc|insert into fixme (id, ts, fixme) values (?,?,?)
             on conflict(id) do nothing
            |] (fxId, fixmeTs, fixme)

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


data SelectPredicate =
    All
  | FixmeHashExactly Text
  | AttrLike Text Text
  | And SelectPredicate SelectPredicate
  | Or SelectPredicate SelectPredicate
  | Not SelectPredicate
  | Ignored
  deriving stock (Data,Generic,Show)

class HasPredicate a where
  predicate :: a -> SelectPredicate

instance HasPredicate () where
  predicate = const All

instance HasPredicate SelectPredicate where
  predicate = id


instance IsContext c => HasPredicate [Syntax c] where
  predicate s = goPred $ unlist $ go s
    where

      goPred :: Syntax c -> SelectPredicate
      goPred = \case
        ListVal [SymbolVal "not", a]     ->  Not (goPred a)
        ListVal [SymbolVal "or", a, b]   ->  Or (goPred a) (goPred b)
        ListVal [SymbolVal "and", a, b]  ->  And (goPred a) (goPred b)
        ListVal [SymbolVal "like", StringLike a, StringLike b] ->  AttrLike (Text.pack a) (Text.pack b)
        _ -> Ignored

      go :: [Syntax c] -> Syntax c
      go = \case

        ( SymbolVal "!" : rest ) -> do
            mklist [mksym "not", unlist (go rest)]

        ( Operand a : SymbolVal "~" : Operand b  : rest ) -> do
          go (mklist [mksym "like", mkstr a, mkstr b] : rest)

        ( w : SymbolVal "&&" : rest ) -> do
          mklist [mksym "and", unlist w, unlist (go rest)]

        ( w : SymbolVal "||" : rest ) -> do
          mklist [mksym "or", unlist w, unlist (go rest)]

        w -> mklist w

      unlist = \case
        ListVal [x] -> x
        x -> x


{- HLINT ignore "Functor law" -}
{- HLINT ignore "Eta reduce" -}

selectFixmeHash :: (FixmePerks m) => Text -> FixmeM m (Maybe Text)
selectFixmeHash what = listToMaybe <$> selectFixmeHashes what

selectFixmeHashes :: (FixmePerks m) => Text -> FixmeM m [Text]
selectFixmeHashes what = withState do
  let w = what <> "%"
  select @(Only Text)
            [qc| select fixme
                 from fixmejson
                 where json_extract(json,'$."fixme-key"') like ?
                 union
                 select id
                 from fixme
                 where id like ?
            |] (w,w)
         <&> fmap fromOnly

selectFixme :: FixmePerks m => Text -> FixmeM m (Maybe Fixme)
selectFixme txt = do

  attrs <- selectFixmeThin (FixmeHashExactly txt)
             <&> fmap coerce . headMay
             <&> fromMaybe mempty

  runMaybeT do

    lift (withState $ select [qc|select fixme from fixme where id = ? limit 1|] (Only txt))
      <&> listToMaybe . fmap fromOnly
      >>= toMPlus
      <&> (deserialiseOrFail @Fixme)
      >>= toMPlus
      <&> over (field @"fixmeAttr") (<> attrs)


data Bound = forall a . (ToField a, Show a) => Bound a

instance ToField Bound where
  toField (Bound x) = toField x

instance Show Bound where
  show (Bound x) = show x

genPredQ  :: Text -> SelectPredicate -> (Text, [Bound])
genPredQ tbl what = go what
  where
    go = \case
      All -> ("true", mempty)

      FixmeHashExactly x ->
        ([qc|({tbl}.fixme = ?)|], [Bound x])

      AttrLike "fixme-hash" val -> do
        let binds = [Bound (val <> "%")]
        ([qc|({tbl}.fixme like ?)|], binds)

      AttrLike name val -> do
        let x = val <> "%"
        let binds = [Bound x]
        ([qc|(json_extract({tbl}.json, '$."{name}"') like ?)|], binds)

      Not a -> do
        let (sql, bound) = go a
        ([qc|(coalesce(not {sql},true))|], bound)

      And a b -> do
        let (asql, abound) = go a
        let (bsql, bbound) = go b
        ([qc|{asql} and {bsql}|], abound <> bbound)

      Or a b -> do
        let asql = go a
        let bsql = go b
        ([qc|{fst asql} or {fst bsql}|], snd asql <> snd bsql)

      Ignored -> ("false", mempty)


updateFixmeJson :: FixmePerks m =>  DBPipeM m ()
updateFixmeJson = do

  update_ [qc|

    insert into fixmejson (fixme,fixmekey,json)
      with json as  (
        select
            a.fixme as fixme,
            cast(json_set(json_group_object(a.name,a.value), '$."fixme-hash"', f.fixme) as blob) as json

        from
          fixmeattrview a join fixmeactual f on f.fixme = a.fixme

        group by a.fixme
      )

      select
          fixme
        , json_extract(json, '$."fixme-key"') as fixmekey
        , json
         from json where true
      on conflict (fixme) do update set json = excluded.json, fixmekey = excluded.fixmekey
  |]


-- TODO: predicate-for-stage-toggle
selectFixmeThin :: (FixmePerks m, HasPredicate a) => a -> FixmeM m [FixmeThin]
selectFixmeThin a = withState do

  let predic = genPredQ "j" (predicate a)

  let emptyObect = [q|'{}'|] :: String

  let sql = [qc|

with s1 as (
  select  m.hash as hash
        , cast(json_group_object(m.attr,m.value) as blob) as json
  from fixmestagemod m
)

select  cast(json_patch(j.json, coalesce(s.json,{emptyObect})) as blob) as blob

from
  fixmejson j join fixmeactual f on f.fixme = j.fixme
              join fixme f0 on f0.id = f.fixme
              left join s1 s on s.hash = f0.id

where

  (
  {fst predic}
  )

order by f0.ts asc nulls first

  |]

  trace $ red "selectFixmeThin" <> line <> pretty sql

  (t,r) <- timeItT $ select sql (snd predic) <&> mapMaybe (Aeson.decode @FixmeThin . fromOnly)

  trace $ yellow "selectFixmeThin" <> line
            <> pretty sql <> line
            <> pretty (length r) <+> "rows" <> line
            <> pretty "elapsed" <+> pretty (realToFrac t :: Fixed E6)

  pure r

cleanupDatabase :: (FixmePerks m, MonadReader FixmeEnv m) => m ()
cleanupDatabase = do
  warn $ red "cleanupDatabase"
  withState $ transactional do
    update_ [qc|delete from fixme|]
    update_ [qc|delete from fixmeattr|]
    update_ [qc|delete from fixmegitobject|]
    update_ [qc|delete from fixmedeleted|]
    update_ [qc|delete from fixmerel|]
    update_ [qc|delete from fixmeactual|]
    update_ [qc|delete from fixmejson|]
    update_ [qc|delete from fixmestagedel|]
    update_ [qc|delete from fixmestagemod|]


insertFixmeModStaged :: (FixmePerks m,MonadReader FixmeEnv m)
                     => Text
                     -> FixmeAttrName
                     -> FixmeAttrVal
                     -> m ()
insertFixmeModStaged hash k v = withState do
  ts <- getEpoch
  insert [qc| insert into fixmestagemod (hash,ts,attr,value) values(?,?,?,?)
              on conflict (hash,attr)
                 do update set hash  = excluded.hash
                             , ts    = excluded.ts
                             , attr  = excluded.attr
                             , value = excluded.value
            |] (hash,ts,k,v)


insertFixmeDelStaged :: (FixmePerks m,MonadReader FixmeEnv m) => Text -> m ()
insertFixmeDelStaged hash = withState do
  ts <- getEpoch
  insert [qc| insert into fixmestagedel (hash,ts) values(?,?)
              on conflict (hash)
                 do update set hash = excluded.hash
                  , ts = excluded.ts
            |] (hash,ts)


type StageModRow = (HashRef,Word64,Text,Text)

selectStageModified :: (FixmePerks m,MonadReader FixmeEnv m) => m [CompactAction]
selectStageModified = withState do
  what <- select_ @_ @StageModRow [qc|select hash,ts,attr,value from fixmestagemod|]
  for what $ \(h,t,k,v) -> do
    pure $ Modified t h (FixmeAttrName k) (FixmeAttrVal v)

selectStageDeleted :: (FixmePerks m,MonadReader FixmeEnv m) => m [CompactAction]
selectStageDeleted = withState do
  what <- select_ @_ @(HashRef,Word64) [qc|select hash,ts from fixmestagedel|]
  for what $ \(h,t) -> do
    pure $ Deleted t h

selectStage :: (FixmePerks m,MonadReader FixmeEnv m) => m [CompactAction]
selectStage = do
  a <- selectStageModified
  b <- selectStageDeleted
  pure (a<>b)

cleanStage :: (FixmePerks m,MonadReader FixmeEnv m) => m ()
cleanStage = withState do
  transactional do
    update_ [qc|delete from fixmestagedel|]
    update_ [qc|delete from fixmestagemod|]

deleteFixme :: (FixmePerks m,MonadReader FixmeEnv m) => Text -> m ()
deleteFixme hash = withState do
  trace $ red "deleteFixme" <+> pretty hash

  here <- select [qc| select true
                      from fixmedeleted
                      where deleted and id = ?
                      order by ts desc
                      limit 1
                     |] (Only hash) <&> isJust . listToMaybe . fmap (fromOnly @Bool)

  unless here do
    insert [qc| insert into fixmedeleted (id,ts,deleted)
                values (?,(strftime('%s', 'now')),true)
                on conflict(id,ts) do nothing
              |] (Only hash)

updateFixme :: (FixmePerks m,MonadReader FixmeEnv m)
            => Maybe FixmeTimestamp
            -> Text
            -> FixmeAttrName
            ->  FixmeAttrVal
            -> m ()

updateFixme ts hash a b = withState do
  warn $ red "updateFixme" <+> pretty hash
  insert [qc| insert into fixmeattr (fixme,ts,name,value)
              values (?,coalesce(?,strftime('%s', 'now')),?,?)
              on conflict(fixme,ts,name) do update set value = excluded.value
            |] (hash,ts,a,b)

updateIndexes :: (FixmePerks m, MonadReader FixmeEnv m) => m ()
updateIndexes = withState $ transactional do
  update_ [qc|delete from fixmeactual|]
  update_ [qc|
    insert into fixmeactual
      select distinct fixme from fixmeactualview
    |]
  updateFixmeJson
  -- FIXME: delete-table-grows
  --   надо добавлять статус в fixmedeleted
  --   только если он отличается от последнего
  --   известного статуса
  update_ [qc|delete from fixmejson where fixme in (select distinct id from fixmedeleted)|]



insertProcessed :: (FixmePerks m, MonadReader FixmeEnv m, Hashed HbSync w)
                => w
                -> DBPipeM m ()
insertProcessed what = do
  insert [qc| insert into fixmeprocessed (hash) values(?)
              on conflict (hash) do nothing
            |] (Only (show $ pretty $ hashObject @HbSync what))


isProcessed :: (FixmePerks m, MonadReader FixmeEnv m, Hashed HbSync w)
             => w
             -> DBPipeM m Bool
isProcessed what = do
  let k = show $ pretty $ hashObject @HbSync what
  select @(Only (Maybe Int)) [qc| select null from fixmeprocessed where hash = ? limit 1 |] (Only k)
   <&> isJust . listToMaybe

