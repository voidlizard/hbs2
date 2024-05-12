{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Fixme.State
  ( evolve
  , withState
  , insertFixme
  , selectFixmeThin
  , selectFixmeHash
  , selectFixme
  , insertCommit
  , selectCommit
  , newCommit
  , HasPredicate(..)
  ) where

import Fixme.Prelude
import Fixme.Types
import Fixme.Config

import HBS2.System.Dir
import Data.Config.Suckless
import Data.Config.Suckless.Syntax
import DBPipe.SQLite

import Data.Aeson as Aeson
import Data.HashMap.Strict qualified as HM
import Text.InterpolatedString.Perl6 (qc)
import Data.Text qualified as Text
import Data.Maybe
import Data.Either
import Data.List (sortBy,sortOn)
import Data.Ord
import Lens.Micro.Platform
import Control.Monad.Trans.Maybe
import Data.Coerce


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
        create table if not exists fixmecommit
          ( hash text not null
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
        ListVal [SymbolVal "or", a, b]   ->  Or (goPred a) (goPred b)
        ListVal [SymbolVal "and", a, b]  ->  And (goPred a) (goPred b)
        ListVal [SymbolVal "like", StringLike a, StringLike b] ->  AttrLike (Text.pack a) (Text.pack b)
        _ -> Ignored

      go :: [Syntax c] -> Syntax c
      go = \case

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

      mklist = List (noContext :: Context c)
      mksym  = Symbol (noContext :: Context c)
      mkstr = Literal (noContext :: Context c) . LitStr

{- HLINT ignore "Functor law" -}
{- HLINT ignore "Eta reduce" -}

selectFixmeHash :: (FixmePerks m) => Text -> FixmeM m (Maybe Text)
selectFixmeHash what = withState do

  r <- select @(Only Text) [qc|select id from fixme where id like ?|] (Only (what <> "%"))
         <&> fmap fromOnly

  pure $ catMaybes [ (x,) <$> Text.length . view _1 <$> Text.commonPrefixes what x | x <- r ]
             & sortBy (comparing (Down . snd))
             & headMay
             & fmap fst


selectFixme :: FixmePerks m => Text -> FixmeM m (Maybe Fixme)
selectFixme txt = do
  attrs <- selectFixmeThin (FixmeHashExactly txt)

  runMaybeT do

    self <- lift (withState $ select [qc|select blob from fixme where id = ? limit 1|] (Only txt))
              <&> listToMaybe . fmap fromOnly
              >>= toMPlus
              <&> (deserialiseOrFail @Fixme)
              >>= toMPlus

    error "what"


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
        let binds = [Bound name, Bound (val <> "%")]
        ([qc|(exists (select null from fixmeattrview x where x.fixme = a.fixme and x.name = ? and x.value like ?))|], binds)


      And a b -> do
        let (asql, abound) = go a
        let (bsql, bbound) = go b
        ([qc|{asql} and {bsql}|], abound <> bbound)

      Or a b -> do
        let asql = go a
        let bsql = go b
        ([qc|{fst asql} or {fst bsql}|], snd asql <> snd bsql)

      Ignored -> ("false", mempty)

selectFixmeThin :: (FixmePerks m, HasPredicate a) => a -> FixmeM m [FixmeThin]
selectFixmeThin a = withState do

  let predic = genPredQ "a" (predicate a)

  let sql = [qc|

select
  cast(json_set(json_group_object(a.name,a.value), '$."fixme-hash"', a.fixme) as blob)

from
  fixmeattrview a join fixme f on a.fixme = f.id

where

  {fst predic}

group by a.fixme
order by f.ts nulls first

  |]

  trace $ yellow "selectFixmeThin" <> line <> pretty sql

  select sql (snd predic) <&> mapMaybe (Aeson.decode @FixmeThin . fromOnly)


