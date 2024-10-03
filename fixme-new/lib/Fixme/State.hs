{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Fixme.State
  ( evolve
  , withState
  , cleanupDatabase
  , listFixme
  , countFixme
  , countByAttribute
  , insertFixme
  , insertFixmeExported
  , modifyFixme
  , insertScannedFile
  , insertScanned
  , selectIsAlreadyScannedFile
  , selectIsAlreadyScanned
  , listAllScanned
  , selectFixmeKey
  , getFixme
  , insertTree
  , FixmeExported(..)
  , HasPredicate(..)
  , SelectPredicate(..)
  , HasLimit(..)
  , HasItemOrder(..)
  , ItemOrder(..)
  , Reversed(..)
  , LocalNonce(..)
  , WithLimit(..)
  , QueryOffset(..)
  , QueryLimit(..)
  , QueryLimitClause(..)
  ) where

import Fixme.Prelude hiding (key)
import Fixme.Types
import Fixme.Config

import HBS2.Base58
import HBS2.System.Dir
import DBPipe.SQLite hiding (field)

import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.Aeson as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.HashMap.Strict qualified as HM
import Text.InterpolatedString.Perl6 (qc)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Maybe
import Data.List qualified as List
import Control.Monad.Trans.Maybe
import Data.Coerce
import Data.Word (Word64)
import System.Directory (getModificationTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

-- TODO: runPipe-omitted
--   runPipe нигде не запускается, значит, все изменения
--   будут закоммичены в БД только по явному вызову
--   commitAll или transactional
--   это может объясняеть некоторые артефакты.
--   Но это и удобно: кажется, что можно менять БД
--   на лету бесплатно


newtype SomeHash h = SomeHash { fromSomeHash :: h }
                     deriving newtype (IsString)

instance Pretty (AsBase58 h) => ToField (SomeHash h) where
  toField (SomeHash h) = toField ( show $ pretty (AsBase58 h))

instance IsString (SomeHash h) => FromField (SomeHash h) where
  fromField = fmap fromString . fromField @String

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
  dbPath <- localDBPath
  debug $ "evolve" <+> pretty dbPath
  mkdir (takeDirectory dbPath)
  withState do
    createTables

withState :: forall m a . (FixmePerks m, MonadReader FixmeEnv m) => DBPipeM m a ->  m a
withState what = do
  lock <- asks fixmeLock

  db <- withMVar lock $ \_ -> do
          t <- asks fixmeEnvDb
          mdb <- readTVarIO t
          case mdb of
            Just d  -> pure (Right d)
            Nothing -> do
              path <- localDBPath
              newDb <- try @_ @IOException (newDBPipeEnv dbPipeOptsDef path)
              case newDb of
                Left e   -> pure (Left e)
                Right db -> do
                  debug "set-new-db"
                  atomically $ writeTVar t (Just db)
                  pure $ Right db

  either throwIO (`withDB` what) db

createTables :: FixmePerks m => DBPipeM m ()
createTables = do

  -- ddl [qc| create table if not exists tree
  --          ( hash   text not null
  --          , nonce  text not null
  --          , primary key (hash,nonce)
  --          )
  --        |]

  ddl [qc| create table if not exists scanned
           ( hash text not null primary key )
         |]

  ddl [qc| create table if not exists object
           ( o   text    not null
           , w   integer not null
           , k   text    not null
           , v   blob    not null
           , nonce text  null
           , primary key (o,k)
           )
         |]


class HasPredicate a where
  predicate :: a -> SelectPredicate

class HasLimit a where
  limit :: a -> Maybe QueryLimitClause

data ItemOrder = Direct | Reverse

class HasItemOrder a where
  itemOrder :: a -> ItemOrder
  itemOrder = const Direct

newtype Reversed a = Reversed a

instance HasItemOrder (Reversed a) where
  itemOrder = const Reverse

-- TODO: move-to-db-pipe?
newtype QueryOffset = QueryOffset Word64
                      deriving newtype (Show,Eq,Ord,Num,Enum,Integral,Real,ToField,FromField,Pretty)

-- TODO: move-to-db-pipe?
newtype QueryLimit = QueryLimit Word64
                      deriving newtype (Show,Eq,Ord,Num,Enum,Integral,Real,ToField,FromField,Pretty)

type QueryLimitClause = (QueryOffset, QueryLimit)

instance HasLimit () where
  limit _ = Nothing

data WithLimit q = WithLimit (Maybe QueryLimitClause) q

instance HasItemOrder q => HasItemOrder (WithLimit q) where
  itemOrder (WithLimit _ q) = itemOrder q

instance HasItemOrder [Syntax c] where
  itemOrder = const Direct

instance HasItemOrder () where
  itemOrder = const Direct

instance HasPredicate q => HasPredicate (WithLimit q) where
  predicate (WithLimit _ query) = predicate query

instance HasLimit (WithLimit a) where
  limit (WithLimit l _) = l

instance HasPredicate q => HasPredicate (Reversed q) where
  predicate (Reversed q) = predicate q

instance HasLimit q => HasLimit (Reversed q) where
  limit (Reversed q) = limit q

data SelectPredicate =
    All
  | FixmeHashExactly Text
  | AttrLike Text Text
  | And SelectPredicate SelectPredicate
  | Or SelectPredicate SelectPredicate
  | Not SelectPredicate
  | Ignored
  deriving stock (Data,Generic,Show)


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
            mkList [mkSym "not", unlist (go rest)]

        ( Operand a : SymbolVal "~" : Operand b  : rest ) -> do
          go (mkList [mkSym "like", mkStr a, mkStr b] : rest)

        ( w : SymbolVal "&&" : rest ) -> do
          mkList [mkSym "and", unlist w, unlist (go rest)]

        ( w : SymbolVal "||" : rest ) -> do
          mkList [mkSym "or", unlist w, unlist (go rest)]

        w -> mkList w

      unlist = \case
        ListVal [x] -> x
        x -> x


{- HLINT ignore "Functor law" -}
{- HLINT ignore "Eta reduce" -}

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
        ([qc|(o.o = ?)|], [Bound x])

      AttrLike name val -> do
        let x = val <> "%"
        let binds = [Bound x]
        ([qc|(json_extract({tbl}.blob, '$."{name}"') like ?)|], binds)

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

      Ignored -> ("true", mempty)


cleanupDatabase :: (FixmePerks m, MonadReader FixmeEnv m) => m ()
cleanupDatabase = do
  warn $ red "cleanupDatabase"
  withState $ transactional do
    update_ [qc|delete from object|]
    update_ [qc|delete from scanned|]

scannedKey :: (FixmePerks m, MonadReader FixmeEnv m) => Fixme -> m HashRef
scannedKey fme = do
  magic <- asks fixmeEnvScanMagic >>= readTVarIO
  let file = fixmeAttr fme & HM.lookup "file"
  let w  = fixmeTs fme
  pure $ hashObject @HbSync ( serialise (magic,w,file) ) & HashRef

scannedKeyForFile :: (FixmePerks m, MonadReader FixmeEnv m) => FilePath-> m HashRef
scannedKeyForFile  file = do
  dir <- fixmeWorkDir
  magic <- asks fixmeEnvScanMagic >>= readTVarIO
  let fn = dir </> file
  w <- liftIO $ getModificationTime fn <&> round . utcTimeToPOSIXSeconds
  pure $ hashObject @HbSync ( serialise (magic,w,file) ) & HashRef

selectIsAlreadyScannedFile :: (FixmePerks m, MonadReader FixmeEnv m) => FilePath -> m Bool
selectIsAlreadyScannedFile file = do
  k <- scannedKeyForFile file
  selectIsAlreadyScanned k

selectIsAlreadyScanned :: (FixmePerks m, MonadReader FixmeEnv m) => HashRef -> m Bool
selectIsAlreadyScanned k = withState do
  what <- select @(Only Int) [qc|select 1 from scanned where hash = ? limit 1|] (Only k)
  pure $ not $ List.null what


insertTree :: FixmePerks m => HashRef -> FixmeKey -> FixmeAttrName -> DBPipeM m ()
insertTree h o k = do
  insert [qc|  insert into tree (hash,o,k)
               values (?,?,?)
               on conflict (hash,o,k) do nothing
         |] (h,o,k)

listAllScanned :: (FixmePerks m, MonadReader FixmeEnv m) => m (HashSet HashRef)
listAllScanned = withState do
  select_ [qc|select hash from scanned|] <&> HS.fromList . fmap ( fromSomeHash . fromOnly )

insertScannedFile :: (FixmePerks m, MonadReader FixmeEnv m) => FilePath -> DBPipeM m ()
insertScannedFile file = do
  k <- lift $ scannedKeyForFile file
  insertScanned k

insertScanned:: (FixmePerks m) => HashRef -> DBPipeM m ()
insertScanned k = do
  insert [qc| insert into scanned (hash)
              values(?)
              on conflict (hash) do nothing|]
         (Only k)

selectFixmeKey :: (FixmePerks m, MonadReader FixmeEnv m) => Text -> m (Maybe FixmeKey)
selectFixmeKey s = do
  withState do
    select @(Only FixmeKey) [qc|select distinct(o) from object where o like ? order by w desc|] (Only (s<>"%"))
       <&> fmap fromOnly
       <&> headMay


sqliteToAeson :: FromJSON a => Text -> Maybe a
sqliteToAeson = Aeson.decode . LBS.fromStrict . Text.encodeUtf8


countFixme :: (FixmePerks m, MonadReader FixmeEnv m) => m Int
countFixme = do

  let present = [qc|coalesce(json_extract(s1.blob, '$.deleted'),'false') <> 'true' |] :: String

  let sql = [qc|
    with s1 as (
      select cast (json_insert(json_group_object(o.k, o.v), '$.w', max(o.w)) as text) as blob
      from object o
      group by o.o
    )
    select count(s1.blob) from s1
    where
      {present}
    |]

  debug $ pretty sql

  withState $ select_ @_ @(Only Int) sql
    <&> maybe 0 fromOnly . headMay


countByAttribute :: ( FixmePerks m
                    , MonadReader FixmeEnv m
                    )
                 => FixmeAttrName
                 -> m [(FixmeAttrVal, Int)]
countByAttribute name = do
  let sql = [qc|


    select v, count(1) from object o
    where not exists
      ( select null from object o1
        where o1.o = o.o
          and o1.k = 'deleted' and o1.v == 'true'
      )
    and o.k = ?
    group by v

    |]

  withState $ select sql (Only name)

listFixme :: ( FixmePerks m
             , MonadReader FixmeEnv m
             , HasPredicate q
             , HasLimit q
             , HasItemOrder q
             )
          => q
          -> m [Fixme]
listFixme expr = do

  let (w,bound) = genPredQ "s1" (predicate expr)

  let present = [qc|and coalesce(json_extract(s1.blob, '$.deleted'),'false') <> 'true' |] :: String

  let (limitClause, lbound) = case limit expr of
                                 Just (o,l) -> ([qc|limit ? offset ?|] :: String, [Bound l, Bound o])
                                 Nothing -> (mempty, [])

  let o = case itemOrder expr of
            Direct  -> "asc" :: String
            Reverse -> "desc"

  let sql = [qc|
    with s1 as (
      select cast (json_insert(json_group_object(o.k, o.v), '$.fixme-timestamp', cast(max(o.w) as text)) as text) as blob
      from object o
      group by o.o
    )
    select s1.blob from s1
    where
      {w}
      {present}
    order by
        json_extract(s1.blob, '$.commit-time') {o} nulls last,
        json_extract(s1.blob, '$.w') {o} nulls last
    {limitClause}
    |]

  debug $ pretty sql

  withState $ select @(Only Text) sql (bound <> lbound)
        <&> fmap (sqliteToAeson . fromOnly)
        <&> catMaybes

getFixme :: (FixmePerks m, MonadReader FixmeEnv m) => FixmeKey -> m (Maybe Fixme)
getFixme key = do

  let sql = [qc|
    select cast (json_insert(json_group_object(o.k, o.v), '$.fixme-timestamp', cast(max(o.w) as text)) as text) as blob
    from object o
    where o.o = ?
    group by o.o
    limit 1
    |]

  runMaybeT do

    lift (withState $ select @(Only Text) sql (Only key))
      <&> fmap (sqliteToAeson . fromOnly)
      <&> catMaybes
      <&> headMay
      >>= toMPlus


modifyFixme :: (FixmePerks m)
            => FixmeKey
            -> [(FixmeAttrName, FixmeAttrVal)]
            -> FixmeM m ()
modifyFixme o a' = do
  FixmeEnv{..} <- ask

  attrNames <- readTVarIO fixmeEnvAttribs
  values    <- readTVarIO fixmeEnvAttribValues

  now <- liftIO getPOSIXTime <&> fromIntegral . round

  let a = [ (k,v) | (k,v) <- a'
          , k `HS.member` attrNames
          , not (HM.member k values) || v `HS.member` fromMaybe mempty (HM.lookup k values)
          ]

  let w = mempty { fixmeAttr = HM.fromList a, fixmeKey = o, fixmeTs = Just now }

  withState $ insertFixme w

insertFixme :: (FixmePerks m, MonadReader FixmeEnv m) => Fixme -> DBPipeM m ()
insertFixme fme = do

  void $ runMaybeT do

    let o = fixmeKey fme
    w   <-  fixmeTs fme & toMPlus
    let attrs   = fixmeAttr fme
    let txt = fixmePlain fme & Text.unlines . fmap coerce

    let sql = [qc|
      insert into object (o, w, k, v)
      values (?, ?, ?, ?)
      on conflict (o, k)
      do update set
        v = case
              when excluded.w > object.w and (excluded.v <> object.v) then excluded.v
              else object.v
            end,
        w = case
              when excluded.w > object.w and (excluded.v <> object.v) then excluded.w
              else object.w
            end,
        nonce = case when excluded.w > object.w and (excluded.v <> object.v) then excluded.nonce
                     else object.nonce
                end
      |]

    for_ (fixmeStart fme) $ \s -> do
      lift $ insert sql (o,w,"fixme-start",s)

    for_ (fixmeEnd fme) $ \s -> do
      lift $ insert sql (o,w,"fixme-end",s)

    for_ (HM.toList attrs) $ \(k,v) -> do
      lift $ insert sql (o,w,k,v)

    lift $ insert sql (o,w,"fixme-text",txt)


data FixmeExported =
  FixmeExported
  { exportedKey    :: FixmeKey
  , exportedWeight :: Word64
  , exportedName   :: FixmeAttrName
  , exportedValue  :: FixmeAttrVal
  }
  deriving stock Generic

instance FromRow FixmeExported
instance ToRow  FixmeExported
instance Serialise FixmeExported

class LocalNonce a where
  localNonce :: a -> HashRef

instance LocalNonce FixmeExported where
  localNonce FixmeExported{..} =
    HashRef $ hashObject @HbSync
            $ serialise (exportedKey,exportedName,exportedValue,exportedWeight)

instance LocalNonce (HashRef, FixmeExported) where
  localNonce (h, e) =  HashRef $ hashObject @HbSync
                               $ serialise (h, localNonce e)

data WithNonce a = WithNonce HashRef a

instance ToRow (WithNonce FixmeExported) where
  toRow (WithNonce nonce f@FixmeExported{..}) = toRow (exportedKey, exportedWeight, exportedName, exportedValue, nonce)

insertFixmeExported :: FixmePerks m => HashRef -> FixmeExported -> DBPipeM m ()
insertFixmeExported h item = do

  let sql = [qc|

      insert into object (o, w, k, v, nonce)
      values (?, ?, ?, ?, ?)
      on conflict (o, k)
      do update set
        v = case
              when excluded.w > object.w  then excluded.v
              else object.v
            end,
        w = case
              when excluded.w > object.w  then excluded.w
              else object.w
            end,
        nonce = case
                  when excluded.w > object.w then excluded.nonce
                  else object.nonce
                end
  |]

  insert sql (WithNonce h item)
  insertScanned h


