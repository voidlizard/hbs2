{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Fixme.State
  ( evolve
  , withState
  , cleanupDatabase
  , insertFixme
  , insertScanned
  , selectIsAlreadyScanned
  , HasPredicate(..)
  , SelectPredicate(..)
  ) where

import Fixme.Prelude
import Fixme.Types
import Fixme.Config

import HBS2.System.Dir
import Data.Config.Suckless
import Data.Config.Suckless.Syntax
import DBPipe.SQLite hiding (field)

import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.Aeson as Aeson
import Data.HashMap.Strict qualified as HM
import Text.InterpolatedString.Perl6 (q,qc)
import Data.Text qualified as Text
import Data.Maybe
import Data.List qualified as List
import Data.Either
import Data.List (sortBy,sortOn)
import Data.Ord
import Lens.Micro.Platform
import Data.Generics.Product.Fields (field)
import Control.Monad.Trans.Maybe
import Data.Coerce
import Data.Fixed
import Data.Word (Word64)
import System.Directory (getModificationTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import System.TimeIt

-- TODO: runPipe-omitted
--   runPipe нигде не запускается, значит, все изменения
--   будут закоммичены в БД только по явному вызову
--   commitAll или transactional
--   это может объясняеть некоторые артефакты.
--   Но это и удобно: кажется, что можно менять БД
--   на лету бесплатно


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
evolve = withState do
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
              path <- asks fixmeEnvDbPath >>= readTVarIO
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


  ddl [qc| create table if not exists scanned
           ( hash text not null primary key )
         |]

  ddl [qc| create table if not exists object
           ( o   text    not null
           , w   integer not null
           , k   text    not null
           , v   blob    not null
           , primary key (o,k)
           )
         |]


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
        ([qc|(s2.fixme = ?)|], [Bound x])

      AttrLike "fixme-hash" val -> do
        let binds = [Bound (val <> "%")]
        ([qc|(s2.fixme like ?)|], binds)

      AttrLike name val -> do
        let x = val <> "%"
        let binds = [Bound x]
        ([qc|(json_extract({tbl}, '$."{name}"') like ?)|], binds)

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

selectIsAlreadyScanned :: (FixmePerks m, MonadReader FixmeEnv m) => FilePath -> m Bool
selectIsAlreadyScanned file = withState do
  k <- lift $ scannedKeyForFile file
  what <- select @(Only Int) [qc|select 1 from scanned where hash = ? limit 1|] (Only k)
  pure $ not $ List.null what

insertScanned :: (FixmePerks m, MonadReader FixmeEnv m) => FilePath -> DBPipeM m ()
insertScanned file = do
  k <- lift $ scannedKeyForFile file
  insert [qc| insert into scanned (hash)
              values(?)
              on conflict (hash) do nothing|]
         (Only k)

insertFixme :: (FixmePerks m, MonadReader FixmeEnv m) => Fixme -> DBPipeM m ()
insertFixme fme = do

  void $ runMaybeT do

    o   <- fixmeKey fme & toMPlus
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
              when excluded.w > object.w and (excluded.v <> object.v) then excluded.v
              else object.w
            end
      |]

    for_ (HM.toList attrs) $ \(k,v) -> do
      lift $ insert sql (o,w,k,v)

    lift $ insert sql (o,w,"fixme-text",txt)


