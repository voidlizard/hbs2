{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module HBS2.Git.DashBoard.State
  ( module HBS2.Git.DashBoard.State
  , Only(..)
  , transactional
  ) where

import HBS2.Git.DashBoard.Prelude
import HBS2.Git.DashBoard.Types

import HBS2.Hash

import HBS2.Git.Data.RepoHead
import HBS2.Git.Data.Tx.Git
import HBS2.Git.Local
import HBS2.Git.Local.CLI
import HBS2.System.Dir

import DBPipe.SQLite hiding (insert)
import DBPipe.SQLite qualified as S
import DBPipe.SQLite.Generic as G


import Data.Aeson as Aeson
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.ByteString.Lazy (ByteString)
import Lucid.Base
import Data.Text qualified as Text
import Data.Word
import Data.Either
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Map (Map)
import System.FilePath
import System.Directory

import Skylighting.Core qualified as Sky
import Skylighting qualified as Sky

data RepoListPred =
  RepoListPred
  { _repoListByLww :: Maybe (LWWRefKey 'HBS2Basic)
  , _repoListLimit :: Maybe Int
  }

makeLenses 'RepoListPred

instance Semigroup RepoListPred where
  (<>) _ b = mempty & set repoListByLww (view repoListByLww b)
                    & set repoListLimit (view repoListLimit b)

instance Monoid RepoListPred where
  mempty = RepoListPred Nothing Nothing


evolveDB :: DashBoardPerks m => DBPipeM m ()
evolveDB = do

  ddl [qc|
    create table if not exists repo
      (  lww text not null
      ,  primary key (lww)
      )
  |]


  ddl [qc|
    create table if not exists repochannel
      ( lww text not null
      , channel text not null
      , primary key (lww,channel)
      )
  |]

  ddl [qc|
    create table if not exists brief
      (  lww text not null
      ,  brief text not null
      ,  primary key (lww)
      )
  |]

  ddl [qc|
    create table if not exists name
      (  lww text not null
      ,  name text not null
      ,  primary key (lww)
      )
  |]


  createRepoHeadTable
  createRepoListView

  ddl [qc|
    create table if not exists processed
      (  hash text not null
      ,  primary key (hash)
      )
  |]

  createRepoTreeIndexTable
  createRepoBlobIndexTable
  createRepoCommitTable
  createForksTable

  ddl [qc|
    create table if not exists object
    ( o text not null
    , w integer not null
    , k text not null
    , v text not null
    , nonce text null
    , primary key (o,k)
    )
  |]

instance ToField GitHash where
  toField x = toField $ show $ pretty x

instance FromField GitHash where
  fromField = fmap fromString . fromField @String

instance ToField HashRef where
  toField x = toField $ show $ pretty x

instance FromField HashRef where
  fromField = fmap (fromString @HashRef) . fromField @String

instance Pretty (AsBase58 (PubKey 'Sign s)) => ToField (LWWRefKey s) where
  toField x = toField $ show $ pretty (AsBase58 x)

instance Pretty (AsBase58 (PubKey 'Sign s)) => ToField (RefLogKey s) where
  toField x = toField $ show $ pretty (AsBase58 x)

instance IsRefPubKey s => FromField (RefLogKey s) where
  fromField = fmap (fromString @(RefLogKey s)) . fromField @String

instance FromField (LWWRefKey HBS2Basic) where
  fromField = fmap fromString . fromField @String


newtype TxHash = TxHash HashRef
                 deriving stock (Generic)
                 deriving newtype (ToField)


newtype RepoHeadTx = RepoHeadTx HashRef
                     deriving stock (Generic)
                     deriving newtype (ToField,FromField,Pretty)

instance Serialise RepoHeadTx

newtype RepoName = RepoName Text
                   deriving stock (Eq,Show,Generic)
                   deriving newtype (ToField,FromField,ToHtml,IsString,Pretty)

newtype RepoBrief = RepoBrief Text
                   deriving stock (Generic)
                   deriving newtype (ToField,FromField)


newtype RepoForks = RepoForks Int
                    deriving stock (Generic,Data)
                    deriving newtype (ToField,FromField,Show,Pretty,Num,Eq,Ord)

newtype RepoCommitsNum = RepoCommitsNum Int
                         deriving stock (Generic,Data)
                         deriving newtype (ToField,FromField,Show,Pretty)

newtype RepoLww = RepoLww (LWWRefKey 'HBS2Basic)
                  deriving stock (Generic,Ord,Eq)
                  deriving newtype (ToField,FromField,Pretty,Hashable)

instance Show RepoLww where
  show (RepoLww x) = show $ parens $  "RepoLww" <+> pretty x

newtype RepoLwwSeq = RepoLwwSeq Integer
                     deriving stock (Generic)
                     deriving newtype (ToField,FromField,Pretty)

newtype RepoChannel = RepoChannel MyRefChan

newtype RefChanField = RefChanField MyRefChan
                       deriving stock (Generic)

newtype RepoHeadRef = RepoHeadRef HashRef
                      deriving stock (Generic)
                      deriving newtype (ToField,FromField)

instance Serialise RepoHeadRef


newtype RepoHeadSeq = RepoHeadSeq Word64
                      deriving stock (Generic)
                      deriving newtype (ToField,FromField,Integral,Real,Ord,Eq,Num,Enum)

newtype RepoRefLog = RepoRefLog (RefLogKey 'HBS2Basic)
                     deriving stock (Generic)
                     deriving newtype (ToField,FromField,Pretty,Serialise)

newtype RepoHeadGK0 = RepoHeadGK0 (Maybe HashRef)
                      deriving stock (Generic)
                      deriving newtype (ToField,FromField)

newtype Base58Field a = Base58Field { fromBase58Field :: a }
                        deriving stock (Eq,Ord,Generic)


instance ToField RepoChannel where
  toField (RepoChannel x) = toField $ show $ pretty (AsBase58 x)

instance ToField RefChanField where
  toField (RefChanField x) = toField $ show $ pretty (AsBase58 x)

instance FromField RefChanField where
  fromField w = fromField @String w
    >>= maybe (fail "invalid key") (pure . RefChanField) . fromStringMay

data TxProcessedTable
data RepoTable
data RepoChannelTable
data RepoNameTable
data RepoBriefTable
data RepoCommitTable

instance HasTableName RepoChannelTable where
  tableName = "repochannel"

instance HasTableName RepoTable where
  tableName = "repo"

instance HasTableName RepoNameTable where
  tableName = "name"

instance HasTableName RepoBriefTable where
  tableName = "brief"

instance HasTableName TxProcessedTable where
  tableName = "processed"

instance HasTableName RepoCommitTable where
  tableName = "repocommit"

instance HasColumnName TxHash where
  columnName = "hash"

instance HasColumnName RepoLww where
  columnName = "lww"

instance HasColumnName RepoLwwSeq where
  columnName = "lwwseq"

instance HasColumnName RepoName where
  columnName = "name"

instance HasColumnName RepoBrief where
  columnName = "brief"

instance HasColumnName RepoForks where
  columnName = "forks"

instance HasColumnName RepoCommitsNum where
  columnName = "kommits"

instance HasColumnName RepoRefLog where
  columnName = "reflog"

instance HasColumnName RepoChannel where
  columnName = "channel"

instance HasColumnName RepoCommit where
  columnName = "kommit"

instance HasPrimaryKey TxProcessedTable where
  primaryKey = [G.columnName @TxHash]

instance HasPrimaryKey RepoChannelTable where
  primaryKey = [G.columnName @RepoLww, G.columnName @RepoChannel]

instance HasPrimaryKey RepoTable where
  primaryKey = [G.columnName @RepoLww]

instance HasPrimaryKey RepoNameTable where
  primaryKey = [G.columnName @RepoLww]

instance HasPrimaryKey RepoBriefTable where
  primaryKey = [G.columnName @RepoLww]

instance HasPrimaryKey RepoCommitTable where
  primaryKey = [G.columnName @RepoLww, G.columnName @RepoCommit]

pattern PRefChan :: MyRefChan -> Syntax C
pattern PRefChan s <- ListVal [ SymbolVal "refchan" , asRefChan -> Just s ]

asRefChan :: Syntax C -> Maybe MyRefChan
asRefChan = \case
  LitStrVal s -> fromStringMay @MyRefChan (Text.unpack s)
  _           -> Nothing

getIndexEntries :: (DashBoardPerks m, MonadReader DashBoardEnv m) => m [MyRefChan]
getIndexEntries = do
  pure mempty
  -- conf <- getConf
  -- pure [ s | ListVal [ SymbolVal "index", PRefChan s] <- conf ]


data NiceTS = NiceTS

data RepoListItem =
  RepoListItem
  { rlRepoLww      :: RepoLww
  , rlRepoSeq      :: RepoHeadSeq
  , rlRepoHead     :: RepoHeadRef
  , rlRepoTx       :: RepoHeadTx
  , rlRepoName     :: RepoName
  , rlRepoBrief    :: RepoBrief
  , rlRepoGK0      :: RepoHeadGK0
  , rlRepoForks    :: RepoForks
  , rlRepoCommits  :: RepoCommitsNum
  }
  deriving stock (Generic)

-- deriving instance Data RepoListItem via Generically RepoListItem

rlRepoLwwAsText :: SimpleGetter RepoListItem Text
rlRepoLwwAsText =
  to \RepoListItem{..} -> do
    Text.pack $ show $ pretty $ rlRepoLww

instance FromRow RepoListItem



selectRepoList :: (DashBoardPerks m, MonadReader DashBoardEnv m) => RepoListPred -> m [RepoListItem]
selectRepoList pred = fmap fixName <$> withState do

  let onLww   = maybe1 (view repoListByLww pred) mempty $ \w -> [("r.lww = ?", w)]
  let claus   = onLww

  let where_ | List.null claus = "true"
             | otherwise = Text.intercalate " and " (fmap fst claus)

  let limit_ = case view repoListLimit pred of
                 Nothing -> mempty
                 Just n  -> show $ "limit" <+> pretty n

  let params = fmap snd claus

  let sql = [qc|
  select r.lww
       , r.seq
       , r.repohead
       , r.tx
       , r.name
       , r.brief
       , r.gk0
       , r.forks
       , r.kommits
   from repolistview r
   where  {where_}
   {limit_}
  |]

  debug $ yellow "selectRepoList" <+> pretty sql

  select  @RepoListItem sql params
  where
    fixName x@RepoListItem{..} | Text.length (coerce rlRepoName) < 3 = x { rlRepoName = fixed }
                               | otherwise = x
      where fixed = Text.pack (show $ pretty (coerce @_ @(LWWRefKey 'HBS2Basic) rlRepoLww) ) & RepoName

createRepoListView :: DashBoardPerks m => DBPipeM m ()
createRepoListView = do
  ddl [qc|
drop view if exists repolistview
  |]

  ddl [qc|
create view repolistview as

with repolist as (
  select
    r.lww,
    0 as lwwseq,
    null as reflog,
    0 as seq,
    null as repohead,
    null as tx,
    coalesce(n.name, r.lww) as name,
    coalesce(b.brief, '') as brief,
    null as gk0
  from repo r
    left join name n on r.lww = n.lww
    left join brief b on r.lww = b.lww
  union
  select
    lww,
    lwwseq,
    reflog,
    seq,
    repohead,
    tx,
    name,
    brief,
    gk0
  from repohead
),
ranked_repos as (
  select
    lww,
    lwwseq,
    reflog,
    seq,
    repohead,
    tx,
    name,
    brief,
    gk0,
    row_number() over (partition by lww order by lwwseq desc, seq desc) as rn
  from repolist
  order by seq desc
)

select lww
     , lwwseq
     , reflog
     , seq
     , repohead
     , tx
     , name
     , brief
     , gk0
     , (select count(1) from fork f where f.a = ranked_repos.lww) as forks
     , (select count(distinct(kommit)) from repocommit r where r.lww = ranked_repos.lww) as kommits
from ranked_repos
where rn = 1;
  |]


createForksTable :: DashBoardPerks m => DBPipeM m ()
createForksTable = do
  ddl [qc|
    create table if not exists fork
      ( a text not null
      , b text not null
      , primary key (a,b)
      )
  |]

createRepoHeadTable :: DashBoardPerks m => DBPipeM m ()
createRepoHeadTable = do
  ddl [qc|
    create table if not exists repohead
      (  lww        text not null
      ,  lwwseq     integer not null
      ,  reflog     text not null
      ,  repohead   text not null
      ,  tx         text not null
      ,  seq        integer not null
      ,  gk0        text null
      ,  name       text
      ,  brief      text
      ,  primary key (lww,lwwseq,repohead)
      )
  |]

  ddl [qc|
    create table if not exists repoheadfixme
    ( lww text       not null
    , lwwseq integer not null
    , refchan text   not null
    , primary key (lww, lwwseq)
    )
  |]

data RepoHeadTable

instance HasTableName RepoHeadTable where
  tableName = "repohead"

instance HasPrimaryKey RepoHeadTable where
  primaryKey = ["lww", "lwwseq", "repohead"]

instance HasColumnName RepoHeadRef where
  columnName = "repohead"

instance HasColumnName RepoHeadSeq where
  columnName = "seq"

instance HasColumnName RepoHeadGK0 where
  columnName = "gk0"

instance HasColumnName RepoHeadTx where
  columnName = "tx"


insertRepoHead :: (DashBoardPerks m, MonadReader DashBoardEnv m)
               => LWWRefKey 'HBS2Basic
               -> RepoLwwSeq
               -> RepoRefLog
               -> RepoHeadTx
               -> RepoHeadRef
               -> RepoHead
               -> DBPipeM m ()
insertRepoHead lww lwwseq rlog tx rf rh = do
  insert @RepoHeadTable $ onConflictIgnore @RepoHeadTable
    ( RepoLww lww
    , lwwseq
    , rlog
    , rf
    , tx
    , RepoHeadSeq      (_repoHeadTime rh)
    , RepoHeadGK0      (_repoHeadGK0 rh)
    , RepoName         (_repoHeadName rh)
    , RepoBrief        (_repoHeadBrief rh)
    )

  pure ()


insertRepoFixme :: (DashBoardPerks m, MonadReader DashBoardEnv m)
                => LWWRefKey 'HBS2Basic
                -> RepoLwwSeq
                -> MyRefChan
                -> DBPipeM m ()
insertRepoFixme lww lwwseq r = do
  S.insert [qc|
    insert into repoheadfixme (lww, lwwseq, refchan) values(?,?,?)
    on conflict (lww, lwwseq) do update set refchan = excluded.refchan
              |]
    (lww, lwwseq, RefChanField r)

selectRepoFixme :: (DashBoardPerks m, MonadReader DashBoardEnv m)
                => m [(RepoLww, MyRefChan)]

selectRepoFixme = do
  let sql = [qc|
    select lww, refchan from (select  lww, refchan, max(lwwseq) from repoheadfixme group by lww)
  |]
  withState $ select_ @_ @(RepoLww, RefChanField)  sql
    <&> fmap (over _2 coerce)

-- FIXME: what-if-two-repo-shares-one-reflog?
selectLwwByRefLog :: (DashBoardPerks m, MonadReader DashBoardEnv m) => RepoRefLog -> m (Maybe RepoLww)
selectLwwByRefLog rlog  = withState do
  select [qc|select lww from repolistview where reflog = ?|] (Only rlog)
    <&> listToMaybe . fmap fromOnly

selectRefLogs :: (DashBoardPerks m, MonadReader DashBoardEnv m) => m [RepoRefLog]
selectRefLogs = withState do
  select_ [qc|select distinct(reflog) from repolistview|] <&> fmap fromOnly

-- TODO: too-much-data-in-tree-index
--   для навигации по дереву, если нам не нужно
--   выходить на верхний уровень -- нам не нужно
--   знать коммит для каждого дерева. таким образом,
--   если убрать коммит -- вариативность будет на порядок
--   меньше, но это повлечёт последствия для навигации.
--   сейчас уже 200K записей на 4K коммитов, нехорошо.
--   ОЧЕНЬ НЕХОРОШО. Однако, если удалить kommit из
--   таблицы tree, там начинает выполняться большой рекурсивный
--   запрос. С колонкой kommit мы сокращаем там выборку.
--   В принципе, можно туда ввести ключ lww, тогда выборка
--   будет ограничиваться только всеми деревьями проекта.
--   С этим полем в таблице будет гораздо меньше ключей, чем
--   с каждым коммитом.
createRepoTreeIndexTable :: (DashBoardPerks m) => DBPipeM m ()
createRepoTreeIndexTable = do
  ddl [qc|
    create table if not exists tree
    ( parent text not null
    , tree   text not null
    , kommit text not null
    , level  int  not null
    , path   text not null
    , primary key (parent,tree,kommit)
    )
  |]


createRepoCommitTable :: (DashBoardPerks m) => DBPipeM m ()
createRepoCommitTable  = do
  ddl [qc|
    create table if not exists repocommit
    ( lww     text not null
    , kommit  text not null
    , primary key (lww,kommit)
    )
  |]


isProcessed :: (MonadIO m) => HashRef -> DBPipeM m Bool
isProcessed href = do
  select @(Only Int) [qc|select 1 from processed where hash = ? limit 1|] (Only href)
    <&> not . List.null

insertProcessed :: (DashBoardPerks m) => HashRef -> DBPipeM m ()
insertProcessed href = do
  S.insert [qc|
  insert into processed (hash)
  values(?)
  on conflict(hash) do nothing
  |] (Only href)


newtype RepoCommit = RepoCommit GitHash
                     deriving newtype (FromField,ToField,Pretty)

newtype TreeCommit = TreeCommit GitHash
                     deriving newtype (FromField,ToField,Pretty)

newtype TreeParent = TreeParent GitHash
                     deriving newtype (FromField,ToField,Pretty)


newtype TreeTree =  TreeTree GitHash
                    deriving newtype (FromField,ToField,Pretty)

newtype TreeLevel = TreeLevel Int
                    deriving newtype (FromField,ToField,Pretty,Num,Enum,Real,Integral,Ord,Eq)

newtype TreePath = TreePath FilePath
                   deriving newtype (FromField,ToField,Pretty)

insertTree :: (DashBoardPerks m)
           => (TreeCommit,TreeParent,TreeTree,TreeLevel,TreePath)
           -> DBPipeM m ()
insertTree (commit,parent,tree,level,path) = do
  S.insert [qc|
  insert into tree (parent,tree,kommit,level,path)
  values (?,?,?,?,?)
  on conflict (parent,tree,kommit)
  do nothing
  |] (parent,tree,commit,level,path)

selectParentTree :: (DashBoardPerks m, MonadReader DashBoardEnv m)
                 => TreeCommit
                 -> TreeTree
                 -> m (Maybe TreeParent)
selectParentTree  co me = withState do
  select [qc|select parent from tree where tree = ? and kommit = ?|] (me,co)
    <&> listToMaybe . fmap fromOnly

{- HLINT ignore "Functor law" -}



createRepoBlobIndexTable :: (DashBoardPerks m) => DBPipeM m ()
createRepoBlobIndexTable = do
  ddl [qc|
    create table if not exists blob
    ( hash    text not null
    , name    text not null
    , size    int  not null
    , syntax  text
    , primary key (hash)
    )
  |]


newtype BlobSyn = BlobSyn (Maybe Text)
                  deriving newtype (FromField,ToField,Pretty,Eq,Ord)

newtype BlobName = BlobName FilePath
                   deriving newtype (FromField,ToField,Pretty)

newtype BlobHash = BlobHash GitHash
                   deriving newtype (FromField,ToField,Pretty)

newtype BlobSize = BlobSize Integer
                   deriving newtype (FromField,ToField,Pretty,Num,Enum,Eq,Ord)


data BlobInfo =
  BlobInfo
  { blobHash :: BlobHash
  , blobName :: BlobName
  , blobSize :: BlobSize
  , blobSyn  :: BlobSyn
  }
  deriving stock (Generic)

instance FromRow BlobInfo

type TreeLocator = [(TreeParent, TreeTree, TreeLevel, TreePath)]

insertBlob :: DashBoardPerks m
           => (BlobHash, BlobName, BlobSize, BlobSyn)
           -> DBPipeM m ()
insertBlob (h,n,size,syn) = do
  S.insert [qc|
  insert into blob (hash,name,size,syntax)
  values (?,?,?,?)
  on conflict (hash)
  do update set name = excluded.name
              , size = excluded.size
              , syntax = excluded.syntax
  |] (h,n,size,syn)


selectBlobInfo :: (DashBoardPerks m, MonadReader DashBoardEnv m)
               => BlobHash
               -> m (Maybe BlobInfo)
selectBlobInfo what = withState do
  select [qc|
  select hash,name,size,syntax
  from blob
  where hash = ?
  |] (Only what)
  <&> listToMaybe

selectTreeLocator :: (DashBoardPerks m, MonadReader DashBoardEnv m)
                  => TreeCommit
                  -> TreeTree
                  -> m TreeLocator

selectTreeLocator kommit tree = withState do

  let sql = [qc|
WITH RECURSIVE ParentTree AS (
    SELECT parent, tree, kommit, level, path
    FROM tree
    WHERE tree = ? AND kommit = ?

    UNION ALL

    SELECT t.parent, t.tree, t.kommit, t.level, t.path
    FROM tree t
    JOIN ParentTree pt ON t.tree = pt.parent AND t.kommit = pt.kommit
     WHERE t.kommit = ?
)
SELECT parent, tree, level, path FROM ParentTree
ORDER BY level
|]

  select sql (tree, kommit, kommit)


pattern TreeHash :: GitHash -> LBS8.ByteString
pattern TreeHash hash <- (LBS8.words -> (_ : (fromStringMay . LBS8.unpack -> Just hash) : _))

readBlob :: (DashBoardPerks m, MonadReader DashBoardEnv m)
         => LWWRefKey 'HBS2Basic
         -> BlobHash
         -> m ByteString

readBlob repo hash = do

  dir <- repoDataPath repo

  gitRunCommand [qc|git --git-dir {dir} cat-file blob {pretty hash}|]
    <&> fromRight mempty


updateForks :: (MonadIO m, MonadReader DashBoardEnv m) => LWWRefKey 'HBS2Basic -> m ()
updateForks lww = withState do

  S.insert [qc|
    insert into fork (a,b)
    select distinct r0.lww
                    , r1.lww
      from repocommit r0 join repocommit r1 on r0.kommit = r1.kommit and r0.lww <> r1.lww
      where r0.lww = ?
    on conflict (a,b) do nothing
  |] (Only lww)

  pure ()

checkCommitProcessed :: (MonadIO m, MonadReader DashBoardEnv m)
                     => LWWRefKey 'HBS2Basic -> GitHash -> m Bool
checkCommitProcessed lww co = withState do
  select [qc|select 1 from repocommit where lww = ? and kommit = ?|] (lww, co)
    <&> listToMaybe @(Only Int) <&> isJust

listCommits :: (MonadUnliftIO m, MonadReader DashBoardEnv m)
            => LWWRefKey HBS2Basic -> m [GitHash]
listCommits lww = do
  dir <- repoDataPath lww
  gitRunCommand [qc|git --git-dir {dir} rev-list --all|]
            <&> fromRight mempty
            <&> mapMaybe (headMay . LBS8.words) . LBS8.lines
            <&> mapMaybe (fromStringMay @GitHash . LBS8.unpack)


getTreeRecursive :: (MonadUnliftIO m,MonadReader DashBoardEnv m)
                 => LWWRefKey HBS2Basic
                 -> GitHash
                 -> m  (Map [FilePath] GitHash,[(FilePath, (GitHash, Integer, Maybe Text))])
getTreeRecursive lww co = do

  let syntaxMap = Sky.defaultSyntaxMap

  dir <- repoDataPath lww
  items <- gitRunCommand [qc|git --git-dir {dir} ls-tree -l -r -t {pretty co}|]
    <&> fromRight mempty
    <&> fmap LBS8.words . LBS8.lines
    <&> mapMaybe \case
          [_,"tree",h,_,n] ->
            (reverse $ splitDirectories $ LBS8.unpack n,) <$> fmap Right (fromStringMay @GitHash (LBS8.unpack h))

          [_,"blob",h,size,n] -> do
            let fn = headMay (reverse $ splitDirectories $ LBS8.unpack n)
                       <&> List.singleton

            let ha  = fromStringMay @GitHash (LBS8.unpack h)
            let sz  = readMay @Integer (LBS8.unpack size)

            let syn = Sky.syntaxesByFilename syntaxMap (LBS8.unpack n)
                        & headMay
                        <&> Text.toLower . Sky.sName

            (,) <$> fn <*> fmap Left ( (,,) <$> ha <*> sz <*> pure syn )

          _ -> Nothing

  let trees = Map.fromList [ (k,v) | (k,Right v) <- items ]
  let blobs = [ (k,v) | ([k],Left v) <- items ]
  pure (trees, blobs)

getRootTree :: (MonadUnliftIO m, MonadReader DashBoardEnv m)
            => LWWRefKey HBS2Basic -> GitHash -> m (Maybe GitHash)
getRootTree lww co = do
  dir <- repoDataPath lww
  let cmd =  [qc|git --git-dir {dir} cat-file commit {pretty co}|]

  gitRunCommand cmd
    <&> fromRight mempty
    <&> LBS8.lines
    <&> \case
           (TreeHash ha : _) -> Just ha
           _                 -> Nothing

updateRepoData :: (MonadReader DashBoardEnv m, MonadUnliftIO m)
               => LWWRefKey HBS2Basic -> GitHash -> m ()
updateRepoData lww co  = do

  env <- ask

  void $ runMaybeT do

    root <- lift (getRootTree lww co) >>= toMPlus
    (trees, blobs) <- lift $ getTreeRecursive lww co

    -- lift $ addJob $ liftIO $ withDashBoardEnv env do

    lift $ withState $ transactional do

      insert @RepoCommitTable $
        onConflictIgnore @RepoCommitTable (RepoLww lww, RepoCommit co)

      for_ blobs $ \(fn, (hash, size, syn)) -> do
        insertBlob (BlobHash hash, BlobName fn, BlobSize size, BlobSyn syn)

        for_ (Map.toList trees) $ \(t,h0) -> do

          case t of
            [x] -> insertTree (TreeCommit co,TreeParent root,TreeTree h0,1,TreePath x)
            _   -> pure ()

          let child = tailSafe t
          debug $ red "TREE-REL:" <+> pretty t
          let parent = Map.lookup child trees

          for_ parent $ \p -> do
            debug $ red "FOUND SHIT:" <+> pretty (h0,p)
            insertTree ( TreeCommit co
                       , TreeParent p
                       , TreeTree h0
                       , TreeLevel (length t)
                       , TreePath (headDef "" t)
                       )

  updateForks lww

buildSingleCommitTreeIndex :: ( MonadUnliftIO m
                              , DashBoardPerks m
                              , MonadReader DashBoardEnv m
                              )
                     => LWWRefKey 'HBS2Basic
                     -> GitHash
                     -> m ()
buildSingleCommitTreeIndex lww co = do

  env <- ask
  ignoreCaches <- getIgnoreCaches

  void $ runMaybeT do
    done <- checkCommitProcessed lww co
    let skip = done && not ignoreCaches
    guard (not skip)
    lift $ updateRepoData lww co

buildCommitTreeIndex :: ( MonadUnliftIO m
                        , DashBoardPerks m
                        , MonadReader DashBoardEnv m
                        )
                     => LWWRefKey 'HBS2Basic
                     -> m ()
buildCommitTreeIndex lww = do

  commits <- listCommits lww
  env <- ask

  ignoreCaches <- getIgnoreCaches

  for_ commits $ \co -> void $ runMaybeT do
    done <- checkCommitProcessed lww co
    let skip = done && not ignoreCaches
    guard (not skip)
    lift $ addJob $ withDashBoardEnv env (updateRepoData lww co)

  -- FIXME: check-names-with-spaces

selectRepoForks :: (DashBoardPerks m, MonadReader DashBoardEnv m)
                => LWWRefKey 'HBS2Basic
                -> m [RepoListItem]
selectRepoForks lww = withState do
  let cols = columnListPart (AllColumns @RepoListItem) & fromSQL
  let sql = [qc| select {cols}
                     from repolistview v join fork f on v.lww = f.b
                     where f.a = ?
            |]

  debug $ yellow "selectRepoForks" <+> pretty sql <+> pretty lww
  select sql (Only (RepoLww lww))

gitShowTree :: (DashBoardPerks m, MonadReader DashBoardEnv m)
            => LWWRefKey 'HBS2Basic
            -> GitHash
            -> m [(GitObjectType, GitHash, Text)]
gitShowTree what hash  = do
  path <- repoDataPath what
  let cmd = [qc|git --git-dir {path} ls-tree {show $ pretty hash}|]

  -- FIXME: extract-method
  gitRunCommand cmd
    <&> fromRight mempty
    <&> LBS8.lines
    <&> fmap LBS8.words
    <&> mapMaybe \case
         [_,tp,h,name] -> do
            (,,) <$> fromStringMay (LBS8.unpack tp)
                 <*> fromStringMay (LBS8.unpack h)
                 <*> pure (fromString (LBS8.unpack name))

         _          -> Nothing


gitShowRefs :: (DashBoardPerks m, MonadReader DashBoardEnv m)
            => LWWRefKey 'HBS2Basic
            -> m [(GitRef, GitHash)]

gitShowRefs what = do
  path <- repoDataPath what
  let cmd = [qc|git --git-dir {path} show-ref|]

  sto <- asks _sto

  fromMaybe mempty <$> runMaybeT do

    (_,hd) <- lift (selectRepoList (mempty & set repoListByLww (Just what) & set repoListLimit (Just 1)))
                <&> listToMaybe
                >>= toMPlus
                <&> rlRepoTx
                >>= readRepoHeadFromTx sto . coerce
                >>= toMPlus

    pure $ view repoHeadRefs hd


insertOWKV :: (DashBoardPerks m, ToJSON a)
           => Text
           -> Maybe Integer
           -> Text
           -> a
           -> DBPipeM m ()
insertOWKV o w k v = do

  let sql = [qc|

      insert into object (o, w, k, v)
      values (?, ?, ?, cast (? as text))
      on conflict (o, k)
      do update set
        v = case
              when excluded.w > object.w  then excluded.v
              else object.v
            end,
        w = case
              when excluded.w > object.w  then excluded.w
              else object.w
            end
  |]

  t <- maybe1 w (round <$> liftIO getPOSIXTime) pure

  S.insert sql (o,t,k,Aeson.encode v)


insertOption :: ( DashBoardPerks m
                , MonadReader DashBoardEnv m
                , Pretty a
                , Serialise a)
             => Text
             -> a
             -> m ()
insertOption  key value = do
  w <- liftIO getPOSIXTime <&> fromIntegral . round
  let o = hashObject @HbSync (serialise ("option", key)) & pretty & show
  let v = show $ pretty v
  withState $ transactional do
    insertOWKV (fromString o) (Just w) "$type" "option"
    insertOWKV (fromString o) (Just w) "name"  key
    insertOWKV (fromString o) (Just w) "value" (fromString v)


insertFixmeAllowed :: ( DashBoardPerks m
                      , MonadReader DashBoardEnv m
                      )
                   => RepoRefLog
                   -> m ()
insertFixmeAllowed  reflog = do
  let o = hashObject @HbSync (serialise ("fixme-allowed", reflog)) & pretty & show
  let v = show $ pretty reflog
  withState $ transactional do
    insertOWKV (fromString o) mzero "$type" "fixme-allowed"
    insertOWKV (fromString o) mzero "value" v

deleteFixmeAllowed :: ( DashBoardPerks m
                      , MonadReader DashBoardEnv m
                      )
                   => m ()
deleteFixmeAllowed = do

  let sql = [qc|
    with
      s1 as (
        select o from object where k = '$type' and json_extract(v, '$') = 'fixme-allowed'
      )
    delete from object where o in (select o from s1)
    |]

  withState $ S.insert_  sql

checkFixmeAllowed :: (DashBoardPerks m, MonadReader DashBoardEnv m)
                  => RepoLww
                  -> m Bool

checkFixmeAllowed r = do

  let sql = [qc|
    with
      s1 as (
        select o from object where k = '$type' and json_extract(v, '$') = 'fixme-allowed'
      )
    select 1
      from s1 join object o on s1.o = o.o
      where o.k = 'value' and json_extract(o.v, '$') = ?
      limit 1;
  |]

  w <- withState $ select @(Only Int) sql (Only r)

  pure $ not $ List.null w

selectRepoFixmeRefChan :: (DashBoardPerks m, MonadReader DashBoardEnv m)
                  => RepoLww
                  -> m (Maybe MyRefChan)
selectRepoFixmeRefChan r = do
  let sql = [qc|
    select refchan from (
      select lww
           , refchan
           , max(lwwseq)
      from repoheadfixme
      where lww = ?
      group by lww, refchan
      limit 1)
    |]

  withState  (select @(Only RefChanField) sql (Only r))
    <&> (fmap coerce . headMay)

rpcSocketKey :: String
rpcSocketKey =
  hashObject @HbSync (serialise "rpc-socket-name") & pretty & show

rpcSocketFile :: MonadUnliftIO m => m FilePath
rpcSocketFile = do
  dir <- liftIO $ getXdgDirectory XdgState hbs2_git_dashboard
  pure $ dir </> rpcSocketKey

setRPCSocket :: (DashBoardPerks m, MonadReader DashBoardEnv m) => FilePath -> m ()
setRPCSocket soname = do
  soFile <- rpcSocketFile
  touch soFile
  liftIO $ writeFile soFile soname

delRPCSocket :: (DashBoardPerks m, MonadReader DashBoardEnv m) => m ()
delRPCSocket = do
  rpcSocketFile >>= rm

getRPCSocket :: (DashBoardPerks m, MonadReader DashBoardEnv m) => m (Maybe FilePath)
getRPCSocket = do
  soFile <- rpcSocketFile
  liftIO $ try @_ @IOError (readFile soFile)
   <&> either (const Nothing) Just

