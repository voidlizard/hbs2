{-# OPTIONS_GHC -fno-warn-orphans #-}
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

import HBS2.Git.Data.Tx.Git

import DBPipe.SQLite hiding (insert)
import DBPipe.SQLite.Generic as G

import Lucid.Base
import Data.Text qualified as Text
import Data.Word

-- import Data.Generics.Generic (genericDataType)

import GHC.Generics (Generic)
import Generic.Data -- (gdataDefault, Generically(..))
-- import Data.Data (Data)

-- import Generics.Deriving.Uniplate qualified as U


type MyRefChan = RefChanId L4Proto


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

  pure ()


instance ToField HashRef where
  toField x = toField $ show $ pretty x

instance FromField HashRef where
  fromField = fmap (fromString @HashRef) . fromField @String

instance Pretty (AsBase58 (PubKey 'Sign s)) => ToField (LWWRefKey s) where
  toField x = toField $ show $ pretty (AsBase58 x)

instance FromField (LWWRefKey HBS2Basic) where
  fromField = fmap fromString . fromField @String


newtype TxHash = TxHash HashRef
                 deriving stock (Generic)
                 deriving newtype (ToField)

newtype RepoName = RepoName Text
                   deriving stock (Eq,Show,Generic)
                   deriving newtype (ToField,FromField,ToHtml,IsString)

newtype RepoBrief = RepoBrief Text
                   deriving stock (Generic)
                   deriving newtype (ToField,FromField)

newtype RepoLww = RepoLww (LWWRefKey 'HBS2Basic)
                  deriving stock (Generic)
                  deriving newtype (ToField,FromField,Pretty)

newtype RepoChannel = RepoChannel MyRefChan


newtype RepoHeadRef = RepoHeadRef HashRef
                      deriving stock (Generic)
                      deriving newtype (ToField)


newtype RepoHeadSeq = RepoHeadSeq Word64
                      deriving stock (Generic)
                      deriving newtype (ToField,FromField)


newtype RepoHeadGK0 = RepoHeadGK0 (Maybe HashRef)
                      deriving stock (Generic)
                      deriving newtype (ToField,FromField)

instance ToField RepoChannel where
  toField (RepoChannel x) = toField $ show $ pretty (AsBase58 x)

data TxProcessedTable
data RepoTable
data RepoChannelTable
data RepoNameTable
data RepoBriefTable

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

instance HasColumnName TxHash where
  columnName = "hash"

instance HasColumnName RepoLww where
  columnName = "lww"

instance HasColumnName RepoName where
  columnName = "name"

instance HasColumnName RepoBrief where
  columnName = "brief"

instance HasColumnName RepoChannel where
  columnName = "channel"

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


pattern PRefChan :: MyRefChan -> Syntax C
pattern PRefChan s <- ListVal [ SymbolVal "refchan" , asRefChan -> Just s ]

asRefChan :: Syntax C -> Maybe MyRefChan
asRefChan = \case
  LitStrVal s -> fromStringMay @MyRefChan (Text.unpack s)
  _           -> Nothing

getIndexEntries :: (DashBoardPerks m, HasConf m, MonadReader DashBoardEnv m) => m [MyRefChan]
getIndexEntries = do
  conf <- getConf
  pure [ s | ListVal [ SymbolVal "index", PRefChan s] <- conf ]


data NiceTS = NiceTS

data RepoListItem =
  RepoListItem
  { rlRepoLww      :: RepoLww
  , rlRepoSeq      :: RepoHeadSeq
  , rlRepoName     :: RepoName
  , rlRepoBrief    :: RepoBrief
  , rlRepoGK0      :: RepoHeadGK0
  }
  deriving stock (Generic)

-- deriving instance Data RepoListItem via Generically RepoListItem

rlRepoLwwAsText :: SimpleGetter RepoListItem Text
rlRepoLwwAsText =
  to \RepoListItem{..} -> do
    Text.pack $ show $ pretty $ rlRepoLww

instance FromRow RepoListItem

selectRepoList :: (DashBoardPerks m, MonadReader DashBoardEnv m) => m [RepoListItem]
selectRepoList = fmap fixName <$> withState do
  select_ @_ @RepoListItem [qc|select r.lww
                                    , r.seq
                                    , r.name
                                    , r.brief
                                    , r.gk0
                               from repolistview r
                              |]

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
    0 as seq,
    coalesce(n.name, r.lww) as name,
    coalesce(b.brief, '') as brief,
    null as gk0
  from repo r
    left join name n on r.lww = n.lww
    left join brief b on r.lww = b.lww
  union
  select
    lww,
    seq,
    name,
    brief,
    gk0
  from repohead
),
ranked_repos as (
  select
    lww,
    seq,
    name,
    brief,
    gk0,
    row_number() over (partition by lww order by seq desc) as rn
  from repolist
  order by seq desc
)

select lww, seq, name, brief, gk0
from ranked_repos
where rn = 1;

  |]



createRepoHeadTable :: DashBoardPerks m => DBPipeM m ()
createRepoHeadTable = do
  ddl [qc|
    create table if not exists repohead
      (  lww        text not null
      ,  repohead   text not null
      ,  seq        integer not null
      ,  gk0        text null
      ,  name       text
      ,  brief      text
      ,  primary key (lww,repohead)
      )
  |]

data RepoHeadTable

instance HasTableName RepoHeadTable where
  tableName = "repohead"

instance HasPrimaryKey RepoHeadTable where
  primaryKey = ["lww", "repohead"]

instance HasColumnName RepoHeadRef where
  columnName = "repohead"

instance HasColumnName RepoHeadSeq where
  columnName = "seq"

instance HasColumnName RepoHeadGK0 where
  columnName = "gk0"

insertRepoHead :: (DashBoardPerks m, MonadReader DashBoardEnv m)
               => LWWRefKey 'HBS2Basic
               -> HashRef
               -> RepoHead
               -> DBPipeM m ()
insertRepoHead lww href rh = do
  insert @RepoHeadTable $ onConflictIgnore @RepoHeadTable
    ( RepoLww lww
    , RepoHeadRef href
    , RepoHeadSeq      (_repoHeadTime rh)
    , RepoHeadGK0      (_repoHeadGK0 rh)
    , RepoName         (_repoHeadName rh)
    , RepoBrief        (_repoHeadBrief rh)
    )

  pure ()


