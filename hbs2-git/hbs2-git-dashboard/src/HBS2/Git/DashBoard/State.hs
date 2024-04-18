{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module HBS2.Git.DashBoard.State where

import HBS2.Prelude.Plated
import HBS2.Merkle
import HBS2.Data.Types.Refs
import HBS2.Base58
import HBS2.Clock
import HBS2.Net.Auth.Schema
import HBS2.Misc.PrettyStuff
import HBS2.Net.Proto.Service
import HBS2.Storage

import HBS2.Peer.Proto.LWWRef
import HBS2.Peer.Proto.RefChan.Types
import HBS2.Peer.Proto.RefChan.RefChanUpdate
import HBS2.Peer.RPC.API.RefChan

import HBS2.Git.DashBoard.Types

import HBS2.System.Logger.Simple.ANSI
import Data.Config.Suckless

import DBPipe.SQLite hiding (insert)
import DBPipe.SQLite.Generic as G

import Data.Maybe
import Data.Text qualified as Text
import Text.InterpolatedString.Perl6 (qc)
import Control.Monad.Reader
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Maybe
import Data.Coerce
import Streaming.Prelude qualified as S

type MyRefChan = RefChanId L4Proto


evolveDB :: MonadIO m => DBPipeM m ()
evolveDB = do

  ddl [qc|
    create table if not exists repo
      (  lww text not null
      ,  primary key (lww)
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

  ddl [qc|
    create table if not exists processed
      (  hash text not null
      ,  primary key (hash)
      )
  |]

  pure ()


instance ToField HashRef where
  toField x = toField $ show $ pretty x

instance Pretty (AsBase58 (PubKey 'Sign s)) => ToField (LWWRefKey s) where
  toField x = toField $ show $ pretty (AsBase58 x)

newtype TxHash = TxHash HashRef
                 deriving newtype (ToField)

newtype RepoName = RepoName Text
                   deriving newtype (ToField)

newtype RepoBrief = RepoBrief Text
                   deriving newtype (ToField)

newtype RepoLww = RepoLww (LWWRefKey 'HBS2Basic)
                  deriving newtype (ToField)

data TxProcessedTable
data RepoTable
data RepoNameTable
data RepoBriefTable

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

instance HasPrimaryKey TxProcessedTable where
  primaryKey = [G.columnName @TxHash]

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


updateIndex :: (DashBoardPerks m, HasConf m, MonadReader DashBoardEnv m) => m ()
updateIndex = do
  debug "updateIndex"

  rchanAPI <- asks _refChanAPI
  sto      <- asks _sto

  flip runContT pure do

    es <- lift getIndexEntries

    for_ es $ \rc -> do
      callCC \next -> do
        debug $ red (pretty (AsBase58 rc))

        h <- lift (callRpcWaitMay @RpcRefChanGet (1 :: Timeout 'Seconds) rchanAPI rc)
               <&> join
               >>= maybe (next ()) pure

        debug $ "rechan val" <+> red (pretty h)

        txs <- S.toList_ do
          walkMerkle @[HashRef] (coerce h) (getBlock sto) $ \case
            Left{} -> pure ()
            Right hs -> mapM_ S.yield hs

        for_ txs $ \txh -> void $ runMaybeT do

            done <- lift $ lift $ withState do
                      select @(Only Int)
                             [qc|select 1 from processed where hash = ? limit 1|]
                             (Only (TxHash txh)) <&> isJust . listToMaybe

            guard (not done)

            tx@GitIndexTx{..} <- getBlock sto (coerce txh)
                                    >>= toMPlus
                                    >>= readProposeTranMay @(GitIndexTx 'HBS2Basic) @L4Proto
                                    >>= toMPlus

            lift $ lift $ withState $ transactional do
              let nm  = [ RepoName n  | GitIndexRepoName n <- universeBi gitIndexTxPayload ] & headMay
              let bri = [ RepoBrief n | GitIndexRepoBrief n <- universeBi gitIndexTxPayload ] & headMay

              insert @RepoTable $ onConflictIgnore @RepoTable (Only (RepoLww gitIndexTxRef))

              -- FIXME: on-conflict-update!
              for_ nm $ \n -> do
                insert @RepoNameTable $ onConflictIgnore @RepoNameTable (RepoLww gitIndexTxRef, n)

              for_ bri $ \n -> do
                insert @RepoBriefTable $ onConflictIgnore @RepoBriefTable (RepoLww gitIndexTxRef, n)

        lift $ withState $ transactional do
          for_ txs $ \t -> do
            insert @TxProcessedTable $ onConflictIgnore @TxProcessedTable (Only (TxHash t))

