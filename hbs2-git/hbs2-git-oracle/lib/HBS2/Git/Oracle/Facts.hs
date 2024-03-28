{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language UndecidableInstances #-}
{-# Language AllowAmbiguousTypes #-}
module HBS2.Git.Oracle.Facts where

import HBS2.Git.Oracle.Prelude

import HBS2.Hash

import DBPipe.SQLite
import DBPipe.SQLite.Generic

import GHC.Generics
import Data.Word

type PKS = PubKey 'Sign HBS2Basic

deriving instance Data (RefLogKey HBS2Basic)
deriving instance Data (LWWRefKey HBS2Basic)

data GitRepoExtended =
  GitRepoExtended
  deriving stock (Generic,Data)

newtype GitLwwRef  = GitLwwRef (LWWRefKey HBS2Basic)
                     deriving stock (Generic,Data)
                     deriving newtype (ToField)

newtype GitLwwSeq  = GitLwwSeq Word64
                     deriving stock (Generic,Data)
                     deriving newtype (ToField)


newtype GitRepoHeadSeq = GitRepoHeadSeq Word64
                         deriving stock (Generic,Data)
                         deriving newtype (ToField)

newtype GitRefLog  = GitRefLog (RefLogKey HBS2Basic)
                     deriving stock (Generic,Data)
                     deriving newtype (ToField)

newtype GitTx  = GitTx HashRef
                 deriving stock (Generic,Data)
                 deriving newtype (ToField)

newtype GitRepoHeadRef  = GitRepoHeadRef HashRef
                          deriving stock (Generic,Data)
                          deriving newtype (ToField)

newtype GitName  = GitName  (Maybe Text)
                   deriving stock (Generic,Data)
                   deriving newtype (ToField)

newtype GitBrief  = GitBrief (Maybe Text)
                    deriving stock (Generic,Data)
                    deriving newtype (ToField)

newtype GitEncrypted = GitEncrypted (Maybe HashRef)
                       deriving stock (Generic,Data)
                       deriving newtype (ToField)

data Facts

data GitRepoFacts =
  GitRepoFacts
  { gitLwwRef       :: GitLwwRef
  , gitLwwSeq       :: GitLwwSeq
  , gitRefLog       :: GitRefLog
  , gitTx           :: GitTx
  , gitRepoHead     :: GitRepoHeadRef
  , gitRepoHeadSeq  :: GitRepoHeadSeq
  , gitName         :: GitName
  , gitBrief        :: GitBrief
  , gitEncrypted    :: GitEncrypted
  , gitExtended     :: [GitRepoExtended]
  }
  deriving stock (Generic,Data)

instance Serialise GitRepoFacts
instance Serialise GitLwwRef
instance Serialise GitLwwSeq
instance Serialise GitRefLog
instance Serialise GitTx
instance Serialise GitRepoHeadRef
instance Serialise GitName
instance Serialise GitBrief
instance Serialise GitRepoExtended
instance Serialise GitEncrypted
instance Serialise GitRepoHeadSeq

instance ToField HashRef where
  toField = toField @String . show . pretty

instance FromField HashRef where
  fromField x = fromField @String x <&> fromString

instance (ToField (LWWRefKey HBS2Basic))  where
  toField = toField @String . show . pretty

instance (FromField (LWWRefKey HBS2Basic))  where
  fromField x = fromField @String x <&> fromString

instance ToField (RefLogKey HBS2Basic) where
  toField = toField @String . show . pretty

instance (FromField (RefLogKey HBS2Basic))  where
  fromField x = fromField @String x <&> fromString

instance HasTableName GitRepoFacts where
  tableName = "gitrepofact"

instance HasPrimaryKey GitRepoFacts where
  primaryKey = ["lwwref","lwwseq","reflog","tx","repohead"]

instance HasColumnName GitLwwRef where
  columnName = "lwwref"

instance HasColumnName GitLwwSeq where
  columnName = "lwwseq"

instance HasColumnName GitRefLog where
  columnName = "reflog"

instance HasColumnName GitTx where
  columnName = "tx"

instance HasColumnName GitRepoHeadRef where
  columnName = "repohead"

instance HasColumnName GitName where
  columnName = "name"

instance HasColumnName GitBrief where
  columnName = "brief"

instance HasColumnName GitEncrypted where
  columnName = "gk"

instance HasColumnName GitRepoHeadSeq where
  columnName = "repoheadseq"

