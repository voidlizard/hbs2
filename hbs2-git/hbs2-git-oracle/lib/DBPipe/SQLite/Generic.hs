{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}
module DBPipe.SQLite.Generic where

import DBPipe.SQLite.Types
import DBPipe.SQLite qualified as SQL
import DBPipe.SQLite hiding (insert,columnName)

import GHC.Generics
import Data.Text qualified as Text
import Data.Text (Text)
import Data.String (IsString(..))
import Text.InterpolatedString.Perl6 (qc)
import Data.Coerce
import UnliftIO

newtype SQLName = SQLName Text
                  deriving stock (Eq,Ord)
                  deriving newtype (IsString,Monoid,Semigroup,Show)

newtype SQLPart = SQLPart Text
                  deriving stock (Eq,Ord)
                  deriving newtype (IsString,Monoid,Semigroup,Show)

class ToSQL a where
  toSQL :: a -> SQLPart

instance ToSQL SQLName where
  toSQL (SQLName a) = SQLPart a


class GHasColumnNames f where
  gColumnNames :: f p -> [SQLName]

class HasTableName a where
  tableName :: SQLName

class HasColumnNames a where
  columnNames :: a -> [SQLName]
  default columnNames :: (Generic a, GHasColumnNames (Rep a)) => a -> [SQLName]
  columnNames = gColumnNames . from

class HasColumnName a where
  columnName :: SQLName

instance HasColumnNames [SQLName] where
  columnNames = id

instance HasColumnNames SQLName where
  columnNames n = [n]

instance {-# OVERLAPPABLE #-} (Generic a, GHasColumnNames (Rep a)) => HasColumnNames a

instance GHasColumnNames U1 where
  gColumnNames U1 = []

instance (GHasColumnNames a, GHasColumnNames b) => GHasColumnNames (a :*: b) where
  gColumnNames (a :*: b) = gColumnNames a <> gColumnNames b

instance (GHasColumnNames a, GHasColumnNames b) => GHasColumnNames (a :+: b) where
  gColumnNames _ = []  -- Не используется для нашего случая.

instance HasColumnName c => GHasColumnNames (K1 i c) where
  gColumnNames (K1 c) = [columnName @c]

instance GHasColumnNames a => GHasColumnNames (M1 i t a) where
  gColumnNames (M1 a) = gColumnNames a

data Bound = forall a . ToField a => Bound a

class GToBoundList f where
  gToBoundList :: f p -> [Bound]

instance GToBoundList U1 where
  gToBoundList U1 = []

instance (GToBoundList a, GToBoundList b) => GToBoundList (a :*: b) where
  gToBoundList (a :*: b) = gToBoundList a <> gToBoundList b

instance (ToField c) => GToBoundList (K1 i c) where
  gToBoundList (K1 c) = [Bound c]

instance GToBoundList a => GToBoundList (M1 i t a) where
  gToBoundList (M1 a) = gToBoundList a

class ToBoundList a where
  toBoundList :: a -> [Bound]
  default toBoundList :: (Generic a, GToBoundList (Rep a)) => a -> [Bound]
  toBoundList = gToBoundList . from

instance (Generic a, GToBoundList (Rep a)) => ToBoundList a where
  toBoundList = gToBoundList . from

columnListPart :: forall a . HasColumnNames a => a -> SQLPart
columnListPart w = SQLPart $ Text.intercalate "," [ coerce @_ @Text x | x <- columnNames w ]

bindListPart :: forall a . HasColumnNames a => a -> SQLPart
bindListPart w = SQLPart $ Text.intercalate "," [ "?" | _ <- columnNames w ]

class HasPrimaryKey t where
  primaryKey :: [SQLName]

newtype OnCoflictIgnore t r = OnCoflictIgnore r
                              deriving stock (Generic)

instance (HasPrimaryKey t, HasColumnNames r) => HasColumnNames (OnCoflictIgnore t r) where
  columnNames (OnCoflictIgnore r) = columnNames r

onConflictIgnore :: forall t r . (HasTableName t, HasColumnNames r) => r -> OnCoflictIgnore t r
onConflictIgnore = OnCoflictIgnore

instance ToField Bound where
  toField (Bound x) = toField x

data BoundQuery =
  BoundQuery SQLPart [Bound]

class (MonadIO m, HasTableName t, HasColumnNames b) => Insert t b m where
  insert :: b -> DBPipeM m ()

instance {-# OVERLAPPABLE #-}
         ( MonadIO m
         , HasTableName t
         , HasColumnNames b
         , ToBoundList b
         ) => Insert t b m where
  insert values = do
    SQL.insert [qc|insert into {tn} values({v}) ({n})|] bound
    where
      v = coerce @_ @Text $ bindListPart values
      n = coerce @_ @Text $ columnListPart values
      bound = toBoundList values
      tn = coerce @_ @Text (tableName @t)

instance {-# OVERLAPPABLE #-}
         ( MonadIO m
         , HasTableName t
         , HasPrimaryKey t
         , HasColumnNames b
         , ToBoundList b
         ) => Insert t (OnCoflictIgnore t b) m where
  insert (OnCoflictIgnore values) = do
    SQL.insert [qc|insert into {tn} ({n}) values({v}) on conflict ({pk}) do nothing|] bound
    where
      v = coerce @_ @Text $ bindListPart values
      n = coerce @_ @Text $ columnListPart values
      bound = toBoundList values
      tn = coerce @_ @Text (tableName @t)
      pk = coerce @_ @Text $ columnListPart $ primaryKey @t

