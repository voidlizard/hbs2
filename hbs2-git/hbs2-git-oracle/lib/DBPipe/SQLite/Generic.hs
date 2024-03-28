{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}
module DBPipe.SQLite.Generic where

import GHC.Generics
import Data.Text qualified as Text
import Data.Text (Text)
import Data.String (IsString(..))
import Text.InterpolatedString.Perl6 (qc)
import Data.Coerce

newtype SQLName = SQLName Text
                  deriving stock (Eq,Ord,Show)
                  deriving newtype (IsString,Monoid,Semigroup)

newtype SQLPart = SQLPart Text
                  deriving stock (Eq,Ord,Show)
                  deriving newtype (IsString,Monoid,Semigroup)

class ToSQL a where
  toSQL :: a -> SQLPart

instance ToSQL SQLName where
  toSQL (SQLName a) = SQLPart a

class GHasColumnNames f where
  gColumnNames :: f p -> [SQLName]

class HasColumnNames a where
  columnNames :: a -> [SQLName]
  default columnNames :: (Generic a, GHasColumnNames (Rep a)) => a -> [SQLName]
  columnNames = gColumnNames . from

class HasTableName t where
  tableName :: SQLName

class HasColumnName a where
  columnName :: SQLName

instance HasColumnNames [SQLName] where
  columnNames = id

instance HasColumnNames SQLName where
  columnNames n = [n]

instance (Generic a, GHasColumnNames (Rep a)) => HasColumnNames a

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

columnListPart :: forall a . HasColumnNames a => a -> SQLPart
columnListPart w = SQLPart $ Text.intercalate "," [ coerce @_ @Text x | x <- columnNames w ]

bindListPart :: forall a . HasColumnNames a => a -> SQLPart
bindListPart w = SQLPart $ Text.intercalate "," [ "?" | _ <- columnNames w ]

class (HasTableName t, HasColumnNames b) => Insert t b where
  insert :: b -> SQLPart

instance (HasTableName t, HasColumnNames b) => Insert t b where
  insert values = [qc|insert into {tableName @t} values({v}) ({n})|]
    where
      n = bindListPart values
      v = columnListPart values

