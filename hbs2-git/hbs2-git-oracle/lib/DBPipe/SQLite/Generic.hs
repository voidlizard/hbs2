{-# Language AllowAmbiguousTypes #-}
{-# Language UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}
module DBPipe.SQLite.Generic where

import GHC.Generics
import Data.Text qualified as Text
import Data.Text (Text)
import Data.String (IsString(..))
import Text.InterpolatedString.Perl6 (qc)
import Data.Coerce

-- FIXME: move-to-DBPipe
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


class HasTableName a where
  tableName :: SQLName

class HasTableName t => GHasColumnNames t f where
  gColumnNames :: f p -> [SQLName]

class HasTableName t => HasColumnNames t a where
  columnNames :: a -> [SQLName]
  default columnNames :: (Generic a, HasTableName t, GHasColumnNames t (Rep a)) => a -> [SQLName]
  columnNames = gColumnNames @t . from

class HasTableName t => HasColumnName t a where
  columnName :: SQLName

instance HasTableName t => HasColumnNames t [SQLName] where
  columnNames = id

instance HasTableName t => HasColumnNames t SQLName where
  columnNames n = [n]

instance (HasTableName t, Generic a, GHasColumnNames t (Rep a)) => HasColumnNames t a

instance HasTableName t => GHasColumnNames t U1 where
  gColumnNames U1 = []

instance (GHasColumnNames t a, GHasColumnNames t b) => GHasColumnNames t (a :*: b) where
  gColumnNames (a :*: b) = gColumnNames @t a <> gColumnNames @t b

instance (GHasColumnNames t a, GHasColumnNames t b) => GHasColumnNames t (a :+: b) where
  gColumnNames _ = []  -- Не применяется для нашего случая, так как у нас нет вариантов.

instance (HasTableName t, HasColumnName t c) => GHasColumnNames t (K1 i c) where
  gColumnNames (K1 c) = [columnName @t @c]

instance GHasColumnNames t a => GHasColumnNames t (M1 i t a) where
  gColumnNames (M1 a) = gColumnNames @t a

columnListPart :: forall t a . (HasTableName t, HasColumnNames t a) => a -> SQLPart
columnListPart w = SQLPart $ Text.intercalate "," [ coerce @_ @Text x | x <- columnNames @t w ]

bindListPart :: forall t a . (HasTableName t, HasColumnNames t a) => a -> SQLPart
bindListPart w = SQLPart $ Text.intercalate "," [ "?" | _ <- columnNames @t w ]

class (HasTableName t, HasColumnNames t b) => Insert t b where
  insert :: b -> SQLPart

instance (HasTableName t, HasColumnNames t b) => Insert t b where
  insert values = [qc|insert into {tableName @t} values({n}) ({v})|]
    where
      n = bindListPart @t values
      v = columnListPart @t values

