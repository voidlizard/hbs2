module HBS2.Git.Local where

import HBS2.Prelude.Plated

import Data.ByteString.Base16 qualified as B16
import Text.InterpolatedString.Perl6 (qc)
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Char8 (ByteString)
import Codec.Serialise


data SHA1 = SHA1
            deriving stock(Eq,Ord,Data,Generic)

newtype GitHash = GitHash ByteString
                  deriving stock (Eq,Ord,Data,Generic,Show)
                  deriving newtype Hashable

gitHashTomb :: GitHash
gitHashTomb = fromString "0000000000000000000000000000000000"

instance Serialise GitHash

instance IsString GitHash where
  fromString s = GitHash (B16.decodeLenient (BS.pack s))

instance FromStringMaybe GitHash where
  fromStringMay s = either (const Nothing) (pure . GitHash) (B16.decode bs)
    where
      bs = BS.pack s

instance Pretty GitHash where
  pretty (GitHash s) = pretty @String [qc|{B16.encode s}|]


newtype GitRef = GitRef { unGitRef :: ByteString }
                 deriving stock (Eq,Ord,Data,Generic,Show)
                 deriving newtype (IsString,Monoid,Semigroup,Hashable)

instance Serialise GitRef

mkGitRef :: ByteString -> GitRef
mkGitRef = GitRef

instance Pretty GitRef where
  pretty (GitRef x) = pretty @String [qc|{x}|]

data GitObjectType = Commit | Tree | Blob
                     deriving stock (Eq,Ord,Show,Generic,Enum)

instance Serialise GitObjectType

instance IsString GitObjectType where
  fromString = \case
    "commit" -> Commit
    "tree"   -> Tree
    "blob"   -> Blob
    x        -> error [qc|invalid git object type {x}|]

instance FromStringMaybe GitObjectType where
  fromStringMay = \case
    "commit" -> Just Commit
    "tree"   -> Just Tree
    "blob"   -> Just Blob
    _        -> Nothing

instance Pretty GitObjectType where
  pretty = \case
    Commit -> pretty @String "commit"
    Tree   -> pretty @String "tree"
    Blob   -> pretty @String "blob"


