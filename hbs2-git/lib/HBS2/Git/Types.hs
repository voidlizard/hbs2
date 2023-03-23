{-# Language AllowAmbiguousTypes #-}
module HBS2.Git.Types where

import HBS2.Prelude
import HBS2.System.Logger.Simple

import Crypto.Hash hiding (SHA1)
import Crypto.Hash qualified as Crypto
import Data.Aeson
import Data.ByteArray qualified as BA
import Data.ByteString.Base16 qualified as B16
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Data
import Data.Generics.Uniplate.Data()
import Data.String (IsString(..))
import Data.Text.Encoding (decodeLatin1)
import Data.Text qualified as Text
import Data.Text (Text)
import GHC.Generics
import Prettyprinter
import Text.InterpolatedString.Perl6 (qc)
import Data.Hashable
import Codec.Serialise
import Data.Maybe

class Monad m => HasCache t k v m where
  cacheLookup  :: t -> k -> m (Maybe v)
  cacheInsert  :: t -> k -> v -> m ()

data SHA1 = SHA1
            deriving stock(Eq,Ord,Data,Generic)

newtype GitHash = GitHash ByteString
                  deriving stock (Eq,Ord,Data,Generic,Show)
                  deriving newtype Hashable

instance Serialise GitHash

instance IsString GitHash where
  fromString s = GitHash (B16.decodeLenient (BS.pack s))

instance Pretty GitHash where
  pretty (GitHash s) = pretty @String [qc|{B16.encode s}|]


data GitObjectType = Commit | Tree | Blob
                     deriving stock (Eq,Ord,Show,Generic)

instance ToJSON GitObjectType
instance FromJSON GitObjectType

instance IsString GitObjectType where
  fromString = \case
    "commit" -> Commit
    "tree"   -> Tree
    "blob"   -> Blob
    x        -> error [qc|invalid git object type {x}|]

instance Pretty GitObjectType where
  pretty = \case
    Commit -> pretty @String "commit"
    Tree   -> pretty @String "tree"
    Blob   -> pretty @String "blob"


data GitObject = GitObject GitObjectType LBS.ByteString

newtype GitRef = GitRef { unGitRef :: Text }
                 deriving stock (Eq,Ord,Data,Generic,Show)
                 deriving newtype (IsString,FromJSON,ToJSON,Monoid,Semigroup,Hashable)

instance Serialise GitRef

mkGitRef :: ByteString -> GitRef
mkGitRef x = GitRef (decodeLatin1 x)

instance Pretty GitRef where
  pretty (GitRef x) = pretty @String [qc|{x}|]


instance FromJSONKey GitRef where
  fromJSONKey = FromJSONKeyText GitRef

class Monad m => HasDependecies m a where
  getDependencies :: a -> m [GitHash]

class GitHashed a where
  gitHashObject :: a -> GitHash

instance GitHashed LBS.ByteString where
  gitHashObject s = GitHash $ BA.convert digest
    where
      digest = hashlazy s :: Digest Crypto.SHA1

instance GitHashed GitObject where
  gitHashObject (GitObject t c) = gitHashObject  (hd <> c)
    where
      hd = LBS.pack $ show (pretty t) <> " " <> show (LBS.length c) <> "\x0"

normalizeRef :: GitRef -> GitRef
normalizeRef (GitRef x) = GitRef "refs/heads/" <> GitRef (fromMaybe x (Text.stripPrefix "refs/heads/" (strip x)))
  where
    strip = Text.dropWhile (=='+')


shutUp :: MonadIO m => m ()
shutUp = do
  setLoggingOff @DEBUG
  setLoggingOff @ERROR
  setLoggingOff @NOTICE
  setLoggingOff @TRACE


