{-# Language PatternSynonyms #-}
{-# Language UndecidableInstances #-}
{-# Language TemplateHaskell #-}
{-# Language AllowAmbiguousTypes #-}
module HBS2Git.Types
  ( module HBS2Git.Types
  , module Control.Monad.IO.Class
  )
  where

import HBS2.Prelude.Plated
import HBS2.Git.Types

import Data.Config.Suckless

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Reader
import Database.SQLite.Simple (Connection)
import Data.Set qualified as Set
import Data.Set (Set)
import Lens.Micro.Platform
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Codec.Serialise

type API = String

type DBEnv = Connection

type C = MegaParsec

data ConfBranch
data HeadBranch

data AppEnv =
  AppEnv
  { _appCurDir      :: FilePath
  , _appGitDir      :: FilePath
  , _appConf        :: [Syntax C]
  , _appStateDir    :: FilePath
  , _appPeerHttpCat :: String
  }

makeLenses 'AppEnv

newtype AsGitRefsFile a = AsGitRefsFile a

data RepoHead =
  RepoHead
  { _repoHEAD  :: Maybe GitRef
  , _repoHeads :: HashMap GitRef GitHash
  }
  deriving stock (Generic)

makeLenses 'RepoHead


instance Monoid RepoHead where
  mempty = RepoHead Nothing mempty

instance Semigroup RepoHead where
  (<>) a b = mempty & set repoHEAD  ( view repoHEAD b <|> view repoHEAD a )
                    & set repoHeads ( view repoHeads a <> view repoHeads b )

instance Pretty (AsGitRefsFile RepoHead) where
  pretty (AsGitRefsFile h) = vcat (hhead : fmap fmt els)
    where
      hhead = case view repoHEAD h of
               Nothing -> mempty
               Just r -> "@" <> pretty r <+> "HEAD"

      els = HashMap.toList (view repoHeads h)
      fmt (r,hx) = pretty hx <+> pretty (normalizeRef r)

instance Serialise RepoHead



pattern Key :: forall {c}. Id -> [Syntax c] -> [Syntax c]
pattern Key n ns <- SymbolVal  n : ns

class MonadIO m => HasCatAPI m where
  getHttpCatAPI :: m API

class Monad m => HasCfgKey a b m where
  -- type family CfgValue a :: Type
  key :: Id

class (Monad m, HasCfgKey a b m) => HasCfgValue a b m where
  cfgValue :: m b

class Monad m => HasConf m where
  getConf :: m [Syntax C]

newtype App m a =
  App { fromApp :: ReaderT AppEnv m a }
  deriving newtype ( Applicative, Functor, Monad, MonadIO, MonadReader AppEnv )

instance MonadIO m => HasConf (App m) where
  getConf = asks (view appConf)

instance {-# OVERLAPPABLE #-} (HasConf m, Ord b, IsString b, HasCfgKey a (Maybe b) m) => HasCfgValue a (Maybe b) m where
  cfgValue = lastMay . val <$> getConf
    where
      val syn = [ fromString (show $ pretty e)
                | ListVal @C (Key s [LitStrVal e]) <- syn, s == key @a @(Maybe b) @m
                ]

instance {-# OVERLAPPABLE #-} (HasConf m, Ord b, IsString b, HasCfgKey a (Set b) m) => HasCfgValue a (Set b) m where
  cfgValue  = Set.fromList . val <$> getConf
    where
      val syn = [ fromString (show $ pretty e)
                | ListVal @C (Key s [LitStrVal e]) <- syn, s == key @a @(Set b) @m
                ]

