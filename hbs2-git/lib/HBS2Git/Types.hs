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
import HBS2.Net.Proto.Types(L4Proto)
import HBS2.Data.Types.Refs
import HBS2.Net.Proto.Types
import HBS2.Net.Auth.Credentials

import Data.Config.Suckless

import System.ProgressBar
import System.Exit as Exit
import Control.Monad.Trans.Maybe
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
import Control.Concurrent.STM
import System.IO qualified as IO
import System.IO (Handle)
import Data.Kind
import Control.Monad.Catch

-- FIXME: remove-udp-hardcode-asap
type Schema = HBS2Basic
type HBS2L4Proto = L4Proto

-- FIXME: introduce-API-type
type API = String

type DBEnv = Connection

type RepoRef = RefLogKey Schema

type C = MegaParsec

data ConfBranch
data HeadBranch
data KeyRingFile
data KeyRingFiles
data StoragePref

data AppEnv =
  AppEnv
  { _appCurDir       :: FilePath
  , _appGitDir       :: FilePath
  , _appConf         :: [Syntax C]
  , _appStateDir     :: FilePath
  , _appPeerHttpCat  :: API
  , _appPeerHttpSize :: API
  , _appPeerHttpPut  :: API
  , _appPeerHttpRefLogGet :: API
  , _appRefCred      :: TVar (HashMap RepoRef (PeerCredentials Schema))
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

class HasProgress m where
  type family ProgressMonitor m :: Type
  newProgressMonitor :: String -> Int -> m (ProgressMonitor  m)
  updateProgress :: ProgressMonitor m -> Int -> m ()


instance {-# OVERLAPPABLE #-} MonadIO m => HasProgress m where
  type instance ProgressMonitor m = ProgressBar ()
  updateProgress bar n = liftIO (incProgress bar n)
  newProgressMonitor s total = liftIO $ liftIO $ newProgressBar st 10 (Progress 0 total ())
    where
      st = defStyle { stylePrefix = msg (fromString s)
                    , styleWidth   = ConstantWidth 60
                    }

class MonadIO m => HasCatAPI m where
  getHttpCatAPI  :: m API
  getHttpSizeAPI :: m API
  getHttpPutAPI  :: m API
  getHttpRefLogGetAPI :: m API

class MonadIO m => HasRefCredentials m where
  getCredentials :: RepoRef -> m (PeerCredentials Schema)
  setCredentials :: RepoRef -> PeerCredentials Schema -> m ()

instance (HasCatAPI m, MonadIO m) => HasCatAPI (MaybeT m) where
  getHttpCatAPI = lift getHttpCatAPI
  getHttpSizeAPI = lift getHttpSizeAPI
  getHttpPutAPI = lift getHttpPutAPI
  getHttpRefLogGetAPI = lift getHttpRefLogGetAPI

class Monad m => HasCfgKey a b m where
  -- type family CfgValue a :: Type
  key :: Id

class (Monad m, HasCfgKey a b m) => HasCfgValue a b m where
  cfgValue :: m b

class Monad m => HasConf m where
  getConf :: m [Syntax C]

newtype App m a =
  App { fromApp :: ReaderT AppEnv m a }
  deriving newtype ( Applicative
                   , Functor
                   , Monad
                   , MonadIO
                   , MonadReader AppEnv
                   , MonadThrow
                   , MonadCatch
                   )

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

hPrint :: (Show a, MonadIO m) => Handle -> a -> m ()
hPrint h s = liftIO $ IO.hPrint h s

hPutStrLn :: (Show a, MonadIO m) => Handle -> String -> m ()
hPutStrLn h s = liftIO $ IO.hPutStrLn h s

exitSuccess :: MonadIO m => m ()
exitSuccess = do
  shutUp
  liftIO Exit.exitSuccess

exitFailure :: MonadIO m => m ()
exitFailure = do
  shutUp
  liftIO Exit.exitFailure

die :: MonadIO m => String -> m a
die s = do
  liftIO $ Exit.die s

