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

import HBS2.System.Logger.Simple

import Data.Config.Suckless

import System.ProgressBar
import System.Exit as Exit
import Control.Monad.Trans.Maybe
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Reader
import Database.SQLite.Simple (Connection)
import Data.Char (isSpace)
import Data.Set qualified as Set
import Data.Set (Set)
import Data.List qualified as List
import Data.Maybe
import Lens.Micro.Platform
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Codec.Serialise
import Control.Concurrent.STM
import System.IO qualified as IO
import UnliftIO.IO qualified as UIO
import System.IO (Handle)
import Data.Kind
import Control.Monad.Catch
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Resource

import System.TimeIt

-- FIXME: remove-udp-hardcode-asap
type Schema = HBS2Basic
type HBS2L4Proto = L4Proto

-- FIXME: introduce-API-type
type API = String

data DBEnv =
  DBEnv { _dbFilePath :: FilePath
        , _dbConn     :: TVar (Maybe Connection)
        }

makeLenses 'DBEnv

type RepoRef = RefLogKey Schema

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
  deriving stock (Generic,Show)

makeLenses 'RepoHead


instance Monoid RepoHead where
  mempty = RepoHead Nothing mempty

instance Semigroup RepoHead where
  (<>) a b = mempty & set repoHEAD  ( view repoHEAD b <|> view repoHEAD a )
                    & set repoHeads ( view repoHeads a <> view repoHeads b )

instance Pretty (AsGitRefsFile RepoHead) where
  pretty (AsGitRefsFile h) = hhead <> vcat (fmap fmt els)
    where
      hhead = case view repoHEAD h of
               Nothing -> mempty
               Just r -> "@" <> pretty r <+> "HEAD"  <> line

      els = HashMap.toList (view repoHeads h)
      fmt (r,hx) = pretty hx <+> pretty (normalizeRef r)


instance Serialise RepoHead

-- FIXME: test-for-from-string-maybe-repohead
--   Нужно написать или сгенерировать тест
instance FromStringMaybe RepoHead where
  fromStringMay "" = Nothing
  fromStringMay s =
    case traverse decodePair (take 2 . words <$> lines trimmed) of
      Right xs -> Just $ mconcat xs
      _        -> Nothing
    where
      trimmed = dropWhile isSpace s
      hbranch x = fromString <$> List.stripPrefix "@" x
      decodePair :: [String] -> Either [String] RepoHead
      decodePair [x, "HEAD"] | "@" `List.isPrefixOf` x = Right $ RepoHead (hbranch x) mempty

      -- special case: deleted branch. should be handled somehow
      decodePair [_] = Right $ RepoHead Nothing mempty

      decodePair [x,r] = case fromStringMay x of
                            Just h  -> Right $ RepoHead Nothing (HashMap.singleton (fromString r) h)
                            Nothing -> Left [r,x]
      decodePair other  = Left other


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


instance (HasCatAPI m, MonadIO m) => HasCatAPI (ResourceT m) where
  getHttpCatAPI = lift getHttpCatAPI
  getHttpSizeAPI = lift getHttpSizeAPI
  getHttpPutAPI = lift getHttpPutAPI
  getHttpRefLogGetAPI = lift getHttpRefLogGetAPI

-- instance (HasCatAPI (App m), MonadIO m) => HasCatAPI (ResourceT (App m)) where
--   getHttpCatAPI = lift getHttpCatAPI
--   getHttpSizeAPI = lift getHttpSizeAPI
--   getHttpPutAPI = lift getHttpPutAPI
--   getHttpRefLogGetAPI = lift getHttpRefLogGetAPI

newtype App m a =
  App { fromApp :: ReaderT AppEnv m a }
  deriving newtype ( Applicative
                   , Functor
                   , Monad
                   , MonadIO
                   , MonadReader AppEnv
                   , MonadThrow
                   , MonadCatch
                   , MonadMask
                   , MonadUnliftIO
                   , MonadTrans
                   )

instance MonadIO m => HasConf (App m) where
  getConf = asks (view appConf)

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

traceTime :: MonadIO m => String -> m a -> m a
traceTime s action = do
  (t, x) <- timeItT action
  trace $ "time" <+> pretty s <+> pretty t
  pure x

