{-# Language PatternSynonyms #-}
{-# Language UndecidableInstances #-}
{-# Language TemplateHaskell #-}
{-# Language AllowAmbiguousTypes #-}
module HBS2Git.Types
  ( module HBS2Git.Types
  , module Control.Monad.IO.Class
  , HasStorage(..)
  , HasConf(..)
  , AnyStorage(..)
  , RefLogKey(..)
  )
  where

import HBS2.Prelude.Plated
import HBS2.Hash
import HBS2.Clock
import HBS2.Git.Types
import HBS2.Actors.Peer.Types (HasStorage(..),AnyStorage(..))
import HBS2.Peer.RPC.Client.Unix hiding (Cookie)
import HBS2.Net.Proto.RefLog (RefLogKey(..))
import HBS2.Net.Auth.Credentials

import HBS2.Peer.RPC.API.Peer
import HBS2.Peer.RPC.API.RefLog
import HBS2.Peer.RPC.API.Storage

import HBS2.System.Logger.Simple

import Data.Config.Suckless

import System.ProgressBar
import System.Exit as Exit
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.ByteString.Lazy.Char8 qualified as LBS
import Database.SQLite.Simple (Connection)
import Data.Char (isSpace)
import Data.List qualified as List
import Lens.Micro.Platform
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Control.Concurrent.STM
import System.IO qualified as IO
import System.IO (Handle)
import Data.Kind
import Control.Monad.Catch
import Control.Monad.IO.Unlift

import System.TimeIt

-- FIXME: remove-udp-hardcode-asap
type Schema = HBS2Basic
type HBS2L4Proto = L4Proto

-- FIXME: introduce-API-type
type API = String

newtype Cookie =
  Cookie { fromCookie :: Text }
  deriving newtype (Eq,Ord,Show)

instance IsString Cookie where
  fromString s = Cookie cookie
    where cookie = fromString $ take 8
                              $ show
                              $ pretty
                              $ hashObject @HbSync (LBS.pack s)
data DBEnv =
  DBEnv { _dbFilePath :: FilePath
        , _dbCookie   :: Cookie
        , _dbConn     :: TVar (Maybe Connection)
        }

makeLenses 'DBEnv

type RepoRef = RefLogKey Schema

data ConfBranch
data HeadBranch
data KeyRingFile
data KeyRingFiles
data StoragePref

data RPCEndpoints =
  RPCEndpoints
  { rpcPeer    :: ServiceCaller PeerAPI UNIX
  , rpcStorage :: ServiceCaller StorageAPI UNIX
  , rpcRefLog  :: ServiceCaller RefLogAPI UNIX
  }

data AppEnv =
  AppEnv
  { _appCurDir       :: FilePath
  , _appGitDir       :: FilePath
  , _appConf         :: [Syntax C]
  , _appStateDir     :: FilePath
  , _appRefCred      :: TVar (HashMap RepoRef (PeerCredentials Schema))
  , _appKeys         :: TVar (HashMap (PubKey 'Encrypt Schema) (PrivKey 'Encrypt Schema))
  , _appOpts         :: TVar (HashMap String String)
  , _appRpc          :: RPCEndpoints
  }

makeLenses 'AppEnv

newtype AsGitRefsFile a = AsGitRefsFile a

class HasRPC m where
  getRPC :: m RPCEndpoints

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

class MonadIO m => HasRefCredentials m where
  getCredentials :: RepoRef -> m (PeerCredentials Schema)
  setCredentials :: RepoRef -> PeerCredentials Schema -> m ()

class MonadIO m => HasGlobalOptions m where
  addGlobalOption :: String -> String -> m ()
  getGlobalOption :: String -> m (Maybe String)

class MonadIO m => HasEncryptionKeys m where
  addEncryptionKey  :: KeyringEntry Schema -> m ()
  findEncryptionKey :: PubKey 'Encrypt Schema -> m (Maybe (PrivKey 'Encrypt Schema))
  enumEncryptionKeys :: m [KeyringEntry Schema]

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
  shutUp
  pause @'Seconds 0.1
  liftIO $ Exit.die s

traceTime :: MonadIO m => String -> m a -> m a
traceTime s action = do
  (t, x) <- timeItT action
  trace $ "time" <+> pretty s <+> pretty t
  pure x

