{-# Language PatternSynonyms #-}
{-# Language UndecidableInstances #-}
{-# Language TemplateHaskell #-}
{-# Language AllowAmbiguousTypes #-}
module HBS2Git.Types
  ( module HBS2Git.Types
  , module Control.Monad.IO.Class
  )
  where

import Data.Config.Suckless

import Control.Monad.IO.Class
import Control.Monad.Reader
import Database.SQLite.Simple (Connection)
import Data.Set qualified as Set
import Data.Set (Set)
import Data.String
import Lens.Micro.Platform
import Prettyprinter
import Network.HTTP.Simple
import Safe

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


pattern Key :: forall {c}. Id -> [Syntax c] -> [Syntax c]
pattern Key n ns <- SymbolVal  n : ns

class HasCfgKey a b where
  -- type family CfgValue a :: Type
  key :: Id

class (Monad m, HasCfgKey a b) => HasCfgValue a b m where
  cfgValue :: m b

class Monad m => HasConf m where
  getConf :: m [Syntax C]

newtype App m a =
  App { fromApp :: ReaderT AppEnv m a }
  deriving newtype ( Applicative, Functor, Monad, MonadIO, MonadReader AppEnv )

instance MonadIO m => HasConf (App m) where
  getConf = asks (view appConf)

instance {-# OVERLAPPABLE #-} (HasConf m, Ord b, IsString b, HasCfgKey a (Maybe b)) => HasCfgValue a (Maybe b) m where
  cfgValue = lastMay . val <$> getConf
    where
      val syn = [ fromString (show $ pretty e)
                | ListVal @C (Key s [LitStrVal e]) <- syn, s == key @a @(Maybe b)
                ]

instance {-# OVERLAPPABLE #-} (HasConf m, Ord b, IsString b, HasCfgKey a (Set b)) => HasCfgValue a (Set b) m where
  cfgValue  = Set.fromList . val <$> getConf
    where
      val syn = [ fromString (show $ pretty e)
                | ListVal @C (Key s [LitStrVal e]) <- syn, s == key @a @(Set b)
                ]

