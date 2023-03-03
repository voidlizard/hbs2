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

class HasCfgKey a b => HasCfgValue a b where
  cfgValue :: AppEnv -> b


newtype App m a =
  App { fromApp :: ReaderT AppEnv m a }
  deriving newtype ( Applicative, Functor, Monad, MonadIO, MonadReader AppEnv )

instance {-# OVERLAPPABLE #-} (Ord b, IsString b, HasCfgKey a (Maybe b)) => HasCfgValue a (Maybe b) where
  cfgValue ae = lastMay val
    where
      syn = view appConf ae
      val = [ fromString (show $ pretty e)
            | ListVal @C (Key s [LitStrVal e]) <- syn, s == key @a @(Maybe b)
            ]

instance {-# OVERLAPPABLE #-} (Ord b, IsString b, HasCfgKey a (Set b)) => HasCfgValue a (Set b) where
  cfgValue ae = Set.fromList val
    where
      syn = view appConf ae
      val = [ fromString (show $ pretty e)
            | ListVal @C (Key s [LitStrVal e]) <- syn, s == key @a @(Set b)
            ]

