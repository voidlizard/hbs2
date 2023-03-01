{-# Language TemplateHaskell #-}
{-# Language PatternSynonyms #-}
{-# Language AllowAmbiguousTypes #-}
{-# Language UndecidableInstances #-}
module HBS2Git.App where

import HBS2.Prelude
import HBS2.System.Logger.Simple

import HBS2Git.Config as Config

import Data.Config.Suckless

import Data.Set (Set)
import Data.Set  qualified as Set
import System.FilePath
import Control.Monad.Reader
import Lens.Micro.Platform

data ConfBranch

data AppEnv =
  AppEnv
  { _appCurDir :: FilePath
  , _appGitDir :: FilePath
  , _appConf   :: [Syntax C]
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

logPrefix s = set loggerTr (s <>)

tracePrefix :: SetLoggerEntry
tracePrefix  = logPrefix "[trace] "

debugPrefix :: SetLoggerEntry
debugPrefix  = logPrefix "[debug] "

errorPrefix :: SetLoggerEntry
errorPrefix  = logPrefix "[error] "

warnPrefix :: SetLoggerEntry
warnPrefix   = logPrefix "[warn] "

noticePrefix :: SetLoggerEntry
noticePrefix = logPrefix ""

instance HasCfgKey ConfBranch (Set String) where
  key = "branch"

runApp :: MonadIO m => App m () -> m ()
runApp m = do

  setLogging @DEBUG  debugPrefix
  setLogging @ERROR  errorPrefix
  setLogging @NOTICE noticePrefix
  setLogging @TRACE  tracePrefix

  (pwd, syn) <- Config.configInit

  let env = AppEnv pwd (pwd </> ".git") syn

  runReaderT (fromApp m) env

  debug $ vcat (fmap pretty syn)

  setLoggingOff @DEBUG
  setLoggingOff @ERROR
  setLoggingOff @NOTICE
  setLoggingOff @TRACE


instance {-# OVERLAPPABLE #-} (Ord b, IsString b, HasCfgKey a (Set b)) => HasCfgValue a (Set b) where
  cfgValue ae = Set.fromList val
    where
      syn = view appConf ae
      val = [ fromString (show $ pretty e)
            | ListVal @C (Key s [LitStrVal e]) <- syn, s == key @a @(Set b)
            ]

