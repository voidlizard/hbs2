{-# Language AllowAmbiguousTypes #-}
module GitRemotePush where

import HBS2.Prelude.Plated
import HBS2.Data.Types.Refs
import HBS2.OrDie
import HBS2.System.Logger.Simple
import HBS2.Net.Auth.Credentials hiding (getCredentials)
-- import HBS2.Merkle
-- import HBS2.Hash

import HBS2.Git.Local
import HBS2.Git.Local.CLI

import HBS2Git.Config as Config
import HBS2Git.Types
import HBS2Git.State
import HBS2Git.App
import HBS2Git.Export (export)

import GitRemoteTypes

import Data.Maybe
import Control.Monad.Reader
import Data.Functor
import Data.Set (Set)
import Data.Set qualified as Set
import Lens.Micro.Platform
import Data.HashMap.Strict qualified as HashMap
import Text.InterpolatedString.Perl6 (qc)
import Data.ByteString qualified as BS
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import Control.Monad.Catch

newtype RunWithConfig m a =
  WithConfig { fromWithConf :: ReaderT [Syntax C] m a }
  deriving newtype ( Applicative
                   , Functor
                   , Monad
                   , MonadIO
                   , MonadReader [Syntax C]
                   , MonadTrans
                   , MonadThrow
                   , MonadCatch
                   )


runWithConfig :: MonadIO m => [Syntax C] -> RunWithConfig m a -> m a
runWithConfig conf m = runReaderT (fromWithConf m) conf

instance MonadIO m => HasConf (RunWithConfig (GitRemoteApp m)) where
  getConf = ask

instance MonadIO m => HasCatAPI (RunWithConfig (GitRemoteApp m)) where
  getHttpCatAPI = lift getHttpCatAPI
  getHttpSizeAPI = lift getHttpSizeAPI
  getHttpPutAPI = lift getHttpPutAPI
  getHttpRefLogGetAPI = lift getHttpRefLogGetAPI

instance MonadIO m => HasRefCredentials (RunWithConfig (GitRemoteApp m)) where
  getCredentials = lift . getCredentials
  setCredentials r c = lift $ setCredentials r c

push :: forall  m . ( MonadIO m
                    , MonadCatch m
                    , HasProgress (RunWithConfig (GitRemoteApp m))
                    )

     => RepoRef -> [Maybe GitRef] -> GitRemoteApp m (Maybe GitRef)

push remote [bFrom , Just br] = do

  (_, syn) <- Config.configInit

  dbPath <- makeDbPath remote
  db <- dbEnv dbPath

  runWithConfig syn do

    brCfg <- cfgValue @ConfBranch  @(Set GitRef) <&> transformBi normalizeRef

    loadCredentials mempty

    oldHead <- readHead db <&> fromMaybe mempty

    newHead <- case bFrom of
                Just newBr -> do
                   gh <- gitGetHash (normalizeRef newBr) `orDie` [qc|can't read hash for ref {pretty newBr}|]
                   pure $ over repoHeads (HashMap.insert br gh) oldHead

                Nothing -> do
                  warn $ "about to delete branch" <+> pretty br <+> pretty "in" <+> pretty remote

                  when ( br `Set.member` brCfg ) do
                    err $ "remove" <+> pretty br <+> "from config first"
                    exitFailure

                  pure $ over repoHeads (HashMap.delete br) oldHead

    (root, hh) <- export remote newHead

    info  $ "head:" <+> pretty hh
    info  $ "merkle:" <+> pretty root
    pure (Just br)

push r w = do
  warn $ "ignoring weird push" <+> pretty w <+> pretty r
  pure Nothing

