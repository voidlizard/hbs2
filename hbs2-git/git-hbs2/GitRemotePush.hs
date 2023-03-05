module GitRemotePush where

import HBS2.Prelude.Plated
import HBS2.Data.Types.Refs
import HBS2.OrDie
import HBS2.System.Logger.Simple
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

newtype RunWithConfig m a =
  WithConfig { fromWithConf :: ReaderT [Syntax C] m a }
  deriving newtype ( Applicative
                   , Functor
                   , Monad
                   , MonadIO
                   , MonadReader [Syntax C]
                   , MonadTrans
                   )

runWithConfig :: MonadIO m => [Syntax C] -> RunWithConfig m a -> m a
runWithConfig conf m = runReaderT (fromWithConf m) conf

instance MonadIO m => HasConf (RunWithConfig (GitRemoteApp m)) where
  getConf = ask

instance MonadIO m => HasCatAPI (RunWithConfig (GitRemoteApp m)) where
  getHttpCatAPI = lift getHttpCatAPI

push :: forall m . ( MonadIO m
                   )

     => HashRef -> [Maybe GitRef] -> GitRemoteApp m ()

push remote [bFrom , Just br] = do

  -- TODO: 1. get-current-head
  -- TODO: 2. update-branch
  -- TODO: 3. make-new-head
  -- TODO: 4. update-state
  -- TODO: 5. update-ref
  -- TODO: 6. notify-ref-updated

  (_, syn) <- Config.configInit

  dbPath <- makeDbPath remote
  db <- dbEnv dbPath

  runWithConfig syn do

    brCfg <- cfgValue @ConfBranch  @(Set GitRef) <&> transformBi normalizeRef

    oldHead <- readHead db <&> fromMaybe mempty

    newHead <- case bFrom of
                Just newBr -> do
                   pure oldHead
                   -- gh <- gitGetHash newBr `orDie` [qc|can't read hash for ref {pretty newBr}|]
                   -- pure $ over repoHeads (HashMap.insert br gh) oldHead

                Nothing -> do
                  warn $ "about to delete branch" <+> pretty br <+> pretty "in" <+> pretty remote

                  when ( br `Set.member` brCfg ) do
                    err $ "remove" <+> pretty br <+> "from config first"
                    exitFailure

                  pure $ over repoHeads (HashMap.delete br) oldHead

    trace $ "new head is" <+> pretty (AsGitRefsFile newHead)

    (root, hh) <- export remote newHead

    info  $ "head:" <+> pretty hh
    info  $ "merkle:" <+> pretty root

push r w = warn $ "ignoring weird push" <+> pretty w <+> pretty r

