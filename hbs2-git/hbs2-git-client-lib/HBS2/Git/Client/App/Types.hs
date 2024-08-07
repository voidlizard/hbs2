{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language UndecidableInstances #-}
module HBS2.Git.Client.App.Types
  ( module HBS2.Git.Client.App.Types
  , module HBS2.Git.Client.App.Types.GitEnv
  , module HBS2.Git.Local
  , module Data.Config.Suckless
  , module Control.Monad.Catch
  ) where

import HBS2.Git.Client.Prelude hiding (info)
import HBS2.Git.Client.Progress
import HBS2.Git.Local
import HBS2.Git.Client.App.Types.GitEnv

import HBS2.Git.Data.Tx.Git
import HBS2.Git.Data.GK

import HBS2.KeyMan.Keys.Direct
import HBS2.Net.Auth.GroupKeySymm
import HBS2.Storage.Operations.ByteString
import HBS2.System.Dir

import Data.Config.Suckless
import Control.Monad.Catch (MonadThrow(..))
import DBPipe.SQLite
import Data.HashMap.Strict qualified as HM
import Data.Maybe
import Data.Word

type Epoch = Word64

data GitOption = GitTrace
               | GitDebug
               | GitExport ExportType
               | GitEnc ExportEncryption
               | GitDontApplyHeads
               deriving stock (Eq,Ord)



newtype GitCLI m a = GitCLI { fromGitCLI :: ReaderT GitEnv m a }
                     deriving newtype ( Applicative
                                      , Functor
                                      , Monad
                                      , MonadIO
                                      , MonadUnliftIO
                                      , MonadTrans
                                      , MonadReader GitEnv
                                      , MonadThrow
                                      )

-- type GitPerks m = ( MonadUnliftIO m, MonadThrow m )
type GitPerks m = ( MonadUnliftIO m )

instance Monad m => HasProgressIndicator (GitCLI m) where
  getProgressIndicator = asks _progress

instance Monad m => HasStorage (GitCLI m) where
  getStorage = asks _storage

instance Monad m => HasAPI PeerAPI UNIX (GitCLI m) where
  getAPI = asks _peerAPI

instance Monad m => HasAPI LWWRefAPI UNIX (GitCLI m) where
  getAPI = asks _lwwRefAPI

instance Monad m => HasAPI RefLogAPI UNIX (GitCLI m) where
  getAPI = asks _refLogAPI

instance MonadReader GitEnv m => HasAPI RefLogAPI UNIX (ExceptT e m) where
  getAPI = asks _refLogAPI

instance MonadReader GitEnv m => HasAPI LWWRefAPI UNIX (ExceptT e m) where
  getAPI = asks _lwwRefAPI

instance MonadReader GitEnv m => HasAPI PeerAPI UNIX (ExceptT e m) where
  getAPI = asks _peerAPI

newGitEnv :: GitPerks m
          => AnyProgress
          -> [GitOption]
          -> FilePath
          -> FilePath
          -> Config
          -> ServiceCaller PeerAPI UNIX
          -> ServiceCaller RefLogAPI UNIX
          -> ServiceCaller RefChanAPI UNIX
          -> ServiceCaller LWWRefAPI UNIX
          -> ServiceCaller StorageAPI UNIX
          -> m GitEnv

newGitEnv p opts path cpath conf peer reflog rchan lww sto = do
  let dbfile  = cpath </> "state.db"
  let dOpt = dbPipeOptsDef { dbLogger = \x -> debug ("state:" <+> pretty x) }
  db <- newDBPipeEnv dOpt dbfile
  cache <- newTVarIO mempty
  pure $ GitEnv
           traceOpt
           debugOpt
           applyHeadsOpt
           exportType
           exportEnc
           path
           cpath
           conf
           peer
           reflog
           rchan
           lww
           (AnyStorage (StorageClient sto))
           db
           p
           cache
  where
    traceOpt = GitTrace `elem` opts
    debugOpt = GitDebug `elem` opts
    applyHeadsOpt = GitDontApplyHeads `notElem` opts
    -- FIXME: from-options
    exportType = lastDef ExportInc [ t | GitExport t <- opts ]
    exportEnc  = lastDef ExportPublic [ t | GitEnc t <- opts ]

withGitEnv :: GitPerks m => GitEnv -> GitCLI m a ->  m a
withGitEnv env m = runReaderT (fromGitCLI m) env

instance (GitPerks m, MonadReader GitEnv m) => GroupKeyOperations m where

  -- FIXME: may-be-faster
  loadKeyrings gkh = do

    sto <- asks _storage
    cache <- asks _keyringCache

    let k = gkh

    ke <- readTVarIO cache <&> HM.lookup k

    case ke of
      Just es -> pure es
      Nothing -> do

        rcpt <- fromMaybe mempty <$> runMaybeT do
                  runExceptT (readGK0 sto gkh)
                      >>= toMPlus
                      <&> HM.keys . recipients

        es <- runKeymanClient $ do
          loadKeyRingEntries rcpt
            <&> fmap snd

        atomically $ modifyTVar cache (HM.insert k es)
        pure es

  openGroupKey gk = runMaybeT do
    ke' <- lift $ runKeymanClient do
            loadKeyRingEntries (HM.keys $ recipients gk)
             <&> headMay

    (_, KeyringEntry{..}) <- toMPlus ke'

    toMPlus $ lookupGroupKey _krSk _krPk gk

class HasGitOpts m where
  debugEnabled :: m Bool
  traceEnabled :: m Bool

instance MonadReader GitEnv m => HasGitOpts m where
  debugEnabled = asks _gitDebugEnabled
  traceEnabled = asks _gitTraceEnabled

