module HBS2.Sync.Prelude
  ( module HBS2.Sync.Prelude
  , module Exported
  ) where


import HBS2.Prelude.Plated as Exported
import HBS2.Base58
import HBS2.OrDie as Exported
import HBS2.Data.Types.Refs as Exported
import HBS2.Clock as Exported
import HBS2.Net.Proto.Service
import HBS2.Peer.CLI.Detect
import HBS2.Peer.RPC.Client
import HBS2.Peer.RPC.Client.Unix
import HBS2.Peer.RPC.API.Peer
import HBS2.Peer.RPC.API.RefChan
import HBS2.Peer.RPC.API.RefLog
import HBS2.Peer.RPC.API.Storage
import HBS2.System.Logger.Simple.ANSI as Exported
import HBS2.Misc.PrettyStuff as Exported

import HBS2.CLI.Run hiding (PeerException(..))

import Data.Config.Suckless as Exported
import Data.Config.Suckless.Script as Exported
import Data.Config.Suckless.Script.File

import Codec.Serialise as Exported
import Control.Concurrent.STM (flushTQueue)
import Control.Monad.Reader as Exported
import Control.Monad.Trans.Cont as Exported
import Data.Either
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import System.FilePath

import UnliftIO

{- HLINT ignore "Functor law" -}

data SyncEnv =
  SyncEnv
  { rechanAPI  :: ServiceCaller RefChanAPI UNIX
  , storageAPI :: ServiceCaller StorageAPI UNIX
  , peerAPI    :: ServiceCaller PeerAPI UNIX
  }

newtype SyncApp m a =
  SyncApp { fromSyncApp :: ReaderT (Maybe SyncEnv) m a }
  deriving newtype ( Applicative
                   , Functor
                   , Monad
                   , MonadUnliftIO
                   , MonadIO
                   , MonadReader (Maybe SyncEnv))


type SyncAppPerks m = MonadUnliftIO m

withSyncApp :: SyncAppPerks m => Maybe SyncEnv -> SyncApp m a -> m a
withSyncApp env action = runReaderT (fromSyncApp action) env

runSyncApp :: SyncAppPerks m => SyncApp m a -> m a
runSyncApp m = do
  setupLogger
  withSyncApp Nothing m `finally` flushLoggers

recover :: SyncApp IO a -> SyncApp IO a
recover what = do
  catch what $ \case
    PeerNotConnectedException -> do

      soname <- detectRPC
                  `orDie` "can't locate hbs2-peer rpc"

      flip runContT pure do

        client <- lift $ race (pause @'Seconds 1) (newMessagingUnix False 1.0 soname)
                    >>= orThrowUser ("can't connect to" <+> pretty soname)

        void $ ContT $ withAsync $ runMessagingUnix client

        peerAPI    <- makeServiceCaller @PeerAPI (fromString soname)
        refChanAPI <- makeServiceCaller @RefChanAPI (fromString soname)
        storageAPI <- makeServiceCaller @StorageAPI (fromString soname)

        -- let sto = AnyStorage (StorageClient storageAPI)

        let endpoints = [ Endpoint @UNIX  peerAPI
                        , Endpoint @UNIX  refChanAPI
                        , Endpoint @UNIX  storageAPI
                        ]

        void $ ContT $ withAsync $ liftIO $ runReaderT (runServiceClientMulti endpoints) client

        let env = Just (SyncEnv refChanAPI storageAPI peerAPI)

        liftIO $ withSyncApp env what

data PeerException =
  PeerNotConnectedException
  deriving stock (Show, Typeable)

instance Exception PeerException

data RunDirectoryException =
  RefChanNotSetException
  deriving stock (Show,Typeable)

instance Exception RunDirectoryException

runDirectory :: ( IsContext c
                , SyncAppPerks m
                , Exception (BadFormException c)
                ) => FilePath -> RunM c m ()
runDirectory path = do

  t <- ask
  d0 <- readTVarIO t

  runDir
    `catch` \case
      RefChanNotSetException -> do
        warn $ "no refchan set for" <+> pretty path
    `finally` do
      warn "exiting"
      atomically (writeTVar t d0)

  where

    runDir = do

      notice $ yellow "run directory" <+> pretty path

      trc    <- newTVarIO Nothing
      texcl  <- newTQueueIO
      tincl  <- newTQueueIO

      atomically $ writeTQueue tincl "**"

      ins <- liftIO $ readFile (path </> ".hbs2-sync/config")
               <&> parseTop
               <&> either mempty (fmap fixContext)

      bindBuiltins $ bindMatch "refchan" $ nil_ $ \case
        [SignPubKeyLike puk] -> do
          debug $ red "USE FUCKING REFCHAN!" <+> pretty (AsBase58 puk)
          atomically $ writeTVar trc (Just puk)

        _ -> pure ()

      bindBuiltins $ bindMatch "exclude" $ nil_ $ \case
        [StringLike excl] -> do
          debug $ red "EXCLUDE!" <+> pretty excl
          atomically $ writeTQueue texcl excl

        _ -> pure ()

      bindBuiltins $ bindMatch "include" $ nil_ $ \case
        [StringLike s] -> do
          debug $ red "INCLUDE!" <+> pretty s
          atomically $ writeTQueue tincl s

        _ -> pure ()

      evalTop ins

      i <- atomically (flushTQueue tincl) <&> HS.fromList <&> HS.toList
      e <- atomically (flushTQueue texcl) <&> HS.fromList <&> HS.toList

      rc <- readTVarIO trc
             >>= orThrow RefChanNotSetException

      debug $ "step 1"   <+> "load state from refchan"
      debug $ "step 2"   <+> "create local state"
      debug $ "step 3"   <+> "merge states"
      debug $ "step 3.1" <+> "generate merge actions"
      debug $ "step 3.2" <+> "apply actions"

      glob i e path  $ \fn -> do
        pure True

      debug $ pretty ins

syncEntries :: forall c m . (MonadUnliftIO m, IsContext c) => MakeDictM c m ()
syncEntries = do

  entry $ bindMatch "--debug" $ nil_ $ \case
    [SymbolVal "off"] -> do
      setLoggingOff @DEBUG

    _ -> do
      setLogging @DEBUG  debugPrefix

debugPrefix = toStderr . logPrefix "[debug] "

setupLogger :: MonadIO m => m ()
setupLogger = do
  -- setLogging @DEBUG  $ toStderr . logPrefix "[debug] "
  setLogging @ERROR  $ toStderr . logPrefix "[error] "
  setLogging @WARN   $ toStderr . logPrefix "[warn] "
  setLogging @NOTICE $ toStdout . logPrefix ""
  pure ()

flushLoggers :: MonadIO m => m ()
flushLoggers = do
  silence

silence :: MonadIO m => m ()
silence = do
  setLoggingOff @DEBUG
  setLoggingOff @ERROR
  setLoggingOff @WARN
  setLoggingOff @NOTICE

