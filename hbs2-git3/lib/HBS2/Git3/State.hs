{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language UndecidableInstances #-}
{-# Language AllowAmbiguousTypes #-}
module HBS2.Git3.State
  ( module HBS2.Git3.State
  , module Exported
  ) where

import HBS2.Git3.Prelude

import HBS2.Merkle
import HBS2.Git3.State.Internal.Types as Exported
import HBS2.Git3.State.Internal.LWWBlock as Exported
import HBS2.Git3.State.Internal.RefLog as Exported
import HBS2.Git3.State.Internal.Segment as Exported
import HBS2.Git3.State.Internal.Index as Exported

import HBS2.Storage.Operations.Missed
import HBS2.Net.Auth.Credentials
import HBS2.KeyMan.Keys.Direct
import HBS2.Data.Detect
import HBS2.CLI.Run.MetaData (getTreeContents)

import Data.Config.Suckless

import HBS2.Peer.RPC.API.Storage
import HBS2.Peer.RPC.Client.StorageClient
import HBS2.System.Dir
import HBS2.Peer.CLI.Detect

import Data.ByteString.Lazy qualified as LBS
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.Kind
import Data.Maybe
import Data.List qualified as L
import Data.Text.Encoding.Error qualified as TE
import Data.Text.Encoding qualified as TE
import Data.Word
import Lens.Micro.Platform

import Codec.Compression.Zstd (maxCLevel)


newtype RepoManifest = RepoManifest [Syntax C]


getRefLog :: RepoManifest -> Maybe GitRemoteKey
getRefLog mf = lastMay [ x
                       | ListVal [SymbolVal "reflog", SignPubKeyLike x]  <- coerce mf
                       ]

updateRepoKey :: forall m . HBS2GitPerks m => GitRepoKey -> Git3 m ()
updateRepoKey key = do

  notice $ "updateRepoKey" <+> pretty (AsBase58 key)

  setGitRepoKey key

  reflog <- getRepoManifest <&> getRefLog

  ask >>= \case
        Git3Connected{..} -> do
          notice $ yellow "UPDATED REFLOG" <+> pretty (fmap AsBase58 reflog)
          atomically $ writeTVar gitRefLog reflog

        _ -> none

getRepoRefMaybe :: forall m . HBS2GitPerks m => Git3 m (Maybe (LWWRef 'HBS2Basic))
getRepoRefMaybe = do
  lwwAPI  <- getClientAPI @LWWRefAPI @UNIX

  pk <- getGitRepoKey >>= orThrow GitRepoRefNotSet

  callRpcWaitMay @RpcLWWRefGet (TimeoutSec 1) lwwAPI (LWWRefKey pk)
    >>= orThrow RpcTimeout

getRepoRefLogCredentials :: forall m . HBS2GitPerks m
                         => Git3 m (PubKey 'Sign 'HBS2Basic, PrivKey 'Sign HBS2Basic)

getRepoRefLogCredentials = do
  -- FIXME: memoize-this
  mf <- getRepoManifest
  rk <- getGitRepoKey >>= orThrow GitRepoRefNotSet

  reflog <- getGitRemoteKey >>= orThrow Git3ReflogNotSet

  creds <- runKeymanClientRO (loadCredentials rk)
             >>= orThrowUser ("not found credentials for"  <+> pretty (AsBase58 rk))

  seed <- [ x | ListVal [SymbolVal "seed", LitIntVal x ] <- coerce mf ]
             & lastMay & orThrow GitRepoManifestMalformed
             <&> fromIntegral @_ @Word64

  let sk = view peerSignSk creds

  (p,s) <- derivedKey @'HBS2Basic @'Sign seed sk

  unless ( p == reflog ) do
    throwIO RefLogCredentialsNotMatched

  pure (p,s)

getRepoManifest :: forall m . HBS2GitPerks m => Git3 m RepoManifest
getRepoManifest = do

  sto <- getStorage

  LWWRef{..} <- getRepoRefMaybe >>= orThrow GitRepoRefEmpty

  mfref <- readLogThrow (getBlock sto) lwwValue
               <&> headMay
               >>= orThrow GitRepoManifestMalformed

  runExceptT (getTreeContents sto mfref)
     >>= orThrowPassIO
     <&> TE.decodeUtf8With TE.lenientDecode . LBS.toStrict
     <&> parseTop
     >>= orThrow GitRepoManifestMalformed
     <&> RepoManifest

nullGit3Env :: MonadIO m => m Git3Env
nullGit3Env = Git3Disconnected
                <$> newTVarIO defSegmentSize
                <*> newTVarIO defCompressionLevel
                <*> newTVarIO defIndexBlockSize
                <*> newTVarIO Nothing

connectedDo :: (MonadIO m) => Git3 m a -> Git3 m a
connectedDo what = do
  env <- ask
  debug $ red "connectedDo"
  case env of
    Git3Disconnected{} -> do
      throwIO Git3PeerNotConnected

    _ -> what

withGit3Env :: Git3Perks m => Git3Env -> Git3 m a -> m a
withGit3Env env a = runReaderT (fromGit3 a) env

runGit3 :: Git3Perks m => Git3Env -> Git3 m b -> m b
runGit3 env action = withGit3Env env action

withStateDo :: MonadUnliftIO m => Git3 m a -> Git3 m a
withStateDo action = do
  waitRepo Nothing
  getStatePathM >>= mkdir
  action

recover :: Git3 IO a -> Git3 IO a
recover m = fix \again -> do
  catch m $ \case
    Git3PeerNotConnected -> do

      soname <- detectRPC
                  `orDie` "can't locate hbs2-peer rpc"

      flip runContT pure do

        client <- lift $ race (pause @'Seconds 1) (newMessagingUnix False 1.0 soname)
                    >>= orThrowUser ("can't connect to" <+> pretty soname)

        void $ ContT $ withAsync $ runMessagingUnix client

        peer       <- makeServiceCaller @PeerAPI (fromString soname)
        refLogAPI  <- makeServiceCaller @RefLogAPI (fromString soname)
        storageAPI <- makeServiceCaller @StorageAPI (fromString soname)
        lwwAPI     <- makeServiceCaller @LWWRefAPI (fromString soname)

        -- let sto = AnyStorage (StorageClient storageAPI)

        let endpoints = [ Endpoint @UNIX  peer
                        , Endpoint @UNIX  refLogAPI
                        , Endpoint @UNIX  lwwAPI
                        , Endpoint @UNIX  storageAPI
                        ]

        void $ ContT $ withAsync $ liftIO $ runReaderT (runServiceClientMulti endpoints) client

        let sto = AnyStorage (StorageClient storageAPI)

        rk <- lift $ getGitRepoKey

        -- debug $ yellow $ "REPOKEY" <+> pretty (AsBase58 rk)

        connected <- Git3Connected soname sto peer refLogAPI lwwAPI
                        <$> newTVarIO rk
                        <*> newTVarIO Nothing
                        <*> newTVarIO Nothing
                        <*> newTVarIO defSegmentSize
                        <*> newTVarIO defCompressionLevel
                        <*> newTVarIO defIndexBlockSize


        liftIO $ withGit3Env connected do
          -- updateRepoKey rk
          -- ref <- getGitRemoteKey >>= orThrow GitRepoManifestMalformed
          -- state <- getStatePathM
          -- mkdir state
          again

    e -> throwIO e


data ReflogWaitTimeout =
  ReflogWaitTimeout
  deriving stock (Show,Typeable)

instance Exception ReflogWaitTimeout


data CWRepo =
    CWaitLWW
  | CCheckManifest (LWWRef HBS2Basic)
  | CAborted


waitRepo :: forall m . HBS2GitPerks m => Maybe (Timeout 'Seconds) -> Git3 m ()
waitRepo timeout = do
  repoKey <- getGitRepoKey >>= orThrow GitRepoRefNotSet

  notice $ yellow "waitRepo"

  ask >>= \case
    Git3Disconnected{} -> throwIO Git3PeerNotConnected
    Git3Connected{..} -> do

      sto <- getStorage

      flip runContT pure $ callCC \done -> do

        rlv <- readTVarIO gitRefLogVal <&> isJust
        rlog <- readTVarIO gitRefLog <&> isJust

        when (rlv && rlog) $ done ()

        reflog_ <- newEmptyTMVarIO

        let wait w what x = pause @'Seconds w >> what x

        callCC \forPeer -> do

          notice "wait for peer"

          lift (callRpcWaitMay @RpcPollAdd (TimeoutSec 1) peerAPI (repoKey, "lwwref", 31))
                   >>= maybe (wait 1 forPeer ()) (const none)

        pFetch <- ContT $ withAsync $ forever do
                    void  (callRpcWaitMay @RpcLWWRefFetch (TimeoutSec 1) lwwAPI (LWWRefKey repoKey))
                    pause @'Seconds 10

        pFetchRefLog <- ContT $ withAsync do
                         r <- atomically $ takeTMVar reflog_
                         forever do
                           void  (callRpcWaitMay @RpcRefLogFetch (TimeoutSec 1) reflogAPI r)
                           pause @'Seconds 10

        lww <- flip fix () \next _ -> do
                  notice $ "wait for" <+> pretty (AsBase58 repoKey)
                  lift (callRpcWaitMay @RpcLWWRefGet (TimeoutSec 1) lwwAPI (LWWRefKey repoKey))
                    >>= \case
                           Just (Just x)  -> pure x
                           _              -> wait 2 next ()

        notice $ "lwwref value" <+> pretty (lwwValue lww)

        mf <- flip fix () $ \next _ -> do
                  notice $ "wait for manifest"
                  lift (try @_ @WalkMerkleError getRepoManifest) >>= \case
                    Left{}  -> wait 1 next ()
                    Right x -> pure x

        reflog <- getRefLog mf & orThrow GitRepoManifestMalformed


        atomically $ writeTMVar reflog_ reflog

        lift (callRpcWaitMay @RpcPollAdd (TimeoutSec 1) peerAPI (reflog, "reflog", 11))
                 >>= orThrow RpcTimeout

        rv <- flip fix () \next _ -> do
                  notice $ "wait for data" <+> pretty (AsBase58 reflog)
                  lift (callRpcWaitMay @RpcRefLogGet (TimeoutSec 1) reflogAPI reflog)
                    >>= \case
                           Just (Just x)  -> pure x
                           _              -> wait 2 next ()

        atomically $ writeTVar gitRefLogVal (Just rv)

        okay <- newEmptyTMVarIO

        flip fix () $ \next _ -> do
           notice $ "wait for data (2)" <+> pretty (AsBase58 reflog)
           -- missed <- findMissedBlocks sto rv
           missed_ <- newTVarIO 0
           lift $ deepScan ScanDeep (\_ -> atomically $ modifyTVar missed_ succ) (coerce rv) (getBlock sto) (const none)
           missed <- readTVarIO missed_

           when (missed > 0) do
            notice $ "still missed blocks:" <+> pretty missed
            wait 5 next ()

           atomically $ writeTMVar okay True

        pWait <- ContT $ withAsync $ race ( pause (fromMaybe 300 timeout) ) do
                   void $ atomically $ takeTMVar okay

        waitAnyCatchCancel [pWait, pFetch, pFetchRefLog]

        lift $ updateRepoKey repoKey

        debug $ "reflog" <+> pretty (AsBase58 reflog) <+> pretty rv

