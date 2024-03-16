module HBS2.Git.Client.Import where

import HBS2.Git.Client.Prelude hiding (info)
import HBS2.Git.Client.App.Types
import HBS2.Git.Client.State
import HBS2.Git.Client.RefLog
import HBS2.Git.Client.Progress

import HBS2.Git.Data.RefLog
import HBS2.Git.Data.Tx
import HBS2.Git.Data.LWWBlock

import Data.ByteString.Lazy qualified as LBS

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Text.InterpolatedString.Perl6 (qc)
import Streaming.Prelude qualified as S
import System.IO (hPrint)
import Data.Maybe

data ImportRefLogNotFound = ImportRefLogNotFound
                            deriving stock (Typeable,Show)

instance Exception ImportRefLogNotFound


data ImportTxApplyError = ImportTxApplyError HashRef
                          deriving stock (Typeable,Show)


instance Exception ImportTxApplyError


data ImportTxError =
    ImportTxReadError HashRef
  | ImportOpError OperationError
  | ImportUnbundleError HashRef
  | ImportMissed HashRef
  deriving stock (Typeable)

instance Show ImportTxError where
  show (ImportTxReadError h) = [qc|ImportTxError {pretty h}|]
  show (ImportOpError o) = show o
  show (ImportUnbundleError h) = [qc|ImportUnbundleError {pretty h}|]
  show (ImportMissed h) = [qc|ImportMissed {pretty h}|]

instance Exception ImportTxError

data IState =
    IWaitLWWBlock Int
  | IWaitRefLog Int RefLogId
  | IScanRefLog RefLogId HashRef
  | IApplyTx HashRef
  | IExit


-- class

merelySubscribeRepo :: forall e s m . ( GitPerks m
                                      , HasStorage m
                                      , HasProgressIndicator m
                                      , HasAPI PeerAPI UNIX m
                                      , HasAPI LWWRefAPI UNIX m
                                      , HasAPI RefLogAPI UNIX m
                                      , e ~ L4Proto
                                      , s ~ Encryption e
                                      )
                    => LWWRefKey HBS2Basic
                    -> m (Maybe (PubKey 'Sign s))
merelySubscribeRepo lwwKey = do

  ip  <- getProgressIndicator
  sto <- getStorage

  subscribeLWWRef lwwKey
  fetchLWWRef lwwKey

  r <- flip fix (IWaitLWWBlock 10) $ \next -> \case

    IWaitLWWBlock w | w <= 0 -> do
      throwIO ImportRefLogNotFound

    IWaitLWWBlock w -> do
      onProgress ip (ImportWaitLWW w lwwKey)
      lww <- readLWWBlock sto lwwKey

      case lww of
        Nothing -> do
          pause @'Seconds 2
          fetchLWWRef lwwKey
          next (IWaitLWWBlock (pred w))

        Just (_, LWWBlockData{..}) -> do
          void $ try @_ @SomeException (getRefLogMerkle lwwRefLogPubKey)
          subscribeRefLog lwwRefLogPubKey
          pause @'Seconds 0.25
          pure $ Just lwwRefLogPubKey

    _ -> pure Nothing

  onProgress ip ImportAllDone
  pure r

importRepoWait :: ( GitPerks m
                  , MonadReader GitEnv m
                  , HasAPI PeerAPI UNIX m
                  , HasAPI LWWRefAPI UNIX m
                  , HasAPI RefLogAPI UNIX m
                  )
               => LWWRefKey HBS2Basic
               -> m ()

importRepoWait lwwKey = do

  env <- ask

  ip  <- asks _progress
  sto <- asks _storage

  meet <- newTVarIO (mempty :: HashMap HashRef Int)

  subscribeLWWRef lwwKey

  fetchLWWRef lwwKey

  flip fix (IWaitLWWBlock 20) $ \next -> \case

    IWaitLWWBlock w | w <= 0 -> do
      throwIO ImportRefLogNotFound

    IWaitLWWBlock w -> do
      onProgress ip (ImportWaitLWW w lwwKey)
      lww <- readLWWBlock sto lwwKey

      case lww of
        Nothing -> do
          pause @'Seconds 2
          fetchLWWRef lwwKey
          next (IWaitLWWBlock (pred w))

        Just (LWWRef{..}, LWWBlockData{..}) -> do

          withState do
            insertLww lwwKey lwwSeq lwwRefLogPubKey

          void $ try @_ @SomeException (getRefLogMerkle lwwRefLogPubKey)
          subscribeRefLog lwwRefLogPubKey
          pause @'Seconds 0.25
          getRefLogMerkle lwwRefLogPubKey
          next (IWaitRefLog 20 lwwRefLogPubKey)

    IWaitRefLog w puk | w <= 0 -> do
      throwIO ImportRefLogNotFound

    IWaitRefLog w puk -> do
      onProgress ip (ImportRefLogStart puk)
      try @_ @SomeException (getRefLogMerkle puk) >>= \case
        Left _ -> do
          onProgress ip (ImportRefLogDone puk Nothing)
          pause @'Seconds 2
          next (IWaitRefLog (pred w) puk)

        Right Nothing -> do
          onProgress ip (ImportRefLogDone puk Nothing)
          pause @'Seconds 2
          next (IWaitRefLog (pred w) puk)

        Right (Just h) -> do
          onProgress ip (ImportRefLogDone puk (Just h))
          next (IScanRefLog puk h)

    IScanRefLog puk h -> do
      scanRefLog puk h
      withState (selectMaxSeqTxNotDone puk) >>= \case
        Just tx -> next (IApplyTx tx)
        Nothing -> do
          hasAnyTx <- withState existsAnyTxDone

          if hasAnyTx then -- existing repo, is' a fetch
             next IExit
          else do
            void $ race (pause @'Seconds 10) do
                    forever do
                      onProgress ip (ImportWaitTx h)
                      pause @'Seconds 0.25

            next (IScanRefLog puk h)

    IApplyTx h -> do
      onProgress ip (ImportApplyTx h)

      r <- runExceptT (applyTx h)
             `catch` \case
               ImportUnbundleError{}  -> pure (Left IncompleteData)
               _ -> throwIO (userError "tx apply / state read error")


      case r of

        Left MissedBlockError -> do
          next =<< repeatOrExit

        Left IncompleteData -> do
          atomically $ modifyTVar meet (HM.insertWith (+) h 1)
          onProgress ip (ImportApplyTxError h (Just "read/decrypt"))
          attempts <- readTVarIO meet <&> fromMaybe 0 . HM.lookup h

          when (attempts >= 10 ) do
            throwIO (ImportTxApplyError h)

          next =<< repeatOrExit

        Left e  -> do
          err (line <> red (viaShow e))
          throwIO (userError "tx apply / state read error")

        Right{} -> next IExit

    IExit -> do
      onProgress ip (ImportSetQuiet True)
      onProgress ip ImportAllDone


  where
    repeatOrExit = do
      hasAnyTx <- withState existsAnyTxDone
      if hasAnyTx then do
        pure IExit
      else do
        pause @'Seconds 2
        pure (IWaitLWWBlock 5)

scanRefLog :: (GitPerks m, MonadReader GitEnv m)
           => RefLogId
           -> HashRef
           -> m ()

scanRefLog puk rv = do
  sto <- asks _storage
  ip   <- asks _progress
  env  <- ask

  txs <- S.toList_ $ do
            walkMerkle @[HashRef] (fromHashRef rv) (getBlock sto) $ \case
              Left he   -> do
                err $ red "missed block" <+> pretty he

              Right hxs -> do
                for_ hxs $ \htx -> do
                  here <- lift (withState (existsTx htx))
                  unless here (S.yield htx)

  tx <- liftIO $ S.toList_ $ do
           for_ txs $ \tx -> do
             onProgress ip (ImportScanTx tx)
             runExceptT (readTx sto tx <&> (tx,))
               >>= either (const none) S.yield

  withState $ transactional do
    for_ tx $ \(th,(n,rhh,rh,bundleh)) -> do
      -- notice $ red "TX" <+> pretty th <+> pretty n
      insertTx puk th n rhh bundleh


applyTx :: (GitPerks m, MonadReader GitEnv m, MonadError OperationError m)
        => HashRef
        -> m ()

applyTx h = do
  sto <- asks _storage
  (n,rhh,r,bunh) <- readTx sto h

  bundles <- readBundleRefs sto bunh
                >>= orThrowError IncompleteData

  trace $ red "applyTx" <+> pretty h <+> pretty h <+> pretty bundles

  withState $ transactional do

    applyBundles r bundles

    app <- lift $ asks (view gitApplyHeads)

    when app do
      lift $ applyHeads r

    insertTxDone h

  where

    applyHeads rh = do

      let refs = _repoHeadRefs rh

      withGitFastImport $ \ps -> do
        let psin = getStdin ps

        for_ refs $ \(r,v) -> do
          unless (r == GitRef "HEAD") do
            liftIO $ hPrint psin $
              "reset" <+> pretty r <> line <> "from" <+> pretty v <> line

        hClose psin
        code <- waitExitCode ps

        trace $ red "git fast-import status" <+> viaShow code
        pure ()

    applyBundles r bundles = do
      env <- lift ask
      sto <- lift $ asks _storage
      ip  <- lift $ asks _progress

      -- withState $ do
      for_ (zip [0..] bundles) $ \(n,bu) -> do

        insertTxBundle h n bu

        here <- existsBundleDone bu

        unless here do

          BundleWithMeta meta bytes <- lift (runExceptT $ readBundle sto r bu)
                                          >>=  orThrow (ImportUnbundleError bu)

          (_,_,idx,lbs) <- unpackPackMay bytes
                             &  orThrow (ImportUnbundleError bu)

          trace $ red "reading bundle" <+> pretty bu -- <+> pretty (LBS.length lbs)

          for_ idx $ \i -> do
            insertBundleObject bu i

          let chunks = LBS.toChunks lbs

          void $ liftIO $ withGitEnv env $ withGitUnpack $ \p -> do
             let pstdin = getStdin p
             for_ (zip [1..] chunks) $ \(i,chu) -> do
               onProgress ip (ImportReadBundleChunk meta (Progress i Nothing))
               liftIO $ LBS.hPutStr pstdin (LBS.fromStrict chu)

             hFlush pstdin >> hClose pstdin

             code <- waitExitCode p

             trace $ "unpack objects done:" <+> viaShow code

          insertBundleDone bu


withGitFastImport :: (MonadUnliftIO m, MonadReader GitEnv m)
                  => (Process Handle Handle () -> m a)
                  -> m ()
withGitFastImport action = do
  fp <- asks _gitPath
  let cmd = "git"
  let args = ["--git-dir", fp, "fast-import"]
  -- let config = setStdin createPipe $ setStdout createPipe $ setStderr closed $ proc cmd args

  trc <- asks traceEnabled >>= \case
                True -> pure id
                False -> pure $ setStdout closed . setStderr closed

  let pconfig = setStdin createPipe $ setStdout createPipe $ trc $ proc cmd args
  p <- startProcess pconfig
  void $ action p
  stopProcess p

withGitUnpack :: (MonadUnliftIO m, MonadReader GitEnv m)
               =>  (Process Handle Handle () -> m a) -> m a
withGitUnpack action = do
  fp <- asks _gitPath
  let cmd = "git"
  let args = ["--git-dir", fp, "unpack-objects", "-q"]

  trc <- asks traceEnabled >>= \case
                True -> pure id
                False -> pure $ setStdout closed . setStderr closed

  let pconfig = setStdin createPipe $ setStdout createPipe $ trc $ proc cmd args
  p <- startProcess pconfig
  action p


gitPrune :: (MonadUnliftIO m, MonadReader GitEnv m)
         => m ()
gitPrune = do
 fp <- asks _gitPath
 let cmd = [qc|git --git-dir={fp} prune|]
 runProcess_ (shell cmd & setStderr closed & setStdout closed)
 pure ()


