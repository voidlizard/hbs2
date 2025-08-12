{-# Language MultiWayIf #-}
module HBS2.Storage.NCQ3.Internal.Run where

import HBS2.Storage.NCQ.Types hiding (FileKey)
import HBS2.Storage.NCQ3.Internal.Prelude
import HBS2.Storage.NCQ3.Internal.Types
import HBS2.Storage.NCQ3.Internal.Files
import HBS2.Storage.NCQ3.Internal.Memtable
import HBS2.Storage.NCQ3.Internal.Index
import HBS2.Storage.NCQ3.Internal.State
import HBS2.Storage.NCQ3.Internal.Sweep
import HBS2.Storage.NCQ3.Internal.MMapCache
import HBS2.Storage.NCQ3.Internal.Fossil

import Control.Monad.Trans.Cont
import Control.Monad.Trans.Maybe
import Data.HashSet qualified as HS
import Data.Vector qualified as V
import Data.Sequence qualified as Seq
import Data.Fixed
import System.Posix.IO as PosixBase
import System.Posix.Types as Posix
import System.Posix.Unistd
import System.Posix.IO.ByteString as Posix
import Control.Concurrent.STM qualified as STM
import System.FileLock as FL

ncqStorageStop :: forall m . MonadUnliftIO m => NCQStorage -> m ()
ncqStorageStop NCQStorage{..} = do
  atomically $ writeTVar ncqStopReq True

ncqStorageRun :: forall m . MonadUnliftIO m
               => NCQStorage
               -> m ()
ncqStorageRun ncq@NCQStorage{..} = flip runContT pure do
  ContT $ bracket setAlive (const unsetAlive)

  ContT $ bracket none $ const $ liftIO do
    readTVarIO ncqFileLock >>= mapM_  FL.unlockFile

  closeQ <- liftIO newTQueueIO

  closer <- spawnActivity $ liftIO $ fix \loop -> do
    what <- atomically do
      tryReadTQueue closeQ >>= \case
        Just e  -> pure $ Just e
        Nothing -> do
          stop  <- readTVar ncqStopReq
          if not stop then STM.retry else pure Nothing

    maybe1 what none $ \(fk :: FileKey, fh) -> do
      closeFd fh >> ncqIndexFile ncq (DataFile fk) >> loop

  let shLast = V.length ncqWriteOps - 1
  spawnActivity $ pooledForConcurrentlyN_ (V.length ncqWriteOps) [0..shLast] $ \i -> do
    let q = ncqWriteOps ! i
    forever (liftIO $ join $ atomically (readTQueue q))

  replicateM_ 2 $ spawnActivity $ forever $ flip runContT pure $ callCC \exit -> do

      (h, answ) <- atomically $ readTQueue ncqReadReq
      let answer l = atomically (putTMVar answ l)

      -- debug $ "REQ" <+> pretty h

      atomically (ncqLookupEntrySTM ncq h) >>= \case
        Nothing  -> none
        Just e -> answer (Just (InMemory (ncqEntryData e))) >> exit ()

      ContT $ ncqWithState ncq

      NCQState{..} <- readTVarIO ncqState

      for_ ncqStateIndex $ \(_, fk) -> do
       CachedIndex bs nw <- lift $ ncqGetCachedIndex ncq fk
       lift (ncqLookupIndex h (bs, nw)) >>= \case
        Just (IndexEntry fk o s) -> answer (Just (InFossil fk o s)) >> exit ()
        Nothing -> none

      -- debug $ "NOT FOUND SHIT" <+> pretty h
      answer Nothing >> exit ()

  spawnActivity measureWPS

  -- spawnActivity (ncqStateUpdateLoop ncq)

  spawnActivity $ forever do
    pause @'Seconds 30
    ema <- readTVarIO ncqWriteEMA
    debug $ "EMA" <+> pretty (realToFrac @_ @(Fixed E3) ema)

  spawnActivity $ postponed 10 $ forever do
    lsInit <- ncqLiveKeys ncq <&> HS.size
    void $ race (pause @'Seconds 60) do
      flip fix lsInit $ \next ls0 -> do
        (lsA,lsB) <- atomically do
          ema <- readTVar ncqWriteEMA
          ls1 <- ncqLiveKeysSTM ncq <&> HS.size

          if  ls1 /= ls0 && ema < ncqIdleThrsh then
            pure (ls0,ls1)
          else
            STM.retry

        debug $ "do sweep" <+> pretty lsA <+> pretty lsB
        ncqSweepObsoleteStates ncq
        ncqSweepFiles ncq
        next lsB

  spawnActivity $ postponed 10 $ compactLoop 10 300 do
    ncqIndexCompactStep ncq

  spawnActivity $ postponed 15 $ compactLoop 10 600 do
    ncqFossilMergeStep ncq


  flip fix RunNew $ \loop -> \case
    RunFin -> do
      debug "exit storage"
      atomically $ pollSTM closer >>= maybe STM.retry (const none)

    RunNew -> do
      alive <- readTVarIO ncqAlive
      empty <- readTVarIO ncqWriteQ <&> Seq.null
      if not alive && empty
        then loop RunFin
        else do
          (fk, fhx) <- openNewDataFile
          loop $ RunWrite (fk, fhx, 0, 0)


    RunSync (fk, fh, w, total, continue) -> do

      (stop,sync) <- atomically do
                           (,) <$> readTVar ncqStopReq
                               <*> readTVar ncqSyncReq
                               -- <*> readTVar ncqWriteEMA

      let needClose = total >= ncqMinLog || stop

      rest <- if not (sync || needClose || w > ncqFsync) then
                  pure w
                else do

                  ss <- appendTailSection fh
                  liftIO (fileSynchronise fh)

                  -- ss <- liftIO (PFS.getFdStatus fh) <&> fromIntegral . PFS.fileSize

                  ncqStateUpdate ncq do
                    ncqStateAddFact (P (PData (DataFile fk) ss))

                  atomically do
                    writeTVar  ncqSyncReq False
                    modifyTVar ncqSyncNo succ

                  pure 0

      if | needClose && continue -> do
              atomically $ writeTQueue closeQ (fk, fh)
              loop RunNew

         | not continue -> loop RunFin

         | otherwise -> loop $ RunWrite (fk, fh, rest, total)


    RunWrite (fk, fh, w, total') -> do

      let timeoutMicro = 10_000_000

      chunk <- liftIO $ timeout timeoutMicro $ atomically do
        stop  <- readTVar ncqStopReq
        sy    <- readTVar ncqSyncReq
        chunk <- stateTVar ncqWriteQ (Seq.splitAt ncqWriteBlock)

        if | Seq.null chunk && stop             -> pure $ Left ()
           | Seq.null chunk && not (stop || sy) -> STM.retry
           | otherwise                          -> pure $ Right chunk

      case chunk of
        Nothing -> do
          liftIO $ join $ readTVarIO ncqOnRunWriteIdle
          if w == 0 then do
            loop $ RunWrite (fk,fh,w,total')
          else do
            atomically $ writeTVar ncqSyncReq True
            loop $ RunSync (fk, fh, w, total', True) -- exit ()

        Just (Left{})  -> loop $ RunSync (fk, fh, w, total', False) -- exit ()

        Just (Right chu) -> do
          ws <- for chu $ \h -> do
                  atomically (ncqLookupEntrySTM ncq h) >>= \case
                    Just (NCQEntry bs w)  -> do
                      atomically (writeTVar w (Just fk))
                      lift (appendSection fh bs)

                    _ -> pure 0

          let written = sum ws
          loop $ RunSync (fk, fh, w + written, total' + written, True)

  mapM_ wait [closer]

  where
    setAlive   = atomically $ writeTVar ncqAlive True
    unsetAlive = atomically $ writeTVar ncqAlive False

    openNewDataFile :: forall mx . MonadIO mx => mx (FileKey, Fd)
    openNewDataFile = do
      fk <- ncqGetNewFileKey ncq DataFile
      let fname = ncqGetFileName ncq (DataFile fk)
      touch fname
      let flags = defaultFileFlags { exclusive = False, creat = Just 0o666 }
      (fk,) <$> liftIO (PosixBase.openFd fname  Posix.ReadWrite flags)

    spawnActivity m = do
      a <- ContT $ withAsync m
      link a
      pure a

    measureWPS = void $ flip fix Nothing \loop -> \case
      Nothing      -> do
        w <- readTVarIO ncqWrites
        t <- getTimeCoarse
        pause @'Seconds step >> loop (Just (w,t))

      Just (w0,t0) -> do
        w1 <- readTVarIO ncqWrites
        t1 <- getTimeCoarse
        let dt = max 1e-9 (realToFrac @_ @Double (t1 - t0)) / 1e9
            dw = fromIntegral (w1 - w0)
        atomically $ modifyTVar ncqWriteEMA \ema -> alpha * (dw/dt) + 0.9 * ema
        pause @'Seconds step >> loop (Just (w1,t1))

      where
        alpha = 0.1
        step  = 1.00

    postponed n m = liftIO (pause @'Seconds n) >> m

    compactLoop :: Timeout 'Seconds -> Timeout 'Seconds -> m Bool -> m ()
    compactLoop t1 t2 what = forever $ void $ runMaybeT do
      ema <- readTVarIO ncqWriteEMA

      when (ema > ncqIdleThrsh) $ pause @'Seconds t1 >> mzero

      compacted <- lift what

      when compacted mzero

      k0 <- readTVarIO ncqStateKey
      void $ lift $ race (pause @'Seconds t2) do
        flip fix k0 $ \waitState k1 -> do
          pause @'Seconds 60
          k2 <- readTVarIO ncqStateKey
          when (k2 == k1) $  waitState k2



data RunSt =
    RunNew
  | RunWrite (FileKey, Fd, Int, Int)
  | RunSync  (FileKey, Fd, Int, Int, Bool)
  | RunFin


