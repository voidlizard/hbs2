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


import Control.Monad.Trans.Cont
import Network.ByteOrder qualified as N
import Data.HashSet qualified as HS
import Data.HashPSQ qualified as PSQ
import Data.Vector qualified as V
import Data.HashMap.Strict qualified as HM
import Data.ByteString qualified as BS
import Data.Sequence qualified as Seq
import System.FilePath.Posix
import System.Posix.Files qualified as Posix
import System.Posix.IO as PosixBase
import System.Posix.Types as Posix
import System.Posix.Unistd
import System.Posix.IO.ByteString as Posix
import System.Posix.Files ( getFileStatus
                          , modificationTimeHiRes
                          , setFileTimesHiRes
                          , getFdStatus
                          , FileStatus(..)
                          , setFileMode
                          )
import System.Posix.Files qualified as PFS
import System.IO.MMap as MMap
import Control.Concurrent.STM qualified as STM

ncqStorageStop3 :: forall m . MonadUnliftIO m => NCQStorage3 -> m ()
ncqStorageStop3 NCQStorage3{..} = atomically $ writeTVar ncqStopReq True

ncqStorageRun3 :: forall m . MonadUnliftIO m
               => NCQStorage3
               -> m ()
ncqStorageRun3 ncq@NCQStorage3{..} = flip runContT pure do
  ContT $ bracket setAlive (const unsetAlive)

  closeQ <- liftIO newTQueueIO

  closer <- spawnActivity $ liftIO $ fix \loop -> do
    what <- atomically do
      tryReadTQueue closeQ >>= \case
        Just e  -> pure $ Just e
        Nothing -> do
          stop  <- readTVar ncqStopReq
          if not stop then STM.retry else pure Nothing

    maybe1 what none $ \(fk :: FileKey, fh) -> do
      closeFd fh
      ncqIndexFile ncq (DataFile fk)
      loop

  let shLast = V.length ncqWriteOps - 1
  spawnActivity $ pooledForConcurrentlyN_ (V.length ncqWriteOps) [0..shLast] $ \i -> do
    let q = ncqWriteOps ! i
    forever (liftIO $ join $ atomically (readTQueue q))

  replicateM_ 2 $ spawnActivity $ fix \next -> do

      (h, answ) <- atomically $ readTQueue ncqReadReq
      let answer l = atomically (putTMVar answ l)

      -- debug $ "REQ" <+> pretty h

      atomically (ncqLookupEntrySTM ncq h) >>= \case
        Nothing  -> none
        Just e -> answer (Just (InMemory (ncqEntryData e))) >> next

      NCQState{..} <- readTVarIO ncqState

      for_ ncqStateIndex $ \(_, fk) -> do
       CachedIndex bs nw <- ncqGetCachedIndex ncq fk
       ncqLookupIndex h (bs, nw) >>= \case
        Just (IndexEntry fk o s) -> answer (Just (InFossil fk o s)) >> next
        Nothing -> none

      -- debug $ "NOT FOUND SHIT" <+> pretty h
      answer Nothing >> next

  spawnActivity measureWPS

  spawnActivity $ forever do
    withSem ncqServiceSem (ncqSweepObsoleteStates ncq)
    pause @'Seconds 10

  spawnActivity (ncqSweepLoop ncq)

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

      stop <- readTVarIO ncqStopReq
      sync <- readTVarIO ncqSyncReq

      let needClose = total >= ncqMinLog || stop

      rest <- if not (sync || needClose || w > ncqFsync) then
                  pure w
                else do
                  appendTailSection fh >> liftIO (fileSynchronise fh)

                  ss <- liftIO (PFS.getFdStatus fh) <&> fromIntegral . PFS.fileSize

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

  wait closer

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

data RunSt =
    RunNew
  | RunWrite (FileKey, Fd, Int, Int)
  | RunSync  (FileKey, Fd, Int, Int, Bool)
  | RunFin


zeroSyncEntry :: ByteString
zeroSyncEntry = ncqMakeSectionBS (Just B) zeroHash zeroPayload
  where zeroPayload = N.bytestring64 0
        zeroHash    = HashRef (hashObject zeroPayload)
{-# INLINE zeroSyncEntry #-}

zeroSyncEntrySize :: Word64
zeroSyncEntrySize = fromIntegral (BS.length zeroSyncEntry)
{-# INLINE zeroSyncEntrySize #-}

-- 1. It's M-record
-- 2. It's last w64be == fileSize
-- 3. It's hash == hash (bytestring64be fileSize)
-- 4. recovery-strategy: start-to-end, end-to-start
fileTailRecord :: Integral a => a -> ByteString
fileTailRecord w = do
  -- on open: last w64be == fileSize
  let paylo = N.bytestring64 (fromIntegral w + zeroSyncEntrySize)
  let h     = hashObject @HbSync paylo & coerce
  ncqMakeSectionBS (Just M) h paylo
{-# INLINE fileTailRecord #-}

appendSection :: forall m . MonadUnliftIO m
            => Fd
            -> ByteString
            -> m Int -- (FOff, Int)

appendSection fh sect = do
  -- off <- liftIO $ fdSeek fh SeekFromEnd 0
  -- pure (fromIntegral off, fromIntegral len)
  liftIO (Posix.fdWrite fh sect) <&> fromIntegral
{-# INLINE appendSection #-}

appendTailSection :: MonadIO m => Fd -> m ()
appendTailSection fh = liftIO do
  s <- Posix.fileSize <$> Posix.getFdStatus fh
  void (appendSection fh (fileTailRecord s))
{-# INLINE appendTailSection #-}


