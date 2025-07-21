{-# Language AllowAmbiguousTypes #-}
{-# Language UndecidableInstances #-}
{-# Language MultiWayIf #-}
{-# Language RecordWildCards #-}
{-# Language ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Main where

import HBS2.Prelude.Plated
import HBS2.OrDie
import HBS2.Hash
import HBS2.Data.Types.Refs
import HBS2.Misc.PrettyStuff
import HBS2.Clock
import HBS2.Merkle
import HBS2.Polling

import HBS2.Storage
import HBS2.Storage.Simple
import HBS2.Storage.Operations.ByteString

import HBS2.System.Logger.Simple.ANSI

import HBS2.Data.Log.Structured.SD
import HBS2.Storage.NCQ
import HBS2.Storage.NCQ2 as N2
import HBS2.Data.Log.Structured.NCQ

import HBS2.CLI.Run.Internal.Merkle

import Data.Config.Suckless.Syntax
import Data.Config.Suckless.Script as SC
import Data.Config.Suckless.System

import DBPipe.SQLite hiding (field)

import System.Posix.Files qualified as PFS
import Numeric (showHex)
import Data.Ord (Down(..))
import Data.Char
import Data.Bits
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Text.Encoding qualified as TE
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Builder
import Data.Hashable (hash)
import Data.Maybe
import Data.Either
import Data.Word
import Data.List qualified as List
import Data.Vector qualified as V
import Data.Vector ((!))
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Maybe
import Control.Monad.Except (runExceptT)
import Network.ByteOrder qualified as N
import Data.Coerce
import Data.HashPSQ qualified as HPSQ
import Data.HashSet qualified as HS
import Data.HashSet (HashSet)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.IntMap qualified as IntMap
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Fixed
import System.Environment
import System.FilePath.Posix
import System.Directory
import System.Posix.Fcntl
import System.Posix.IO
import System.IO.MMap
import System.IO qualified as IO
import System.Exit (exitSuccess, exitFailure)
import System.Random
import System.Random.MWC as MWC
import System.Random.Stateful
import System.Random.Shuffle (shuffleM)
import Safe
import Lens.Micro.Platform
import Control.Concurrent.STM qualified as STM
import System.IO.Temp qualified as Temp
import System.Mem

import UnliftIO
import UnliftIO.Async

import Test.Tasty.HUnit
import Text.InterpolatedString.Perl6 (qc)

import Streaming.Prelude qualified as S
import System.TimeIt

import System.IO.Unsafe (unsafePerformIO)

import Data.BloomFilter.Easy as Bloom

{- HLINT ignore "Functor law" -}

setupLogger :: MonadIO m => m ()
setupLogger = do
  setLogging @DEBUG  $ toStderr . logPrefix "[debug] "
  setLogging @ERROR  $ toStderr . logPrefix "[error] "
  setLogging @WARN   $ toStderr . logPrefix "[warn] "
  setLogging @NOTICE $ toStdout . logPrefix ""

flushLoggers :: MonadIO m => m ()
flushLoggers = do
  silence

silence :: MonadIO m => m ()
silence = do
  setLoggingOff @DEBUG
  setLoggingOff @ERROR
  setLoggingOff @WARN
  setLoggingOff @NOTICE
  setLoggingOff @TRACE


data TestEnv =
  TestEnv
  { testEnvDir :: FilePath
  }

runTest :: forall m a . MonadUnliftIO m => (TestEnv -> m a) -> RunM C m a
runTest action = do
  pref <- lookupValueDef nil "test:root" >>= \case
            StringLike dir -> pure dir
            _              -> pure "/tmp/ncq-tests"

  keep <- lookupValueDef nil "test:dir:keep" >>= \case
            LitBoolVal True -> pure True
            _               -> pure False

  mkdir pref

  tmp <- liftIO (Temp.createTempDirectory pref "ncq-test")
  SC.bind "test:dir"  (mkStr tmp)

  flip runContT pure do
    ContT $ bracket none $ const do
      unless keep (rm tmp)
      flushLoggers

    lift $ lift $ action (TestEnv tmp)


testNCQFuckupRecovery1 :: MonadUnliftIO m
                       => TestEnv
                       -> m ()

testNCQFuckupRecovery1 TestEnv{..} = flip runContT pure do

  let ncqDir = testEnvDir </> "ncq"

  (cur,ha,h0) <- lift $ withNCQ id ncqDir $ \ncq -> do
    let sto = AnyStorage ncq

    source <- LBS.take (100 * 1024^2) <$> liftIO (LBS.readFile "/dev/urandom")

    let h0 = hashObject @HbSync source

    hash <- runExceptT (writeAsMerkle sto source <&> HashRef)
               >>= orThrowPassIO @_ @SomeException

    notice $ "stored" <+> pretty hash <+> pretty (LBS.length source)

    pure (ncqGetCurrentName ncq, hash, h0)

  liftIO do
    ss <- randomRIO (1, 32*1024)
    shit <- LBS.take ss <$> LBS.readFile "/dev/urandom"
    BS.appendFile cur (LBS.toStrict shit)
    newSize <- getFileSize cur
    notice $ "CURRENT-FILE" <+> pretty cur <+> "successfully corrupted" <+> pretty newSize

  notice $ "CURRENT-FILE" <+> pretty cur

  lift $ withNCQ id ncqDir $ \ncq -> do
    notice $ "REOPEN STORAGE"
    let sto = AnyStorage ncq

    lbs <- runExceptT (getTreeContents sto ha)
               >>= orThrowPassIO

    let h1 = hashObject @HbSync lbs

    when (h0 /= h1) do
      error "corrupted state"

    notice $ "loaded" <+> pretty ha <+> pretty (LBS.length lbs)



testNCQLongWrite :: MonadUnliftIO m => Int -> TestEnv -> m ()
testNCQLongWrite n TestEnv{..} = flip runContT pure do
  let ncqDir = testEnvDir </> "ncq-simple"

  -- Step 1: Write block
  lift $ withNCQ id ncqDir $ \ncq -> liftIO do
    let sto = AnyStorage ncq
    replicateM_ n do
      size  <- randomRIO (1, 256*1024)
      let payload = LBS.replicate size 0x41 -- 0x41 = 'A'
      h <- putBlock sto payload
      assertBool "block written" (isJust h)


testNCQLongWriteRead :: MonadUnliftIO m => Int -> TestEnv -> m ()
testNCQLongWriteRead n TestEnv{..} = flip runContT pure do
  let ncqDir = testEnvDir </> "ncq-simple"

  wq <- newTQueueIO

  -- Step 1: Write block
  lift $ withNCQ id ncqDir $ \ncq -> liftIO do
    let sto = AnyStorage ncq
    replicateM_ n do
      size  <- randomRIO (1, 256*1024)
      let payload = LBS.replicate size 0x41 -- 0x41 = 'A'
      h <- putBlock sto payload
      assertBool "block written" (isJust h)
      for_ h $ \hhh -> do
        atomically $ writeTQueue wq (HashRef hhh)

    r <- atomically $ STM.flushTQueue wq

    for_ r $ \h -> do
      s <- ncqLocate ncq h
      assertBool "actually written" (isJust s)

testNCQSimple1 :: MonadUnliftIO m => TestEnv -> m ()
testNCQSimple1 TestEnv{..} = flip runContT pure do
  let ncqDir = testEnvDir </> "ncq-simple"

  for_ [ 0 .. 18 ] $ \s -> do
    let size = 2 ^ s
    let payload = LBS.replicate size 0x41 -- 0x41 = 'A'
    let expectedHash = hashObject @HbSync payload

    -- Step 1: Write block
    lift $ withNCQ id ncqDir $ \ncq -> do
      let sto = AnyStorage ncq
      h <- putBlock sto payload `orDie` "failed to write block"
      liftIO $ assertBool "hashes match (write)" (h == expectedHash)

    -- Step 2: Read back
    lift $ withNCQ id ncqDir $ \ncq -> do
      let sto = AnyStorage ncq
      blk <- getBlock sto (coerce expectedHash) `orDie` "block not found"
      sx <- hasBlock sto (coerce expectedHash)

      loc <- ncqLocate ncq (coerce expectedHash)
               >>= orThrowUser "not found"

      blk0 <- ncqStorageGet_ ncq loc

      let sblk0 = LBS.length <$> blk0

      liftIO $ print $ "block size"
                 <+> pretty sx
                 <+> ";"
                 <+> pretty (LBS.length blk)
                 <+> ";"
                 <+> pretty size
                 <+> ";"
                 <+> pretty sblk0
                 <+> pretty loc

      liftIO $ do
        assertBool "block has correct length" (LBS.length blk == size)
        assertBool "block contents are correct" (blk == payload)


testNCQSimple2 :: MonadUnliftIO m => Int -> TestEnv -> m ()
testNCQSimple2 n TestEnv{..} = flip runContT pure do
  let ncqDir = testEnvDir </> "ncq-simple2"

  let alph_ = V.fromList ['A' .. 'z']
  cnt <- newTVarIO 0

  let alphx = liftIO do
       i <- atomically $ stateTVar cnt (\x -> (x, succ x))
       pure $ alph_ ! ( i `mod` V.length alph_)

  -- Step 1: write N blocks
  hashes <- lift $ withNCQ id ncqDir $ \ncq -> do
    let sto = AnyStorage ncq
    replicateM n do
      size <- liftIO $ randomRIO (0, 256 * 1024)
      chr <- alphx
      let payload = LBS.replicate size (fromIntegral $ ord chr)
      let h = hashObject @HbSync payload
      h' <- putBlock sto payload `orDie` "putBlock failed"
      loc <- ncqLocate ncq (coerce h)
      s <- hasBlock sto h
      w <- getBlock sto h
      let w' = fromMaybe mempty w

      if w == Just payload then do
        debug $ "okay" <+> pretty loc
      else do
        err $ pretty s <> "/" <> pretty size
               <+> viaShow (LBS.take 48 w')
               <+> ".."
               <+> viaShow (LBS.take 8 $ LBS.reverse w')
               <> line
               <+> pretty loc

        error "ABORTED"

      liftIO $ assertBool "hash matches" (h == h')
      pure (h, size, payload)

  let testRead ncq = do
        let sto = AnyStorage ncq
        forM_ hashes $ \(h, expectedSize, expectedPayload) -> do
          loc <- ncqLocate ncq (coerce h) >>= orThrowUser "not found"
          blk <- getBlock sto (coerce h) `orDie` "block not found"
          sx  <- hasBlock sto (coerce h)
          blk0 <- ncqStorageGet_ ncq loc
          let sblk0 = LBS.length <$> blk0
          let actualSize = LBS.length blk

          debug $ "block size"
              <+> pretty sx
              <+> ";"
              <+> pretty actualSize
              <+> ";"
              <+> pretty expectedSize
              <+> ";"
              <+> pretty sblk0
              <+> pretty loc

          liftIO do
            assertBool "size match" (actualSize == expectedSize)
            assertBool "payload match" (blk == expectedPayload)

  -- Step 2: reopen and verify
  lift $ withNCQ id ncqDir $ \ncq -> do
      testRead ncq
      -- ncqIndexRightNow ncq
      pause @'Seconds 2

  liftIO $ print $ "LAST PASS"
  -- Step 3: reopen and verify - fossil
  lift $ withNCQ id ncqDir $ \ncq -> do
    testRead ncq

testNCQ1 :: MonadUnliftIO m
         => Int
         -> TestEnv
         -> m ()

testNCQ1 n TestEnv{..} = flip runContT pure $  callCC \stop -> do

    let tmp = testEnvDir

    let inputDir = tmp </> "input"
    let ncqDir   = tmp </> "ncq-test-data"

    for_ [inputDir] mkdir

    twritten <- newTVarIO (mempty :: HashSet HashRef)

    nSize <- newTVarIO 0

    tssQ <- newTQueueIO

    forM_ [1..n] $ \i -> liftIO do
      withBinaryFile "/dev/urandom" ReadMode \urandom -> do
        let fname = inputDir </> show i <> ".bin"
        size <- randomRIO (1, 256*1024)
        atomically $ modifyTVar' nSize (+size)
        file <- BS.copy <$> BS.hGetSome urandom size
        BS.writeFile fname file
        let !ha = hashObject @HbSync file
        let !len = fromIntegral $ BS.length file
        -- atomically $ writeTQueue tssQ (fname, (ha, fromIntegral $! BS.length file))
        -- l <- getFileSize fname
        -- atomically $ writeTQueue tssQ (fname, (ha, l))
        atomically $ writeTQueue tssQ (fname, (ha, len))
        -- performGC

    fss <- atomically (STM.flushTQueue tssQ)

    stop ()

    liftIO do
      withNCQ id ncqDir $ \ncq -> flip runContT pure do

        let sto = AnyStorage ncq
        let fileMap = HM.fromList [ (ha,(s,fn)) | (fn,(ha,s)) <- fss ]

        let
          written :: forall m a . (Fractional a, MonadIO m) => m [(HashRef, a)]
          written = readTVarIO twritten <&> HS.toList <&> fmap (,0.1)

        ContT $ withAsync $ forever do
          polling (Polling 0.25 0.25)  written $ \(HashRef hz) -> liftIO do
            what <- getBlock sto hz >>= orThrowUser ("block not found" <+> pretty hz)
            let h2 = hashObject @HbSync what

            (s,_) <- HM.lookup hz fileMap & orThrowUser "fileMap entry  missed"

            ssz <- hasBlock sto hz
                     >>= orThrowUser ("block size not found" <+> pretty hz)

            when (ssz /= s) do
              error $ show $ "size mismatch" <+> pretty hz

            when (hz /= h2) do
              error $ show $ pretty "hash does not  match" <+> pretty hz <+> pretty s

        liftIO $ forConcurrently_ fss $ \(fn, (ha,s)) -> do
          co  <- liftIO (BS.readFile fn) <&> LBS.fromStrict
          h1 <- putBlock sto co >>= orThrowUser "block not written"
          lbs2 <- getBlock sto ha >>= orThrowUser "block not found"
          let h2 = hashObject @HbSync lbs2

          when (ha /= h2 || h1 /= ha) do
            error $ show $ pretty "hash does not  match" <+> pretty h1 <+> pretty s

          atomically $ modifyTVar twritten (HS.insert (HashRef h1))

          debug $ "putBlock" <+> pretty ha <+> pretty h2

        liftIO $ forConcurrently_ fss $ \(fn, (ha,s)) -> do
          lbs2 <- getBlock sto ha >>= orThrowUser "block not found"
          let h2 = hashObject @HbSync lbs2

          when (ha /= h2) do
            error $ show $ pretty "hash does not  match" <+> pretty ha <+> pretty s

          debug $ "getBlock" <+> pretty ha <+> pretty h2

    liftIO do
      withNCQ id ncqDir $ \ncq -> flip runContT pure do

        let sto = AnyStorage ncq

        for_ fss $ \(fn, (ha,s)) -> do
          lbs2 <- getBlock sto ha >>= orThrowUser "block not found"
          let h2 = hashObject @HbSync lbs2

          when (ha /= h2) do
            error $ show $ pretty "hash does not  match" <+> pretty ha <+> pretty s

          debug $ "getBlock" <+> pretty ha <+> pretty h2


testNCQTree1 :: MonadUnliftIO m
         => Int
         -> TestEnv
         -> m ()

testNCQTree1 n TestEnv{..} = flip runContT pure do

  let size = 1024 * 1024 * fromIntegral n

  let tmp = testEnvDir

  let inputDir = tmp </> "input"
  let ncqDir   = tmp </> "ncq-test-data"

  treeLbs <- LBS.take size <$> liftIO (LBS.readFile ("/dev/urandom"))

  let h1 = hashObject @HbSync treeLbs

  lift $ withNCQ id  ncqDir $ \ncq1 -> do

    let sto = AnyStorage ncq1

    r <- createTreeWithMetadata sto Nothing mempty treeLbs
           >>= orThrowPassIO

    lbs2 <- runExceptT (getTreeContents sto r)
              >>= orThrowPassIO

    let h2 = hashObject @HbSync lbs2


    let l1 = LBS.length treeLbs
    let l2 = LBS.length treeLbs
    display (mkList @C [mkSym r, mkSym h1, mkSym h2, mkInt l1, mkInt l2])

    liftIO $ assertBool "hashes equal" (h1 == h2)

    -- display (mkSym @C $ show $ pretty r)

testNCQRefs1 :: MonadUnliftIO m
         => Int
         -> TestEnv
         -> m ()

testNCQRefs1 n TestEnv{..} = flip runContT pure do

  let tmp = testEnvDir

  let ncqDir   = tmp </> "ncq-test-data"

  refs <- liftIO $ replicateM n $ do
            ref <- SomeRefKey <$> randomIO @Word64
            val <- randomIO @Word64 <&> hashObject . serialise
            pure (ref, val)

  lift $ withNCQ id ncqDir $ \ncq -> do
    let sto = AnyStorage ncq

    for_ refs $ \(k,v) -> do
      updateRef sto k v

    for_ refs $ \(k,v0) -> liftIO do
      v1 <- getRef sto k
      assertBool "refs equal 1" (Just v0 == v1)

  notice $ "all" <+> pretty n  <+> "refs found"

  debug "restart storage"

  lift $ withNCQ id ncqDir $ \ncq -> do
    let sto = AnyStorage ncq

    for_ refs $ \(k,v0) -> liftIO do
      v1 <- getRef sto k
      assertBool "refs equal 2" (Just v0 == v1)
      delRef sto k

    notice $ "all" <+> pretty n  <+> "refs found after restart"

    for_ refs $ \(k,_) -> liftIO do
      v1 <- getRef sto k
      assertBool "ref deleted" (isNothing v1)

    notice $ "all" <+> pretty n  <+> "refs deleted"


testNCQConcurrent1 :: MonadUnliftIO m
         => Bool
         -> Int
         -> Int
         -> TestEnv
         -> m ()

testNCQConcurrent1 noRead tn n TestEnv{..} = flip runContT pure do

  let tmp = testEnvDir
  let inputDir = tmp </> "input"
  let ncqDir   = tmp </> "ncq-test-data"

  debug "preparing"

  mkdir inputDir

  debug $ pretty inputDir

  filez <- liftIO $ pooledReplicateConcurrentlyN 8 n $ do
    size <- randomRIO (64*1024, 256*1024)
    w <- liftIO (randomIO :: IO Word8)
    let tbs = BS.replicate size w -- replicateM size w <&> BS.pack
    let ha = hashObject @HbSync tbs -- & show . pretty
    let fn = inputDir </> show (pretty ha)
    liftIO $ BS.writeFile fn tbs
    pure (fn, ha, BS.length tbs)

  debug "done"

  let fnv = V.fromList filez
  let ssz = sum [ s | (_,_,s) <- filez ] & realToFrac

  setLoggingOff @DEBUG

  for_ [1 .. tn] $ \tnn -> do

    (t,_) <- timeItT $ liftIO $ withNCQ id  ncqDir $ \ncq1 -> do

      pooledForConcurrentlyN_ tnn  fnv $ \(n,ha,_) -> do
        co <- BS.readFile n <&> LBS.fromStrict
        putBlock ncq1 co

      pooledReplicateConcurrentlyN_ tnn (10 * V.length fnv) do
        unless noRead do
          i <- randomRIO (0, V.length fnv - 1)
          let (n,ha,_) = fnv ! i
          sz <- getBlock ncq1 ha
          none

    let tt = realToFrac @_ @(Fixed E2) t
    let speed = ((ssz / (1024 **2)) / t) & realToFrac @_ @(Fixed E2)
    notice $ pretty tnn <+> pretty tt <+> pretty speed

    rm ncqDir


testNCQ2Sweep1 :: forall c m . (MonadUnliftIO m, IsContext c)
         => [Syntax c]
         -> TestEnv
         -> m ()

testNCQ2Sweep1 syn TestEnv{..} = do
  debug $ "testNCQ2Sweep1" <+> pretty syn
  let tmp = testEnvDir
  let ncqDir   = tmp
  q <- newTQueueIO

  g <- liftIO MWC.createSystemRandom

  let (opts, argz) = splitOpts [] syn

  let n = headDef 100000 [ fromIntegral x | LitIntVal x <- argz ]

  bz <- replicateM n $ liftIO do
          n <- (`mod` (256*1024)) <$> uniformM @Int g
          uniformByteStringM n g

  notice $ "generate" <+> pretty n <+> "blocks"

  ncqWithStorage ncqDir $ \sto -> liftIO do
    for bz $ \z ->  do
      h <- ncqPutBS sto (Just B) Nothing z
      atomically $ writeTQueue q h

  ncqWithStorage ncqDir $ \sto -> liftIO do
    notice $ red "PERFORM MERGE"
    ncqMergeFull sto

  notice $ "full sweep unused states"

  ncqWithStorage ncqDir $ \sto -> liftIO do
    ncqSweepStates sto
    ncqSweepFossils sto

  notice $ "lookup" <+> pretty n <+> "blocks"

  ncqWithStorage ncqDir $ \sto -> liftIO do
    hashes <- atomically (STM.flushTQueue q)
    for_ hashes $ \ha -> do
      found <- ncqLocate2 sto ha <&> maybe (-1) ncqEntrySize
      assertBool (show $ "found" <+> pretty ha) (found > 0)



testNCQ2Sweep2 :: forall c m . (MonadUnliftIO m, IsContext c)
         => [Syntax c]
         -> TestEnv
         -> m ()

testNCQ2Sweep2 syn TestEnv{..} = do
  debug $ "testNCQ2Sweep2" <+> pretty syn
  let tmp = testEnvDir
  let ncqDir   = tmp
  q <- newTQueueIO

  g <- liftIO MWC.createSystemRandom

  let (opts, argz) = splitOpts [] syn

  let n = headDef 100000 [ fromIntegral x | LitIntVal x <- argz ]

  notice $ "generate" <+> pretty n <+> "blocks"

  bz <- replicateM n $ liftIO do
          n <- (`mod` (256*1024)) <$> uniformM @Int g
          uniformByteStringM n g

  -- race (pause @'Seconds 260) do

  ncqWithStorage ncqDir $ \sto -> liftIO do
    for_ bz $ \z ->  do
      h <- ncqPutBS sto (Just B) Nothing z
      atomically $ writeTQueue q h

    notice "wait some time to see merge+sweep"
    pause @'Seconds 240

  ncqWithStorage ncqDir $ \sto -> liftIO do
    hashes <- atomically (STM.flushTQueue q)
    for_ hashes $ \ha -> do
      found <- ncqLocate2 sto ha <&> maybe (-1) ncqEntrySize
      assertBool (show $ "found" <+> pretty ha) (found > 0)



testNCQ2Kill1 :: forall c m . (MonadUnliftIO m, IsContext c)
         => [Syntax c]
         -> TestEnv
         -> m ()

testNCQ2Kill1 syn TestEnv{..} = flip runContT pure do
  debug $ "testNCQ2Kill1" <+> pretty syn
  let tmp = testEnvDir
  let ncqDir   = tmp
  q <- newTQueueIO

  g <- liftIO MWC.createSystemRandom

  let (opts, argz) = splitOpts [] syn

  let n = headDef 100000 [ fromIntegral x | LitIntVal x <- argz ]

  notice $ "generate" <+> pretty n <+> "blocks"

  bz <- replicateM n $ liftIO do
          n <- (`mod` (256*1024)) <$> uniformM @Int g
          uniformByteStringM n g

  -- race (pause @'Seconds 260) do

  wIdle <- newEmptyTMVarIO

  ncq1 <- ContT $ withAsync $ ncqWithStorage ncqDir $ \sto -> liftIO do
    ncqSetOnRunWriteIdle sto (atomically (putTMVar wIdle ()))
    for_ bz $ \z ->  do
      h <- ncqPutBS sto (Just B) Nothing z
      atomically $ writeTQueue q h
    pause @'Seconds 300

  notice $ red "WAIT FUCKING IDLE!"

  atomically $ takeTMVar wIdle

  notice $ red "GOT FUCKING IDLE!" <+> "lets see what happen now"

  cancel ncq1

  liftIO $ ncqWithStorage ncqDir $ \sto -> liftIO do
    hashes <- atomically (STM.flushTQueue q)
    for_ hashes $ \ha -> do
      found <- ncqLocate2 sto ha <&> maybe (-1) ncqEntrySize
      assertBool (show $ "found" <+> pretty ha) (found > 0)


testNCQ2Simple1 :: forall c m . (MonadUnliftIO m, IsContext c)
         => [Syntax c]
         -> TestEnv
         -> m ()

testNCQ2Simple1 syn TestEnv{..} = do
  debug $ "testNCQ2Simple1" <+> pretty syn
  let tmp = testEnvDir
  let ncqDir   = tmp
  q <- newTQueueIO

  g <- liftIO MWC.createSystemRandom

  let (opts, argz) = splitOpts [] syn

  let n = headDef 100000 [ fromIntegral x | LitIntVal x <- argz ]
  let l = headDef 5 $ drop 1 [ fromIntegral x | LitIntVal x <- argz ]
  let s = headDef (256*1024) $ drop 2 [ fromIntegral (1024 * x) | LitIntVal x <- argz ]


  notice $ "insert" <+> pretty n <+> "random blocks of size" <+> pretty s

  thashes <- newTQueueIO

  ncqWithStorage ncqDir $ \sto -> liftIO do
    replicateM_ n do
      n <- (`mod` s) <$> uniformM @Int g
      z <- uniformByteStringM n g
      h <- ncqPutBS sto (Just B) Nothing z
      found <- ncqLocate2 sto h <&> maybe (-1) ncqEntrySize
      atomically $ writeTQueue q h
      assertBool (show $ "found-immediate" <+> pretty h) (found > 0)
      atomically $ writeTQueue thashes h

    t0 <- getTimeCoarse

    hs <- atomically $ STM.flushTQueue thashes

    flip fix (t0, List.length hs,  hs) $ \loop (tp, num, xs) -> case xs of
      [] -> none
      (ha:rest) -> do
        t1 <- getTimeCoarse

        t2 <- if realToFrac (toNanoSecs (t1 - t0)) / 1e9 < 1.00 then do
                 pure tp
              else do
                 notice $ green "lookup"  <+> pretty num
                 pure t1

        found <- ncqLocate2 sto ha <&> maybe (-1) ncqEntrySize
        assertBool (show $ "found" <+> pretty ha) (found > 0)
        unless (List.null hs) $ loop (t1, pred num, rest)

  hashes <- atomically (STM.flushTQueue q)

  notice $ "merge data"

  ncqWithStorage ncqDir $ \sto -> liftIO do
    notice "perform merge"
    ncqMergeFull sto
    ncqSweepStates sto
    ncqSweepFossils sto

  notice $ "full sweep unused states"

  ncqWithStorage ncqDir $ \sto -> liftIO do
    ncqSweepStates sto
    ncqSweepFossils sto

  notice $ "lookup" <+> pretty n <+> "blocks"

  ncqWithStorage ncqDir $ \sto -> liftIO do

    replicateM_ l do

      -- performMajorGC

      (t1,_) <- timeItT do

          for_ hashes $ \ha -> do
            found <- ncqLocate2 sto ha <&> maybe (-1) ncqEntrySize
            assertBool (show $ "found" <+> pretty ha) (found > 0)
            -- debug $ fill 44 (pretty ha) <+> fill 8 (pretty found)

      notice $ pretty (sec6 t1) <+> "lookup" <+> pretty n <+> "blocks"



testNCQ2Lookup1:: forall c m . (MonadUnliftIO m, IsContext c)
         => [Syntax c]
         -> TestEnv
         -> m ()

testNCQ2Lookup1 syn TestEnv{..} = do
  debug $ "testNCQ2Simple1" <+> pretty syn
  let tmp = testEnvDir
  let ncqDir   = tmp
  q <- newTQueueIO

  g <- liftIO MWC.createSystemRandom

  let (opts, argz) = splitOpts [("-r",1),("-m",0)] syn

  let n = headDef 100000 [ fromIntegral x | LitIntVal x <- argz ]
  let nt = max 2 . headDef 1 $ [ fromIntegral x | LitIntVal x <- drop 1 argz ]
  let nl = headDef 3 $ [ fromIntegral x | LitIntVal x <- drop 2 argz ]
  let r = (4*1024, 64*1024)

  let rt = headDef 2 [ fromIntegral x | ListVal [StringLike "-r", LitIntVal x ] <- opts ]
  let merge = headDef False [ True | ListVal [StringLike "-m"] <- opts ]

  notice $ "insert" <+> pretty n <+> "random blocks of size" <+> parens (pretty r) <+> pretty opts

  thashes <- newTQueueIO

  sizes <- liftIO $ replicateM n (uniformRM r g )

  ncqWithStorage ncqDir $ \sto -> liftIO do
    pooledForConcurrentlyN_ 8  sizes $ \size -> do
      z <- uniformByteStringM size g
      h <- ncqPutBS sto (Just B) Nothing z
      atomically $ writeTQueue thashes h


    hs <- atomically $ STM.flushTQueue thashes

    when merge do
      notice "merge full"
      ncqMergeFull sto

    ffs <- N2.ncqListTrackedFiles sto
    notice $ "database prepared" <+> pretty (List.length ffs) <+> pretty (List.length hs)

    replicateM_ nl do

      tfound <- newTVarIO 0

      t0 <- getTimeCoarse

      void $ flip runContT pure $ callCC \exit -> do

        readQ <- newTQueueIO

        reader <- replicateM rt $ ContT $ withAsync $ fix \next -> do

          (h, answ) <- atomically $ readTQueue readQ

          f1 <- ncqLookupEntry sto h <&> isJust

          when f1 do
            atomically (putTMVar answ True) >> next

          ffs <- liftIO $ N2.ncqListTrackedFiles sto

          for_ ffs $ \(f, ce, te) -> do

            -- when (isNotPending ce) do
            case ce of
              Just (PendingEntry{}) -> none

              Just (CachedEntry{..}) -> do
                found <- ncqLookupIndex h (cachedMmapedIdx, cachedNway) <&> isJust

                when found do
                  atomically (putTMVar answ True) >> next

              Nothing -> do

                tnow <- getTimeCoarse >>= newTVarIO

                let indexFile = N2.ncqGetFileName sto (toFileName (IndexFile f))
                let dataFile  = N2.ncqGetFileName sto (toFileName (DataFile f))

                what@(idxBs, idxNway) <- nwayHashMMapReadOnly indexFile `orDie` "mmap fucked"
                datBs  <- mmapFileByteString dataFile Nothing

                let ce = CachedEntry idxBs datBs idxNway tnow

                atomically $ writeTVar te (Just ce)

                found <- ncqLookupIndex h what <&> isJust

                when found do
                  atomically (putTMVar answ True) >> next

          atomically (putTMVar answ False) >> next

        liftIO $ pooledForConcurrentlyN_ nt hs $ \h -> do
          answ <- newEmptyTMVarIO
          atomically $ writeTQueue readQ (h, answ)
          found <- atomically $ takeTMVar answ

          when found do
            atomically $ modifyTVar' tfound succ

      t1 <- getTimeCoarse

      let dt = realToFrac (toNanoSecs (t1 - t0)) / 1e9 :: Fixed E3

      found <- readTVarIO tfound

      notice $ "scan all files" <+> pretty dt <+> pretty found

    -- pause @'Seconds 5


genRandomBS :: forall g m . (Monad m, StatefulGen g m) => g -> Int -> m ByteString
genRandomBS g n = do
  uniformByteStringM n g

sec6 :: RealFrac a => a -> Fixed E6
sec6 = realToFrac

sec2 :: RealFrac a => a -> Fixed E2
sec2 = realToFrac

sec3 :: RealFrac a => a -> Fixed E3
sec3 = realToFrac

testNCQ2Merge1 :: MonadUnliftIO m
         => Int
         -> TestEnv
         -> m ()

testNCQ2Merge1 n TestEnv{..} = do
  let tmp = testEnvDir
  let ncqDir   = tmp

  g <- liftIO MWC.createSystemRandom

  let fake = n `div` 3

  ncqWithStorage ncqDir $ \sto -> liftIO do

    notice $ "write" <+> pretty n <+> "random blocks"

    ws <- flip fix (mempty :: HashSet HashRef) $ \loop -> \case
      hs | HS.size hs >= n -> pure hs
         | otherwise  -> do

        s <- liftIO $ genRandomBS g (256 * 1024)
        h <- ncqPutBS sto (Just B) Nothing s
        loop (HS.insert h hs)

    notice $ "written" <+> pretty (HS.size ws)

    assertBool "all written" (HS.size ws == n)

    nHashes <- HS.fromList . filter (not . flip HS.member ws) <$> replicateM fake do
                 liftIO (genRandomBS g (64*1024)) <&> HashRef . hashObject

    notice $ "gen" <+> pretty (HS.size nHashes) <+> pretty "missed hashes"


    (t1,n1) <- over _2 sum <$> timeItT do
                  for (HS.toList ws) $ \h -> do
                    r <- ncqLocate2 sto h

                    unless (isJust r) do
                      err $ "not found" <+> pretty h

                    pure $ maybe 0 (const 1) r

    notice $ pretty (sec3 t1) <+> pretty n1 <+> pretty (n1 == HS.size ws)

    assertBool "all written" (n1 == HS.size ws)

    ncqWaitTasks sto

    let hashes = HS.toList ws <> HS.toList nHashes

    (t2,_) <- timeItT do
                for hashes $ \h -> do
                  r <- ncqLocate2 sto h
                  pure $ maybe 0 (const 1) r

    notice $ "before-merge" <+> pretty (sec3 t1) <+> pretty (List.length hashes)

    notice $ "merge whatever possible"

    n <- flip fix 0 \next i -> do
           N2.ncqMergeStep sto >>= \case
            False -> pure i
            True  -> next (succ i)

    notice $ "merged" <+> pretty n

    (t3,r) <- timeItT do
                  for hashes $ \h -> do
                    ncqLocate2 sto h >>= \case
                      Nothing -> pure $ Left h
                      Just{}  -> pure $ Right h

    let w1 = HS.fromList (rights r)
    let n2 = HS.fromList (lefts r)

    notice $ "after-merge" <+> pretty (sec3 t3) <+> pretty (HS.size w1) <+> pretty (HS.size n2)

    pause @'Seconds 300

testFilterEmulate1 :: MonadUnliftIO m
         => Bool
         -> Int
         -> TestEnv
         -> m ()

testFilterEmulate1 doMerge n TestEnv{..} = do
  let tmp = testEnvDir
  let ncqDir   = tmp

  g <- liftIO MWC.createSystemRandom

  bz <- replicateM n $ liftIO do
          n <- (`mod` (64*1024)) <$> uniformM @Int g
          uniformByteStringM n g


  hs' <- newTVarIO (mempty  :: HashSet HashRef)
  noHs' <- newTVarIO (mempty  :: HashSet HashRef)

  ncqWithStorage ncqDir $ \sto -> liftIO do

    for bz $ \z ->  do
      h <- ncqPutBS sto (Just B) Nothing z
      atomically $ modifyTVar' hs' (HS.insert h)

  replicateM_ (max 100 (n `div` 3)) $ liftIO do
    n <- (`mod` (64*1024)) <$> uniformM @Int g
    fake <- HashRef . hashObject @HbSync <$> uniformByteStringM n g
    atomically $ modifyTVar' noHs' (HS.insert fake)

  hs <- readTVarIO hs'
  noHs <- readTVarIO noHs'

  let allShit = HS.toList hs <> HS.toList noHs

  let bloom = easyList 0.01 (fmap (coerce @_ @ByteString) (HS.toList hs))

  let bucno e = hash e `mod` 4096

  let dumb  = IntSet.fromList [ bucno k | k <- HS.toList hs ]

  ncqWithStorage ncqDir $ \sto -> liftIO do

    when doMerge do
      notice "merge data"
      fix $ \next -> ncqMergeStep sto >>= \case
        True  -> next
        False -> none

    for_ [1..5] $ \i -> do

      notice $ "-- pass" <+> pretty i <+> "--"

      (t1,_) <- timeItT do
          for_ allShit $ \ha -> do
             void $ ncqLocate2 sto ha

      notice $ "lookup-no-filter" <+> pretty (realToFrac @_ @(Fixed E3) t1)

      (t2,_) <- timeItT do
          for_ allShit $ \ha -> do
             unless (HS.member  ha noHs) do
               void $ ncqLocate2 sto ha

      notice $ "lookup-fake-filter" <+> pretty (realToFrac @_ @(Fixed E3) t2)

      (t3,_) <- timeItT do
          for_ allShit $ \ha -> do
             let here = IntSet.member (bucno ha) dumb
             when here do
               void $ ncqLocate2 sto ha

      notice $ "lookup-dumb-filter" <+> pretty (realToFrac @_ @(Fixed E3) t3)

      (t4,_) <- timeItT do
          for_ allShit $ \ha -> do
             let here = Bloom.elem (coerce ha) bloom
             when here do
               void $ ncqLocate2 sto ha

      notice $ "lookup-simple-bloom-filter" <+> pretty (realToFrac @_ @(Fixed E3) t4)


testNCQ2Repair1:: MonadUnliftIO m
         => TestEnv
         -> m ()

testNCQ2Repair1 TestEnv{..} = do
  debug "testNCQ2Repair1"
  let tmp = testEnvDir
  let ncqDir   = tmp
  q <- newTQueueIO

  g <- liftIO MWC.createSystemRandom

  bz <- replicateM 3000 $ liftIO do
          n <- (`mod` (256*1024)) <$> uniformM @Int g
          uniformByteStringM n g

  ncqWithStorage ncqDir $ \sto -> liftIO do
    for_ bz $ \z ->  do
      h <- ncqPutBS sto (Just B) Nothing z
      atomically $ writeTQueue q h
      found <- ncqLocate2 sto h <&> maybe (-1) ncqEntrySize
      assertBool (show $ "found-immediate" <+> pretty h) (found > 0)

  ncqWithStorage ncqDir $ \sto -> liftIO do
    written <- N2.ncqListDirFossils  sto
    debug $ "TRACKED" <+> vcat (fmap pretty written)
    toDestroy <- pure (headMay written) `orDie` "no file written"

    debug $ "adding garbage to" <+> pretty toDestroy

    k <- (`mod` 4096) <$> uniformM @Int g
    shit <- uniformByteStringM k g
    let df = toFileName (DataFile toDestroy)
    let f = N2.ncqGetFileName sto df
    let cq = N2.ncqGetFileName sto (toFileName (IndexFile toDestroy))
    rm cq
    BS.appendFile f shit

  notice "after destroying storage"

  ncqWithStorage ncqDir $ \sto -> liftIO do
    hashes <- atomically (STM.flushTQueue q)
    for_ hashes $ \ha -> do
      found <- ncqLocate2 sto ha <&> maybe (-1) ncqEntrySize
      none
      -- assertBool (show $ "found-immediate" <+> pretty ha) (found > 0)
      -- debug $ fill 44 (pretty ha) <+> fill 8 (pretty found)


testWriteNThreads :: forall g m . (MonadUnliftIO m)
                  => FilePath
                  -> Int
                  -> Int
                  -> m ()
testWriteNThreads ncqDir tnn n = do

    g <- liftIO MWC.createSystemRandom

    wtf <- liftIO getPOSIXTime <&> show . round

    t0 <- getTimeCoarse

    w <- ncqWithStorage (ncqDir </> wtf <> show tnn)  $ \sto -> do
       ss <- liftIO $ replicateM n $ MWC.uniformRM (64*1024, 256*1024) g

       pooledForConcurrentlyN_ tnn ss $ \len -> do
         tbs <- liftIO $ genRandomBS g len
         ncqPutBS sto (Just B) Nothing tbs
         -- atomically $ modifyTVar' tss (+ len)

       -- 32 bytes per key, 4 per len
       pure $ (List.length ss * 36) +  sum ss

    t1 <- getTimeCoarse

    let t = realToFrac (toNanoSecs (t1 - t0)) / 1e9

    let total = realToFrac w

    let speed = if t > 0 then total / t else 0
    let totMegs = realToFrac @_ @(Fixed E2) $ total / (1024**2)
    let speedMbs = realToFrac @_ @(Fixed E2) $ speed / (1024**2)

    notice $ pretty tnn <+> pretty (sec2 t) <+> pretty totMegs <+> pretty speedMbs


testNCQ2Concurrent1 :: MonadUnliftIO m
         => Bool
         -> Int
         -> Int
         -> TestEnv
         -> m ()

testNCQ2Concurrent1 noRead tn n TestEnv{..} = flip runContT pure do

  let tmp = testEnvDir
  let inputDir = tmp </> "input"
  let ncqDir   = tmp </> "ncq"

  debug "preparing"

  mkdir inputDir

  debug $ pretty inputDir

  g <- liftIO MWC.createSystemRandom

  log <- liftIO $ Temp.emptyTempFile inputDir "log-.bin"

  (t0,size) <- timeItT do
    liftIO $ withFile log IO.AppendMode $ \hlog -> do
      replicateM_ n do
        size <- MWC.uniformRM (64*1024, 256*1024) g
        tbs <- genRandomBS g size
        let ha  = hashObject @HbSync tbs
        let ss = coerce ha <> tbs
        let bssize  = N.bytestring32 (fromIntegral $ BS.length ss)
        BS.hPut hlog (bssize <> ss)
      getFileSize log


  let mbps = realToFrac size / (1024**2)
  let v0 = mbps / t0
  notice $ "baseline" <+> pretty n
                      <+> pretty (sec3 t0)
                      <+> pretty (realToFrac @_ @(Fixed E2) mbps)
                      <+> pretty (sec2 v0)


  for_ [1..tn] $ \tnn -> liftIO do
    testWriteNThreads ncqDir tnn n



testNCQ2Concurrent2 :: MonadUnliftIO m
         => Int -- ^ threads
         -> Int -- ^ times
         -> Int -- ^ blocks
         -> TestEnv
         -> m ()

testNCQ2Concurrent2 tn times n TestEnv{..} = flip runContT pure do
  replicateM_ times do
    lift $ testWriteNThreads testEnvDir tn n

testNCQ2ConcurrentWriteSimple1 :: MonadUnliftIO m
         => Int
         -> Int
         -> TestEnv
         -> m ()

testNCQ2ConcurrentWriteSimple1 tn n TestEnv{..} = flip runContT pure do

  let tmp = testEnvDir
  let inputDir = tmp </> "input"
  let ncqDir   = tmp </> "ncq-test-data"

  debug "preparing"

  mkdir inputDir

  debug $ pretty inputDir

  filez <- liftIO $ pooledReplicateConcurrentlyN 8 n $ do
    size <- randomRIO (64*1024, 256*1024)
    w <- liftIO (randomIO :: IO Word8)
    let tbs = BS.replicate size w -- replicateM size w <&> BS.pack
    let ha = hashObject @HbSync tbs -- & show . pretty
    let fn = inputDir </> show (pretty ha)
    liftIO $ BS.writeFile fn tbs
    pure (fn, ha, BS.length tbs)

  debug "done"

  let fnv = V.fromList filez
  let ssz = sum [ s | (_,_,s) <- filez ] & realToFrac

  -- setLoggingOff @DEBUG

  ncq1 <- ncqStorageOpen2 ncqDir (\x -> x { ncqFsync = 64^(1024^2) } )
  w <- ContT $ withAsync (ncqStorageRun2 ncq1)

  liftIO $ pooledForConcurrentlyN_ tn  fnv $ \(n,ha,_) -> do
    co <- BS.readFile n
    ncqPutBS ncq1 (Just B) Nothing co

  liftIO $ ncqStorageStop2 ncq1
  wait w

main :: IO ()
main = do

  tvd <- newTVarIO mempty

  let dict = makeDict @C do


        entry $ bindMatch "--help" $ nil_ \case
          HelpEntryBound what -> helpEntry what
          [StringLike s]      -> helpList True (Just s)
          _                   -> helpList True Nothing


        entry $ bindMatch "--run" $ \case
          (StringLike what : args) -> liftIO do

            liftIO (readFile what)
              <&> parseTop
              >>= either (error.show) pure
              >>= \syn -> do
                    runTM tvd do

                      for_ (zip [1..] args) $ \(i,a) -> do
                        let n = Id ("$" <> fromString (show i))
                        SC.bind n a

                      SC.bind "$argv" (mkList args)

                      evalTop syn

          e -> throwIO $ BadFormException @C (mkList e)

        entry $ bindMatch "debug" $ nil_ \case

          [ LitBoolVal False ] -> do
             setLoggingOff @DEBUG

          [ StringLike "off" ] -> do
             setLoggingOff @DEBUG

          _ ->
             setLogging @DEBUG  $ toStderr . logPrefix "[debug] "

        entry $ bindMatch "test:root" $ nil_ $ \case
          [ s@(StringLike _) ] -> do
            SC.bind "test:root" s

          e -> throwIO $ BadFormException @C (mkList e)

        entry $ bindMatch "test:dir:keep" $ nil_ $ \case
          [] -> SC.bind "test:dir:keep" (mkBool True)
          e -> throwIO $ BadFormException @C (mkList e)

        entry $ bindMatch "test:ncq:fuckup-recovery1" $ nil_ $ \_ -> do
          debug $ "test:ncq:fuckup-recovery1"
          runTest testNCQFuckupRecovery1

        entry $ bindMatch "test:ncq:long-write" $ nil_ $ \case
          [ LitIntVal n ] -> runTest $ testNCQLongWrite (fromIntegral n)
          e -> throwIO $ BadFormException @C (mkList e)

        entry $ bindMatch "test:ncq:long-write-read" $ nil_ $ \case
          [ LitIntVal n ] -> runTest $ testNCQLongWriteRead (fromIntegral n)
          e -> throwIO $ BadFormException @C (mkList e)

        entry $ bindMatch "test:ncq:test-simple1" $ nil_ $ \case
          [] -> runTest $ testNCQSimple1
          e -> throwIO $ BadFormException @C (mkList e)

        entry $ bindMatch "test:ncq:test-simple2" $ nil_ $ \case
          [ LitIntVal n ] -> runTest $ testNCQSimple2 (fromIntegral n)
          e -> throwIO $ BadFormException @C (mkList e)

        entry $ bindMatch "test:ncq:test1" $ nil_ $ \case
          [ LitIntVal n ] -> do
            debug $ "ncq:test1" <+> pretty n
            runTest $ testNCQ1 (fromIntegral n)

          e -> throwIO $ BadFormException @C (mkList e)

        entry $ bindMatch "test:ncq:refs1" $ nil_ $ \case
          [ LitIntVal n ] -> do
            debug $ "ncq:refs1" <+> pretty n
            runTest $ testNCQRefs1 (fromIntegral n)

          e -> throwIO $ BadFormException @C (mkList e)

        entry $ bindMatch "test:ncq:tree1" $ nil_ $ \case
          [ LitIntVal n ] -> do
            debug $ "ncq:tree1" <+> pretty n
            runTest $ testNCQTree1 (fromIntegral n)

          e -> throwIO $ BadFormException @C (mkList e)

        entry $ bindMatch "test:ncq:concurrent1" $ nil_ $ \case
          [ LitIntVal tn, LitIntVal n ] -> do
            debug $ "ncq:concurrent1" <+> pretty tn <+> pretty n
            runTest $ testNCQConcurrent1 False ( fromIntegral tn) (fromIntegral n)

          e -> throwIO $ BadFormException @C (mkList e)


        entry $ bindMatch "test:ncq:concurrent2" $ nil_ $ \case
          [ LitIntVal tn, LitIntVal times, LitIntVal n ] -> do
            debug $ "ncq:concurrent2" <+> pretty tn <+> pretty n
            runTest $ testNCQ2Concurrent2  (fromIntegral tn) (fromIntegral times) (fromIntegral n)

          e -> throwIO $ BadFormException @C (mkList e)

        entry $ bindMatch "test:ncq:concurrent1:wo" $ nil_ $ \case
          [ LitIntVal tn, LitIntVal n ] -> do
            debug $ "ncq:concurrent1" <+> pretty tn <+> pretty n
            runTest $ testNCQConcurrent1 True ( fromIntegral tn) (fromIntegral n)

          e -> throwIO $ BadFormException @C (mkList e)

        entry $ bindMatch "test:ncq:test-lock" $ nil_ $ \case
          [ ] -> do
            runTest $ \TestEnv{..} -> do
              debug $ "test:ncq:test-lock" <+> pretty testEnvDir

              let ncq1 = testEnvDir </> "ncq1"

              flip runContT pure do

                pause @'Seconds 2
                r1 <- ContT $ withAsync do
                        withNCQ id ncq1 $ \_ -> do
                          forever $ pause @'Seconds 1

                -- link r1

                sto2 <- ContT $ withNCQ id ncq1

                result <- poll r1

                notice $ viaShow result

                case result of
                  Just Left{} -> none
                  _ ->  liftIO $ assertBool "must be (Left _)" False

          e -> throwIO $ BadFormException @C (mkList e)

        entry $ bindMatch "test:ncq2:merge1" $ nil_ $ \case
          [ LitIntVal n ] -> do
            runTest $ testNCQ2Merge1 (fromIntegral n)

          e -> throwIO $ BadFormException @C (mkList e)

        entry $ bindMatch "test:ncq2:concurrent1" $ nil_ $ \case
          [ LitIntVal tn, LitIntVal n ] -> do
            debug $ "ncq2:concurrent1" <+> pretty tn <+> pretty n
            runTest $ testNCQ2Concurrent1 False ( fromIntegral tn) (fromIntegral n)

          e -> throwIO $ BadFormException @C (mkList e)

        entry $ bindMatch "test:ncq2:simple1" $ nil_ $ \e -> do
            runTest (testNCQ2Simple1 e)

        entry $ bindMatch "test:ncq2:lookup1" $ nil_ $ \e -> do
            runTest (testNCQ2Lookup1 e)

        entry $ bindMatch "test:ncq2:sweep1" $ nil_ $ \e -> do
            runTest (testNCQ2Sweep1 e)

        entry $ bindMatch "test:ncq2:kill1" $ nil_ $ \e -> do
            runTest (testNCQ2Kill1 e)

        entry $ bindMatch "test:ncq2:sweep2" $ nil_ $ \e -> do
            runTest (testNCQ2Sweep2 e)

        entry $ bindMatch "test:ncq2:repair1" $ nil_ $ const $ do
            runTest testNCQ2Repair1

        entry $ bindMatch "test:ncq2:filefastcheck" $ nil_ $ \case
          [ StringLike fn ] -> do
            ncqFileFastCheck fn

          e -> throwIO $ BadFormException @C (mkList e)


        entry $ bindMatch "test:ncq2:wtf1" $ nil_ $ const do
          runTest $ \TestEnv{..} -> do
            let dir = testEnvDir
            r1 <- ncqWithStorage dir $ \sto -> do
              h <- ncqPutBS sto (Just B) Nothing "JOPAKITAPECHENTRESKI"
              loc <- ncqLocate2 sto h `orDie` "not found shit"
              let re@(k,r) = ncqEntryUnwrap sto $ ncqGetEntryBS sto loc
              notice $ pretty "MEM" <+> pretty (ncqEntrySize loc) <+> pretty (coerce @_ @HashRef k) <+> viaShow r
              pure re

            ncqWithStorage dir $ \sto -> do
              let (k,v) = r1
              loc <- ncqLocate2 sto (coerce k) `orDie` "not found shit"
              let s0 = ncqGetEntryBS sto loc
              let (k1,r1) = ncqEntryUnwrap sto s0
              notice $  "FOSSIL:" <+> pretty (ncqEntrySize loc) <+> pretty (coerce @_ @HashRef k1) <+> viaShow r1
              assertBool "written-same" (r1 == v && k == k1)



        entry $ bindMatch "test:ncq2:scan-index" $ nil_ \case
          [ StringLike dir, HashLike item ] -> do
            notice $ "SCAN DIR" <+> pretty dir <+> pretty item

            ncqWithStorage dir $ \sto@NCQStorage2{..} -> do

              -- let d = N2.ncqGetFileName sto ""

              -- files <- dirFiles d <&> List.filter (List.isSuffixOf ".cq")

              -- files <- N2.ncqListTrackedFiles sto

              tracked <- N2.ncqListTrackedFiles sto

              for_ tracked $ \(k,_,_) -> do

                let indexFile = N2.ncqGetFileName sto (toFileName (IndexFile k))

                (idxBs, idxNway) <- liftIO (nwayHashMMapReadOnly indexFile)
                                     >>= orThrow (NCQStorageCantMapFile indexFile)


                notice $ "scan file" <+> pretty indexFile

                stat <- liftIO $ PFS.getFileStatus indexFile
                -- -- FIXME: maybe-creation-time-actually
                let ts = posixToTimeSpec $ PFS.modificationTimeHiRes stat

                nwayHashScanAll idxNway idxBs $ \_ k v -> do
                  when (coerce k == item ) do

                    let off = fromIntegral $ N.word64 (BS.take 8 v)
                    let size = fromIntegral $ N.word32 (BS.take 4 (BS.drop 8 v))

                    notice $ yellow "found"
                                 <+> pretty (fromString @FileKey indexFile)
                                 <+> pretty (fromIntegral @_ @Word64 ts)
                                 <+> pretty (off,size,item)
                                 <+> pretty (foldMap (`showHex` "") (BS.unpack v) )

              -- datBs <- liftIO $ mmapFileByteString dataFile Nothing


            none

          e -> throwIO (BadFormException (mkList e))

        entry $ bindMatch "test:ncq2:del1" $ nil_ $ \syn -> do

          runTest $ \TestEnv{..} -> do
            g <- liftIO MWC.createSystemRandom
            let dir = testEnvDir

            let (_, argz) = splitOpts [] syn
            let n = headDef 10000 [ fromIntegral x | LitIntVal x <- argz ]

            thashes <- newTVarIO mempty

            ncqWithStorage dir $ \sto@NCQStorage2{..} -> do

              notice $ "write+immediate delete" <+> pretty n <+> "records"

              hashes <- replicateM n do

                h <- ncqPutBS sto (Just B) Nothing =<< genRandomBS g (64*1024)
                ncqDelEntry sto h

                t <- (ncqLocate2 sto h <&> fmap (N2.ncqIsTomb sto))
                       >>= orThrowUser ("missed" <+> pretty h)

                assertBool "tomb/1" t

                pure h


              pause @'Seconds 5

              atomically $ writeTVar thashes (HS.fromList hashes)

              flip runContT pure $ callCC \exit -> do

                for_ hashes $ \h -> do
                  l <- lift (ncqLocate2 sto h)
                         >>= orThrowUser ("missed" <+> pretty h)

                  unless (N2.ncqIsTomb sto l) do
                    let (k,e') =  ncqEntryUnwrap sto (ncqGetEntryBS sto l)

                    e <- orThrowUser "bad entry" e'
                    err $ pretty l
                    err $ "WTF?" <+> pretty (coerce @_ @HashRef k) <+> pretty h <+> viaShow (fst e)
                    lfs <- readTVarIO ncqTrackedFiles

                    for_ lfs $ \TrackedFile{..} -> do
                      npe <- readTVarIO tfCached <&> isNotPending
                      err $ "FILE" <+> pretty npe <+> pretty tfKey

                    exit ()

            ncqWithStorage dir $ \sto -> do
              -- notice "check deleted"
              hashes  <- readTVarIO  thashes

              for_ hashes $ \h -> do

                ncqLocate2 sto h >>= \case
                  Nothing -> notice $ "not-found" <+> pretty h
                  Just loc -> do

                        what <- (ncqLocate2 sto h <&> fmap (ncqGetEntryBS sto))
                                   >>= orThrowUser "NOT FOUND"

                        let (k,wtf) = ncqEntryUnwrap sto what
                        let tomb = N2.ncqIsTomb sto loc

                        -- debug $ pretty (coerce @_ @HashRef k) <+> viaShow wtf <+> pretty tomb

                        assertBool (show $ "tomb/3" <+> pretty h) tomb


        entry $ bindMatch "test:ncq2:concurrent:write:simple1" $ nil_ $ \case
          [ LitIntVal tn, LitIntVal n ] -> do
            runTest $ testNCQ2ConcurrentWriteSimple1 ( fromIntegral tn) (fromIntegral n)

          e -> throwIO $ BadFormException @C (mkList e)

        entry $ bindMatch "test:ncq2:ema" $ nil_ $ const do
          notice "test:ncq2:ema"
          runTest $ \TestEnv{..} -> do
            g <- liftIO MWC.createSystemRandom
            let dir = testEnvDir </> "ncq1"
            let n = 50000
            ncqWithStorage dir $ \sto -> do
              replicateM_ n do
                ncqPutBS sto (Just B) Nothing =<< genRandomBS g (256*1024)

              notice $ "written" <+> pretty n

              pause @'Seconds 120

        entry $ bindMatch "test:filter:emulate-1" $ nil_ $ \case
          [ LitIntVal n ] -> runTest $ testFilterEmulate1 False (fromIntegral n)
          e -> throwIO $ BadFormException @C (mkList e)

        entry $ bindMatch "test:filter:emulate:merged" $ nil_ $ \case
          [ LitIntVal n ] -> runTest $ testFilterEmulate1 True (fromIntegral n)
          e -> throwIO $ BadFormException @C (mkList e)


        hidden do
          internalEntries
          entry $ bindMatch "#!" $ nil_ $ const none

  setupLogger

  argz <- liftIO getArgs

  forms <- parseTop (unlines $ unwords <$> splitForms argz)
           & either  (error.show) pure

  atomically $ writeTVar tvd dict

  (runEval tvd forms >>= eatNil display)
    `finally` flushLoggers


