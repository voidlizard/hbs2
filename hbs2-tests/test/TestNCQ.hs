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
import HBS2.Clock
import HBS2.Merkle
import HBS2.Polling

import HBS2.Storage
import HBS2.Storage.Simple
import HBS2.Storage.Operations.ByteString

import HBS2.System.Logger.Simple.ANSI

import HBS2.Storage.NCQ
import HBS2.Storage.NCQ2 as N2
import HBS2.Data.Log.Structured.NCQ

import HBS2.CLI.Run.Internal.Merkle

import Data.Config.Suckless.Syntax
import Data.Config.Suckless.Script as SC
import Data.Config.Suckless.System

import DBPipe.SQLite hiding (field)

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


testNCQ2Simple1 :: MonadUnliftIO m
         => TestEnv
         -> m ()

testNCQ2Simple1 TestEnv{..} = do
  debug "testNCQ2Simple1"
  let tmp = testEnvDir
  let ncqDir   = tmp
  q <- newTQueueIO

  g <- liftIO MWC.createSystemRandom

  bz <- replicateM 30000 $ liftIO do
          n <- (`mod` (256*1024)) <$> uniformM @Int g
          uniformByteStringM n g

  ncqWithStorage ncqDir $ \sto -> liftIO do
    for bz $ \z ->  do
      h <- ncqPutBS sto (Just B) Nothing z
      atomically $ writeTQueue q h
      found <- ncqLocate2 sto h <&> maybe (-1) ncqEntrySize
      assertBool (show $ "found-immediate" <+> pretty h) (found > 0)

  ncqWithStorage ncqDir $ \sto -> liftIO do
    hashes <- atomically (STM.flushTQueue q)
    for_ hashes $ \ha -> do
      found <- ncqLocate2 sto ha <&> maybe (-1) ncqEntrySize
      assertBool (show $ "found-immediate" <+> pretty ha) (found > 0)
      -- debug $ fill 44 (pretty ha) <+> fill 8 (pretty found)


testFilterEmulate1 :: MonadUnliftIO m
         => Int
         -> TestEnv
         -> m ()

testFilterEmulate1 n TestEnv{..} = do
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

    for_ [1..4] $ \i -> do

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
    written <- N2.ncqListTrackedFiles sto
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

  ncqWithStorage ncqDir $ \sto -> liftIO do
    hashes <- atomically (STM.flushTQueue q)
    for_ hashes $ \ha -> do
      found <- ncqLocate2 sto ha <&> maybe (-1) ncqEntrySize
      none
      -- assertBool (show $ "found-immediate" <+> pretty ha) (found > 0)
      -- debug $ fill 44 (pretty ha) <+> fill 8 (pretty found)

testNCQ2Concurrent1 :: MonadUnliftIO m
         => Bool
         -> Int
         -> Int
         -> TestEnv
         -> m ()

testNCQ2Concurrent1 noRead tn n TestEnv{..} = flip runContT pure do

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

  notice "NO SHIT"

  -- setLoggingOff @DEBUG

  for_ [1..tn] $ \tnn -> do

    ncq1 <- ncqStorageOpen2 ncqDir (\x -> x { ncqFsync = 64^(1024^2) } )
    w <- ContT $ withAsync (ncqStorageRun2 ncq1)

    (t,_) <- timeItT $ liftIO do

      pooledForConcurrentlyN_ tnn  fnv $ \(n,ha,_) -> do
        co <- BS.readFile n
        ncqPutBS ncq1 (Just B) Nothing co

      ncqStorageStop2 ncq1
      performMajorGC
      wait w
      rm ncqDir

    let tt = realToFrac @_ @(Fixed E2) t
    let speed = ((ssz / (1024 **2)) / t) & realToFrac @_ @(Fixed E2)
    notice $ pretty tnn <+> pretty tt <+> pretty speed


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

        entry $ bindMatch "test:ncq2:concurrent1" $ nil_ $ \case
          [ LitIntVal tn, LitIntVal n ] -> do
            debug $ "ncq2:concurrent1" <+> pretty tn <+> pretty n
            runTest $ testNCQ2Concurrent1 False ( fromIntegral tn) (fromIntegral n)

          e -> throwIO $ BadFormException @C (mkList e)

        entry $ bindMatch "test:ncq2:simple1" $ nil_ $ const $ do
            runTest testNCQ2Simple1

        entry $ bindMatch "test:ncq2:repair1" $ nil_ $ const $ do
            runTest testNCQ2Repair1

        entry $ bindMatch "test:ncq2:filefastcheck" $ nil_ $ \case
          [ StringLike fn ] -> do
            ncqFileFastCheck fn

          e -> throwIO $ BadFormException @C (mkList e)

        entry $ bindMatch "test:ncq2:concurrent:write:simple1" $ nil_ $ \case
          [ LitIntVal tn, LitIntVal n ] -> do
            runTest $ testNCQ2ConcurrentWriteSimple1 ( fromIntegral tn) (fromIntegral n)

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


        entry $ bindMatch "test:filter:emulate-1" $ nil_ $ \case
          [ LitIntVal n ] -> runTest $ testFilterEmulate1 (fromIntegral n)
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



