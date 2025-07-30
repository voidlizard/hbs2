{-# Language RecordWildCards #-}
module NCQ3 where

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
import HBS2.Storage.NCQ3
import HBS2.Storage.NCQ3.Internal.Files
import HBS2.Storage.NCQ3.Internal.Index
import HBS2.Storage.NCQ3.Internal.Fossil

import HBS2.System.Logger.Simple.ANSI

import HBS2.Data.Log.Structured.SD
import HBS2.Data.Log.Structured.NCQ

import HBS2.CLI.Run.Internal.Merkle

import Data.Config.Suckless.Syntax
import Data.Config.Suckless.Script as SC
import Data.Config.Suckless.System

import NCQTestCommon

import Data.Generics.Labels
import Lens.Micro.Platform
import Network.ByteOrder qualified as N
import System.TimeIt
import Data.Fixed
import Data.HashSet qualified as HS
import Data.HashMap.Strict qualified as HM
import Test.Tasty.HUnit
import Data.ByteString qualified as BS
import Data.Ord
import Data.Set qualified as Set
import System.Random.MWC as MWC
import Control.Concurrent.STM qualified as STM
import Data.List qualified as List
import Control.Monad.Trans.Cont
import System.IO.Temp qualified as Temp
import UnliftIO
import UnliftIO.IO.File
import UnliftIO.IO as IO
import UnliftIO.Directory

{-HLINT ignore "Functor law"-}

ncq3Tests :: forall m . MonadUnliftIO m => MakeDictM C m ()
ncq3Tests = do
  entry $ bindMatch "test:ncq3:start-stop" $ nil_ $ \e ->do
      let (opts,args) = splitOpts [] e
      let num = headDef 1000 [ fromIntegral n | LitIntVal n <- args ]
      g <- liftIO MWC.createSystemRandom
      runTest $ \TestEnv{..} -> do
        ncqWithStorage3 testEnvDir $ \sto -> do
           notice "start/stop ncq3 storage / write 1000 blocks"
           replicateM_ num do
             n <- liftIO $ uniformRM (1024, 256*1024) g
             bs <- liftIO $ genRandomBS g n
             ncqPutBS sto (Just B) Nothing bs

  entry $ bindMatch "test:ncq3:write-reopen" $ nil_ $ \e ->do
      let (opts,args) = splitOpts [] e
      let num = headDef 1000 [ fromIntegral n | LitIntVal n <- args ]
      g <- liftIO MWC.createSystemRandom
      runTest $ \TestEnv{..} -> do

        pending <- ncqWithStorage3 testEnvDir $ \sto -> do
           notice $ "write" <+> pretty num <+> "blocks"
           replicateM_ num do
             n <- liftIO $ uniformRM (1024, 256*1024) g
             bs <- liftIO $ genRandomBS g n
             ncqPutBS sto (Just B) Nothing bs

           fa <- readTVarIO (ncqState sto) <&> ncqStateFacts

           pure $ [ (ncqGetFileName sto (toFileName k),s) | P (PData k s) <- Set.toList fa ]
                           & maximumByMay (comparing snd)

        for_ pending $ \(dataFile,_) -> do
           n <- liftIO $ uniformRM (1, 16*1024) g
           bss <- liftIO $ genRandomBS g n
           notice $ "CORRUPTING PENDING FILE" <+> pretty n <+> pretty dataFile
           liftIO $ BS.appendFile dataFile bss

        notice $ "reopen"
        ncqWithStorage3 testEnvDir $ \sto -> do
          pause @'Seconds 2
          notice $ "done"


  entry $ bindMatch "test:ncq3:write:simple" $ nil_ $ \e ->do
      let (opts,args) = splitOpts [] e
      let num = headDef 1000 [ fromIntegral n | LitIntVal n <- args ]
      g <- liftIO MWC.createSystemRandom

      w1 <- newTVarIO 0
      f1 <- newTVarIO 0
      m1 <- newTVarIO 0

      runTest $ \TestEnv{..} -> do
        hq <- newTQueueIO
        ncqWithStorage3 testEnvDir $ \sto -> do
           notice $ "write/lookup" <+> pretty num
           replicateM_ num do
             n <- liftIO $ uniformRM (1024, 256*1024) g
             bs <- liftIO $ genRandomBS g n
             h <- ncqPutBS sto (Just B) Nothing bs
             found <- ncqLocate sto h <&> isJust
             liftIO $ assertBool (show $ "found" <+> pretty h) found
             atomically do
              writeTQueue hq h
              modifyTVar w1 succ

        ncqWithStorage3 testEnvDir $ \sto -> do
          notice $ "reopen/lookup" <+> pretty num
          hh <- atomically $ STM.flushTQueue hq

          for_ hh $ \h -> do
             found <- ncqLocate sto h <&> isJust
             atomically do
               if found then do
                  modifyTVar f1 succ
               else do
                  modifyTVar m1 succ

        w <- readTVarIO w1
        f <- readTVarIO f1
        m <- readTVarIO m1

        notice $ "done" <+> pretty w <+> pretty f <+> pretty m

        liftIO $ assertBool (show $ "all-found" <+> pretty w) (f == w && m == 0)

  entry $ bindMatch "test:ncq3:seek" $ nil_ $ \case
    [ StringLike p, HashLike h ] -> do

        files <- dirFiles p  <&> filter (List.isPrefixOf "i-" .takeBaseName)

        for_ files $ \f -> do
          (bs,nw) <- nwayHashMMapReadOnly f >>= orThrowUser ("Can't mmap" <+> pretty f)

          nwayHashScanAll nw bs $ \_ k v -> do
            unless (coerce k == emptyKey) do
              let e = unpackIndexEntry v
              notice $ "found:" <+> pretty (coerce @_ @HashRef k) <+> viaShow e

    e -> throwIO $ BadFormException @C (mkList e)


  entry $ bindMatch "test:ncq3:merge" $ nil_ \e -> do

      let (opts,args) = splitOpts [] e
      let num = headDef 1000 [ fromIntegral n | LitIntVal n <- args ]
      g <- liftIO MWC.createSystemRandom

      runTest $ \TestEnv{..} -> do
        ncqWithStorage3 testEnvDir $ \sto@NCQStorage3{..} -> do
           notice $ "write" <+> pretty num
           hst <- newTVarIO ( mempty :: HashSet HashRef )
           replicateM_ num do
             n <- liftIO $ uniformRM (1024, 64*1024) g
             bs <- liftIO $ genRandomBS g n
             h <- ncqPutBS sto (Just B) Nothing bs
             atomically $ modifyTVar hst (HS.insert h)

           idx <- readTVarIO ncqState
                     <&> ncqStateIndex
                     <&> fmap (IndexFile . snd)

           r <- ncqFindMinPairOf sto idx
           notice $ pretty r

           fix $ \loop -> do
             notice "compacting once"
             w <- ncqIndexCompactStep sto
             when w loop

           nstate <- readTVarIO ncqState

           notice $ "new state" <> line <> pretty nstate

           hss <- readTVarIO hst

           for_ hss $ \h -> do
              found <- ncqLocate sto h <&> isJust
              liftIO $ assertBool (show $ "found" <+> pretty h) found


  entry $ bindMatch "test:ncq3:sweep" $ nil_ \e -> do

      t0 <- getTimeCoarse

      let (opts,args) = splitOpts [] e
      let num = headDef 1000 [ fromIntegral n | LitIntVal n <- args ]
      g <- liftIO MWC.createSystemRandom

      runTest $ \TestEnv{..} -> do
        ncqWithStorage3 testEnvDir $ \sto@NCQStorage3{..} -> flip runContT pure do

           hst <- newTVarIO ( mempty :: HashSet HashRef )
           lostt <- newTVarIO 0
           req   <- newTVarIO 0

           ContT $ withAsync $ forever do
            pause @'Seconds 20
            t <- getTimeCoarse <&> sec2 . (*1e-9) . realToFrac . toNanoSecs . (+ (-t0))
            l <- readTVarIO lostt
            r <- readTVarIO req
            pp <- readTVarIO ncqStateUse <&> HM.size
            let c = if l > 0 then red else id
            debug $ "Elapsed" <+> pretty t <+> pretty pp <+> pretty r <+> c (pretty l)

           ContT $ withAsync $ forever do
            p <- liftIO $ uniformRM (0, 0.75) g
            pause @'Seconds (realToFrac p)
            hh <- readTVarIO hst

            when (HS.size hh > 0) do

              i <- liftIO $ uniformRM (0, HS.size hh - 1) g
              let hi = HS.toList hh !! i
              found <- ncqLocate sto hi <&> isJust
              atomically $ modifyTVar req succ

              unless found do
                err $ red "NOT FOUND" <+> pretty hi
                atomically $ modifyTVar lostt succ

           notice $ "write" <+> pretty num
           replicateM_ num do
             n <- liftIO $ uniformRM (1024, 64*1024) g
             bs <- liftIO $ genRandomBS g n
             h <- lift  $ ncqPutBS sto (Just B) Nothing bs
             atomically $ modifyTVar hst (HS.insert h)

           pause @'Seconds 180

           notice "check after compaction"

           h1 <- readTVarIO hst

           for_ h1 $ \h -> lift do
              found <- ncqLocate sto h <&> isJust
              liftIO $ assertBool (show $ "found" <+> pretty h) found



  entry $ bindMatch "test:ncq3:merge:fossil" $ nil_ \e -> do

      let (opts,args) = splitOpts [] e
      let num = headDef 1000 [ fromIntegral n | LitIntVal n <- args ]
      g <- liftIO MWC.createSystemRandom

      runTest $ \TestEnv{..} -> do
        ncqWithStorage3 testEnvDir $ \sto@NCQStorage3{..} -> flip runContT pure do

           hst <- newTVarIO ( mempty :: HashSet HashRef )

           notice $ "write" <+> pretty num
           replicateM_ num do
             n <- liftIO $ uniformRM (1024, 64*1024) g
             bs <- liftIO $ genRandomBS g n
             h <- lift  $ ncqPutBS sto (Just B) Nothing bs
             atomically $ modifyTVar hst (HS.insert h)

           lift (ncqFossilMergeStep sto)

           notice "merge done"

           pause @'Seconds 180

           notice "check after compaction"

           h1 <- readTVarIO hst

           for_ h1 $ \h -> lift do
              found <- ncqLocate sto h <&> isJust
              liftIO $ assertBool (show $ "found" <+> pretty h) found

  entry $ bindMatch "test:ncq3:concurrent1" $ nil_ $ \case
    [ LitIntVal tn, LitIntVal n ] -> do
      debug $ "ncq2:concurrent1" <+> pretty tn <+> pretty n
      runTest $ testNCQ3Concurrent1 False ( fromIntegral tn) (fromIntegral n)
    e -> throwIO $ BadFormException @C (mkList e)

  entry $ bindMatch "test:ncq3:lookup1" $ nil_ $ \e -> do
      runTest (testNCQ3Lookup1 e)

testNCQ3Concurrent1 :: MonadUnliftIO m
         => Bool
         -> Int
         -> Int
         -> TestEnv
         -> m ()

testNCQ3Concurrent1 noRead tn n TestEnv{..} = flip runContT pure do

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
    testWriteNThreads3 ncqDir tnn n


testWriteNThreads3 :: forall g m . (MonadUnliftIO m)
                  => FilePath
                  -> Int
                  -> Int
                  -> m ()
testWriteNThreads3 ncqDir tnn n = do

    g <- liftIO MWC.createSystemRandom

    wtf <- liftIO getPOSIXTime <&> show . round

    t0 <- getTimeCoarse

    w <- ncqWithStorage3 (ncqDir </> show tnn)  $ \sto -> do
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



testNCQ3Lookup1:: forall c m . (MonadUnliftIO m, IsContext c)
         => [Syntax c]
         -> TestEnv
         -> m ()

testNCQ3Lookup1 syn TestEnv{..} = do
  debug $ "testNCQ3Lookup1" <+> pretty syn
  let tmp = testEnvDir
  let ncqDir   = tmp
  q <- newTQueueIO

  g <- liftIO MWC.createSystemRandom

  let (opts, argz) = splitOpts [("-m",0)] syn

  let n = headDef 100000 [ fromIntegral x | LitIntVal x <- argz ]
  let nt = max 2 . headDef 1 $ [ fromIntegral x | LitIntVal x <- drop 1 argz ]
  let nl = headDef 3 $ [ fromIntegral x | LitIntVal x <- drop 2 argz ]
  let r = (64*1024, 256*1024)

  let merge = headDef False [ True | ListVal [StringLike "-m"] <- opts ]

  notice $ "insert" <+> pretty n <+> "random blocks of size" <+> parens (pretty r) <+> pretty opts

  thashes <- newTQueueIO

  sizes <- liftIO $ replicateM n (uniformRM r g )

  res <- newTQueueIO

  ncqWithStorage3 ncqDir $ \sto -> liftIO do
    pooledForConcurrentlyN_ 8  sizes $ \size -> do
      z <- genRandomBS g size
      h <- ncqPutBS sto (Just B) Nothing z
      atomically $ writeTQueue thashes h

    hs <- atomically $ STM.flushTQueue thashes

    when merge do
      notice "COMPACT INDEX"
      ncqIndexCompactFull sto

    idx <- readTVarIO (ncqState sto) <&> List.length . view #ncqStateIndex

    replicateM_ nl do

      tfound <- newTVarIO 0

      t0 <- getTimeCoarse

      liftIO $ pooledForConcurrentlyN_ nt hs $ \h -> do
        found <- ncqLocate sto h <&> isJust
        when found do
          atomically $ modifyTVar' tfound succ

      t1 <- getTimeCoarse

      let dt = realToFrac (toNanoSecs (t1 - t0)) / 1e9 :: Fixed E3
      atomically $ writeTQueue res dt

      found <- readTVarIO tfound

      notice $ "scan all files" <+> pretty idx <+> pretty dt <+> pretty found

    m <- atomically (STM.flushTQueue res)
          <&> List.sort
          <&> \x -> atDef 0 x (List.length x `quot` 2)

    notice $ "median" <+> pretty m




