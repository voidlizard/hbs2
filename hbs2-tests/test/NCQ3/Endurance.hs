{-# Language AllowAmbiguousTypes #-}
{-# Language RecordWildCards #-}
{-# Language MultiWayIf #-}
module NCQ3.Endurance where


import HBS2.Prelude.Plated
import HBS2.OrDie
import HBS2.Hash
import HBS2.Data.Types.Refs
import HBS2.Misc.PrettyStuff
import HBS2.Clock
import HBS2.Merkle
import HBS2.Polling
import HBS2.Peer.Proto.AnyRef

import HBS2.Storage
import HBS2.Storage.Simple
import HBS2.Storage.Operations.ByteString
import HBS2.Storage.NCQ3
import HBS2.Storage.NCQ3.Internal.Prelude
import HBS2.Storage.NCQ3.Internal.Files
import HBS2.Storage.NCQ3.Internal.Index
import HBS2.Storage.NCQ3.Internal.Fossil
import HBS2.Storage.NCQ3.Internal.State
import HBS2.Storage.NCQ3.Internal.Sweep
import HBS2.Storage.NCQ3.Internal

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
import Data.Either
import Data.HashPSQ qualified as HPSQ
import Data.HashMap.Strict qualified as HM
import Test.Tasty.HUnit
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Ord
import Data.Set qualified as Set
import System.Random.MWC as MWC
import Control.Concurrent.STM qualified as STM
import Data.List qualified as List
import Control.Monad.Trans.Cont
import Control.Monad.Except
import System.IO.Temp qualified as Temp
import System.Environment (getExecutablePath)
import System.Process.Typed as PT
import System.IO qualified as IO
import System.IO.Error
import System.Posix.IO qualified as Posix
import GHC.IO.Handle qualified as GHC
import System.Random.Stateful
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import UnliftIO
import UnliftIO.IO.File
import UnliftIO.IO as IO
import UnliftIO.Directory

import Streaming.Prelude qualified as S

{-HLINT ignore "Functor law"-}


data EnduranceFSM =
    EnduranceIdle
  | EndurancePutBlk
  | EnduranceHasBlk
  | EnduranceGetBlk
  | EnduranceHasSeedBlk
  | EnduranceDelBlk
  | EndurancePutRef
  | EnduranceGetRef
  | EnduranceDelRef
  | EnduranceStorm
  | EnduranceCalm
  | EnduranceKill
  | EnduranceExit
  | EnduranceMerge
  | EnduranceCompact
  | EnduranceSweep
  | EnduranceStop

buildCDF :: [(s, Double)] -> (V.Vector s, U.Vector Double)
buildCDF xs =
  let states = V.fromList (map fst xs)
      cdf    = U.fromList (scanl1 (+) (map snd xs))
  in (states, cdf)

-- выборка по бинарному поиску
sampleState :: MonadIO m => GenIO -> (V.Vector s, U.Vector Double) -> m s
sampleState g (states,cdf) = do
  let total = U.last cdf
  r <- liftIO $ uniformRM (0,total) g
  pure $ states V.! binarySearch cdf r

binarySearch :: U.Vector Double -> Double -> Int
binarySearch vec x = go 0 (U.length vec - 1)
  where
    go l r
      | l >= r    = l
      | otherwise =
          let mid = (l+r) `div` 2
          in if x <= vec U.! mid
               then go l mid
               else go (mid+1) r

-- | Pick a random key from a HashPSQ
getRandomFromPSQ :: forall k p v m . (MonadIO m, Hashable k, Ord k, Ord p)
                 => MWC.GenIO
                 -> TVar (HPSQ.HashPSQ k p v)
                 -> m (Maybe k)
getRandomFromPSQ g tvar = do
  psq <- readTVarIO tvar
  let n = HPSQ.size psq
  if n == 0
    then pure Nothing
    else do
      dropn <- liftIO $ uniformRM (0, n-1) g
      let e = fmap (view _1) . headMay $ drop dropn $ HPSQ.toList psq
      pure e


-- | Deleted = Left (), Alive = Right size
type BlockState = Either () Integer

-- | Deleted = Left (), Alive = Right destination
type RefState   = Either () HashRef

validateTestResult :: forall m . MonadUnliftIO m => FilePath -> m ()
validateTestResult logFile = do

  blocks <- newTVarIO (mempty :: HM.HashMap HashRef BlockState)
  refs   <- newTVarIO (mempty :: HM.HashMap HashRef RefState)

  let dict = makeDict @C do

        -- block-written: remember size
        entry $ bindMatch "block-written" $ nil_ \case
          [ HashLike h, LitIntVal n ] ->
            atomically $ modifyTVar blocks (HM.insert h (Right n))
          _ -> none

        -- block-deleted: mark deleted
        entry $ bindMatch "block-deleted" $ nil_ \case
          [ HashLike h ] ->
            atomically $ modifyTVar blocks (HM.insert h (Left ()))
          _ -> none

        entry $ bindMatch "has-seed-block-result" $ nil_ \case
          [ HashLike _, LitIntVal _ ] -> none
          [ HashLike h]  -> err $ red "missed seed block (2)" <+> pretty h
          _ -> none

        -- has-block-result
        entry $ bindMatch "has-block-result" $ nil_ \case
          [ HashLike h, LitIntVal n ] -> do
            really <- readTVarIO blocks <&> HM.lookup h
            case really of
              Just (Right n0) | n0 == n -> none
              Just (Left ())             -> err $ red "has-block says present, but deleted" <+> pretty h
              _                          -> err $ red "has-block mismatch" <+> pretty h

          [ HashLike h ] -> do
            really <- readTVarIO blocks <&> HM.lookup h
            case really of
              Just (Left ()) -> none
              Nothing        -> none
              Just (Right _) -> err $ red "has-block says missing, but we have" <+> pretty h
          _ -> none

        -- get-block-result
        entry $ bindMatch "get-block-result" $ nil_ \case
          [ HashLike h, HashLike _hx ] -> do
            really <- readTVarIO blocks <&> HM.lookup h
            case really of
              Just (Right _) -> none
              Just (Left ()) -> err $ red "get-block returned data for deleted block" <+> pretty h
              Nothing        -> err $ red "get-block returned data for unknown block" <+> pretty h

          [ HashLike h ] -> do
            really <- readTVarIO blocks <&> HM.lookup h
            case really of
              Just (Right _) -> err $ red "get-block missing, but expected present" <+> pretty h
              _              -> none
          _ -> none

        -- ref-updated
        entry $ bindMatch "ref-updated" $ nil_ \case
          [ HashLike h, HashLike hdest ] ->
            atomically $ modifyTVar refs (HM.insert h (Right hdest))
          _ -> none

        -- get-ref-result
        entry $ bindMatch "get-ref-result" $ nil_ \case
          [ HashLike h, HashLike hdest ] -> do
            really <- readTVarIO refs <&> HM.lookup h
            case really of
              Just (Right h0) | h0 == hdest -> none
              Just (Left ()) -> err $ red "get-ref returned value for deleted ref" <+> pretty h
              _ -> err $ red "get-ref mismatch" <+> pretty h <+> "got" <+> pretty hdest

          [ HashLike h ] -> do
            really <- readTVarIO refs <&> HM.lookup h
            case really of
              Just (Left ()) -> none
              Nothing        -> none
              Just (Right _) -> err $ red "get-ref says missing, but we have" <+> pretty h
          _ -> none

        -- ref-deleted
        entry $ bindMatch "ref-deleted" $ nil_ \case
          [ HashLike h ] ->
            atomically $ modifyTVar refs (HM.insert h (Left ()))
          _ -> none

        entry $ bindMatch "compact" $ nil_ $ const none
        entry $ bindMatch "merge"   $ nil_ $ const none
        entry $ bindMatch "sweep"   $ nil_ $ const none


  -- читаем лог построчно и скармливаем dict
  rs <- lines <$> liftIO (IO.readFile logFile)
  for_ rs $ \s -> case parseTop s of
    Left{}  -> none
    Right syn -> void $ run dict syn

  -- финальная статистика
  bs <- readTVarIO blocks
  rs' <- readTVarIO refs
  notice $ green "validate done"
         <+> "blocks:" <+> pretty (length [() | Right _ <- HM.elems bs])
         <+> "deleted-blocks:" <+> pretty (length [() | Left () <- HM.elems bs])
         <+> "refs:" <+> pretty (length [() | Right _ <- HM.elems rs'])
         <+> "deleted-refs:" <+> pretty (length [() | Left () <- HM.elems rs'])


ncq3EnduranceTest :: forall m . MonadUnliftIO m => MakeDictM C m ()
ncq3EnduranceTest = do

  entry $ bindMatch "test:ncq3:endurance:inner" $ nil_ $ \syn -> do
    let (opts,args) = splitOpts [] syn
    path <- orThrowUser "path not set" $ headMay [ x | StringLike x <- args ]
    testEnduranceInner @C path

  entry $ bindMatch "test:ncq3:endurance" $ nil_ $ \syn -> do

    let dbl = \case
                 LitScientificVal x -> realToFrac x
                 LitIntVal x        -> realToFrac x
                 _ -> 0.00

    let int = \case
                 LitScientificVal x -> floor x
                 LitIntVal x        -> fromIntegral  x
                 _ -> 0

    wSeed     <-  int <$> lookupValueDef (mkInt 1000)      "w:seed"
    wWindow   <-  int <$> lookupValueDef (mkInt 100000)    "w:win"
    wIdle     <-  dbl <$> lookupValueDef (mkDouble 200.00) "w:idle"
    wIdleDef  <-  dbl <$> lookupValueDef (mkDouble   0.25) "w:idle:def"
    wMaxBlk   <-  int <$> lookupValueDef     (mkInt 65536) "w:maxblk"
    wPutBlk   <-  dbl <$> lookupValueDef (mkDouble  30.00) "w:putblk"
    wGetBlk   <-  dbl <$> lookupValueDef (mkDouble  30.00) "w:getblk"
    wHasBlk   <-  dbl <$> lookupValueDef (mkDouble  30.00) "w:hasblk"
    wDelBlk   <-  dbl <$> lookupValueDef (mkDouble   3.00) "w:delblk"
    wPutRef   <-  dbl <$> lookupValueDef (mkDouble   5.00) "w:putref"
    wGetRef   <-  dbl <$> lookupValueDef (mkDouble  10.00) "w:getref"
    wDelRef   <-  dbl <$> lookupValueDef (mkDouble   1.00) "w:delref"
    wStorm    <-  dbl <$> lookupValueDef (mkDouble   0.80) "w:storm"
    wStormMin <-  dbl <$> lookupValueDef (mkDouble   1.00) "w:stormmin"
    wStormMax <-  dbl <$> lookupValueDef (mkDouble  60.00) "w:stormmax"
    wCalm     <-  dbl <$> lookupValueDef (mkDouble  0.001) "w:calm"
    wKill     <-  dbl <$> lookupValueDef (mkDouble  0.00)  "w:kill"
    wExit     <-  dbl <$> lookupValueDef (mkDouble  0.001) "w:exit"
    wMerge    <-  dbl <$> lookupValueDef (mkDouble  0.005) "w:merge"
    wCompact  <-  dbl <$> lookupValueDef (mkDouble  0.005) "w:compact"
    wSweep    <-  dbl <$> lookupValueDef (mkDouble  0.005) "w:sweep"
    wNum      <-  int <$> lookupValueDef (mkInt     10000) "w:num"


    runTest \TestEnv{..} -> do
      g <- liftIO $ MWC.createSystemRandom

      let (opts,args) = splitOpts [] syn

      let n = headDef wNum [ fromIntegral x | LitIntVal x <- args ]

      rest      <- newTVarIO n
      blocks    <- newTVarIO ( HPSQ.empty :: HPSQ.HashPSQ HashRef Double () )
      seed      <- newTVarIO ( HPSQ.empty :: HPSQ.HashPSQ HashRef Double () )
      refs      <- newTVarIO ( HPSQ.empty :: HPSQ.HashPSQ HashRef Double HashRef )
      killed    <- newTVarIO 0
      stopped   <- newTVarIO 0
      merged    <- newTVarIO 0
      sweeped   <- newTVarIO 0
      compacted <- newTVarIO 0

      let getRandomBlock = liftIO $ getRandomFromPSQ g blocks
      let getRandomSeedBlock = liftIO $ getRandomFromPSQ g seed
      let getRandomRef = liftIO $ getRandomFromPSQ g refs

      let d = makeDict do

               entry $ bindMatch "ref-updated" $ nil_ \case
                         [HashLike h, HashLike r] -> do

                          w <- liftIO $ uniformRM (0,1.0) g

                          atomically do
                            modifyTVar refs (HPSQ.insert h w r)
                            size <- readTVar refs <&> HPSQ.size
                            when (size > wWindow ) do
                              modifyTVar refs HPSQ.deleteMin

                         _ -> none


               entry $ bindMatch "block-written" $ nil_ \case
                         [HashLike h, _] -> do

                          w <- liftIO $ uniformRM (0,1.0) g

                          atomically do
                            modifyTVar blocks (HPSQ.insert h w ())
                            size <- readTVar blocks <&> HPSQ.size
                            when (size > wWindow ) do
                              modifyTVar blocks HPSQ.deleteMin

                         _ -> none


      -- pI <- rublookupValue "endurance:idle"
      --
      debug $ red "pKill" <+> pretty wKill

      let actions = [ (EnduranceIdle,    wIdle)
                    , (EndurancePutBlk,  wPutBlk)
                    , (EnduranceGetBlk,  wGetBlk)
                    , (EnduranceHasSeedBlk, wHasBlk)
                    , (EnduranceHasBlk,  wHasBlk)
                    , (EnduranceDelBlk,  wDelBlk)
                    , (EndurancePutRef,  wPutRef)
                    , (EnduranceGetRef,  wGetRef)
                    , (EnduranceDelRef,  wDelRef)
                    , (EnduranceStorm,   wStorm)
                    , (EnduranceCalm,    wCalm)
                    , (EnduranceMerge,   wMerge)
                    , (EnduranceCompact, wCompact)
                    , (EnduranceSweep,   wSweep)
                    , (EnduranceKill,    wKill)
                    , (EnduranceExit,    wExit)
                    ]

      let dist = buildCDF actions   -- ← подготовили один раз

      let inner =  "test:ncq3:endurance:inner"
      self <- liftIO getExecutablePath
      let conf = proc self [ "debug on"
                           , "and"
                           , "test:ncq3:endurance:inner", testEnvDir
                           ] & setStdin createPipe & setStdout createPipe

      ncqWithStorage testEnvDir $ \sto -> do
        replicateM_ wSeed do
          n <- liftIO $ uniformRM (1, wMaxBlk) g
          bs <- liftIO $ LBS.fromStrict <$> genRandomBS g n
          putBlock (AnyStorage sto) bs >>= \case
            Just h -> atomically $ modifyTVar seed (HPSQ.insert (HashRef h) 1.0 ())
            Nothing -> err $ red "can't write seed block"

      ncqWithStorage testEnvDir $ \sto -> do
        seeds <- readTVarIO seed <&> HPSQ.toList
        for_ seeds $ \(h,_,_) -> do
          here <- hasBlock (AnyStorage sto) (coerce h)
          unless (isJust here) do
            err $ "missed seed block (1)" <+> pretty h

      let handler e = err (viaShow e) >> debug "RECOVERING" >> pause @'Seconds 3
      fix \recover -> handleAny (\e -> handler e >> recover) do

        flip runContT pure do
          p <- startProcess conf -- ContT $ withProcessWait conf
          storms <- newTQueueIO
          let inp  = getStdin p
          let logFile = testEnvDir </> "op.log"

          pread <- ContT $ withAsync $ flip runContT pure $ callCC \stop -> do
            let outp = getStdout p
            fix \loop -> do
              s <- liftIO (try @_ @IOException (IO.hGetLine outp)) >>= \case
                     Left  e -> err (red "pread:" <+> viaShow e) >> stop ()
                     Right s -> pure s

              liftIO do
                appendFile logFile (s <> "\n")
                void $ try @_ @SomeException (parseTop s & either (err.viaShow) (void . run d))
                putStrLn s
              loop

          ContT $ withAsync $ forever do
            join $ atomically (readTQueue storms)

          ContT $ withAsync $ forever do
            rest <- readTVarIO rest
            b    <- readTVarIO blocks <&> HPSQ.size
            r    <- readTVarIO refs <&> HPSQ.size
            k    <- readTVarIO killed
            s    <- readTVarIO stopped
            c    <- readTVarIO compacted
            m    <- readTVarIO merged
            sw   <- readTVarIO sweeped

            notice $ green "status"
                       <+> "rest:" <+> pretty rest
                       <+> "b:"  <+> pretty b
                       <+> "r:"  <+> pretty r
                       <+> "m:"  <+> pretty m
                       <+> "sw:" <+> pretty sw
                       <+> "c:"  <+> pretty c
                       <+> "k:"  <+> pretty k
                       <+> "s:"  <+> pretty s

            pause @'Seconds 1

          liftIO $ hSetBuffering inp  LineBuffering

          pid <- liftIO (PT.getPid p) `orDie` "oopsie!"
          info $ "spawned" <+> pretty inner <+> viaShow pid

          let getNextState = sampleState g dist

          let defaultIdle = realToFrac wIdleDef :: Timeout 'Seconds

          idleTime  <- newTVarIO defaultIdle
          trelaxTill <- newTVarIO 0

          flip fix EnduranceIdle \loop -> \case
            EnduranceIdle -> do
              readTVarIO idleTime >>= pause

              r <- readTVarIO rest

              if r <= 0 then do
                loop EnduranceStop
              else do
                getNextState >>= loop

            EndurancePutBlk -> do
              bsize <- liftIO $ uniformRM (1, wMaxBlk) g
              liftIO $ IO.hPrint inp ("write-random-block" <+> viaShow bsize)
              atomically $ modifyTVar rest pred
              getNextState >>= loop

            EnduranceDelBlk -> do
              blk <- getRandomBlock
              for_ blk $ \h -> do
                liftIO $ IO.hPrint inp ("del-block" <+> pretty h)

              getNextState >>= loop

            EnduranceHasBlk -> do
              blk <- getRandomBlock
              for_ blk $ \h -> do
                liftIO $ IO.hPrint inp ("has-block" <+> pretty h)

              getNextState >>= loop

            EnduranceHasSeedBlk -> do
              blk <- getRandomSeedBlock
              for_ blk $ \h -> do
                liftIO $ IO.hPrint inp ("has-seed-block" <+> pretty h)

              getNextState >>= loop

            EnduranceGetBlk -> do
              blk <- getRandomBlock
              for_ blk $ \h -> do
                liftIO $ IO.hPrint inp ("get-block" <+> pretty h)
              getNextState >>= loop

            EndurancePutRef -> do
              href <- liftIO (genRandomBS g 32) <&> HashRef . coerce
              blk  <- getRandomBlock
              for_ blk $ \val -> do
                liftIO $ IO.hPrint inp ("set-ref" <+> pretty href <+> pretty val)
              atomically $ modifyTVar rest pred
              getNextState >>= loop

            EnduranceGetRef -> do
              e <- getRandomRef
              for_ e $ \h ->
                liftIO $ IO.hPrint inp ("get-ref" <+> pretty h)
              getNextState >>= loop

            EnduranceDelRef -> do
              e <- getRandomRef
              for_ e $ \h ->
                liftIO $ IO.hPrint inp ("del-ref" <+> pretty h)
              getNextState >>= loop

            EnduranceMerge -> do
              liftIO $ IO.hPrint inp "merge"
              atomically $ modifyTVar merged succ
              getNextState >>= loop

            EnduranceCompact -> do
              liftIO $ IO.hPrint inp "compact"
              atomically $ modifyTVar compacted succ
              getNextState >>= loop

            EnduranceSweep -> do
              liftIO $ IO.hPrint inp "sweep"
              atomically $ modifyTVar sweeped succ
              getNextState >>= loop

            EnduranceExit -> do
              liftIO $ IO.hPrint inp "exit"
              debug $ yellow "inner process stopped?"
              liftIO $ race (pause @'Seconds 1) (waitExitCode p) >>= \case
                Right{} -> none
                Left{}  -> do
                  debug $ red "force inner process to stop"
                  stopProcess p
              atomically $ modifyTVar stopped succ
              lift recover

            EnduranceKill -> do
              debug $ red "KILL" <+> viaShow pid
              cancel pread
              hFlush inp
              liftIO $ appendFile logFile "; killed"
              pause @'Seconds 0.25
              void $ runProcess (proc "kill" ["-9", show pid])
              notice $ red "Killed" <+> viaShow pid
              atomically $ modifyTVar killed succ
              pause @'Seconds 0.5
              lift recover

            EnduranceStop -> do
              liftIO $ hClose inp
              wait pread
              stopProcess p
              notice $ green "done"
              notice $ "validate" <+> pretty logFile
              liftIO $ validateTestResult logFile

            EnduranceCalm -> do
              n <- liftIO $ uniformRM (0.5,10.00) g
              debug $ "CALM" <+> pretty n
              pause @'Seconds (realToFrac n)
              getNextState >>= loop

            EnduranceStorm -> do

              now <- getTimeCoarse
              relaxTill <- readTVarIO trelaxTill

              itn <- readTVarIO idleTime

              if | itn < defaultIdle -> do
                     loop EnduranceIdle

                 | now < relaxTill -> do
                    debug $ yellow "storm on cooldown"
                    loop EnduranceIdle

                 | otherwise -> do
                    t0 <- liftIO $ uniformRM (wStormMin,wStormMax) g
                    debug $ red "FIRE IN DA HOLE!" <+> pretty t0
                    atomically $ writeTQueue storms do
                      atomically $ writeTVar idleTime 0
                      pause @'Seconds (realToFrac t0)
                      atomically $ writeTVar idleTime defaultIdle
                      t1 <- getTimeCoarse
                      -- add 10 sec cooldown
                      atomically $ writeTVar trelaxTill (t1 + ceiling 10e9)

                    getNextState >>= loop

testEnduranceInner :: forall c m . (MonadUnliftIO m, IsContext c, Exception (BadFormException c))
                   => FilePath
                   -> m ()

testEnduranceInner path = flip runContT pure $ callCC \exit -> do

  g <- liftIO $ MWC.createSystemRandom

  debug $ red "storage path" <+> pretty path

  hSetBuffering stdout LineBuffering

  sto <- ContT $ ncqWithStorage path

  forever $ callCC \again -> do

     s' <- liftIO (try @_ @IOException getLine)
            <&> fromRight "exit"
            <&> parseTop >>= \case
                  Left  e  -> err (viaShow e) >> again ()
                  Right s  -> pure (fmap (fixContext @C @c) s)

     lift (try @_ @SomeException (run @c (dict g sto) s')) >>= \case
        Left e -> err (viaShow e)
        Right (StringLike "done") -> do
          liftIO $ IO.hPutStrLn stderr $ "INNER PROCESS TO EXIT"
          exit ()

        Right _ -> none

  where
    dict g sto@NCQStorage{..} = makeDict @c @m do

      entry $ bindMatch "exit" $ const do
        pure $ mkSym "done"

      entry $ bindMatch "write-random-block" $ nil_ \case
        [ LitIntVal n ] -> do
          s <- liftIO $ genRandomBS g (fromIntegral n)
          h <- putBlock (AnyStorage sto) (LBS.fromStrict s) >>= orThrowUser "block-not-written"
          liftIO $ print $ "block-written" <+> pretty h <+> pretty (BS.length s)

        e -> throwIO (BadFormException @c (mkList e))

      entry $ bindMatch "has-block" $ nil_ \case
        [ HashLike h ] -> do
          s <- hasBlock (AnyStorage sto) (coerce h)
          liftIO $ print $ "has-block-result" <+> pretty h <+> pretty s

        e -> throwIO (BadFormException @c (mkList e))

      entry $ bindMatch "has-seed-block" $ nil_ \case
        [ HashLike h ] -> do
          s <- hasBlock (AnyStorage sto) (coerce h)
          liftIO $ print $ "has-seed-block-result" <+> pretty h <+> pretty s

        e -> throwIO (BadFormException @c (mkList e))

      entry $ bindMatch "get-block" $ nil_ \case
        [ HashLike h ] -> do
          s <- getBlock (AnyStorage sto) (coerce h)
          let hx = fmap (hashObject @HbSync) s
          liftIO $ print $ "get-block-result" <+> pretty h <+> pretty hx

        e -> throwIO (BadFormException @c (mkList e))

      entry $ bindMatch "del-block" $ nil_ \case
        [ HashLike h ] -> do
          delBlock (AnyStorage sto) (coerce h)
          liftIO $ print $ "block-deleted" <+> pretty h

        e -> throwIO (BadFormException @c (mkList e))

      entry $ bindMatch "set-ref" $ nil_ \case
        [ HashLike h, HashLike hdest ] -> lift do
          updateRef (AnyStorage sto) (RefAlias2 mempty h) (coerce hdest)
          liftIO $ print $ "ref-updated" <+> pretty h <+> pretty hdest

        e -> throwIO (BadFormException @c (mkList e))

      entry $ bindMatch "get-ref" $ nil_ \case
        [ HashLike h ] -> lift do
          what <- getRef (AnyStorage sto) (RefAlias2 mempty h)
          liftIO $ print $ "get-ref-result" <+> pretty h <+> pretty what

        e -> throwIO (BadFormException @c (mkList e))

      entry $ bindMatch "del-ref" $ nil_ \case
        [ HashLike h ] -> lift do
          delRef (AnyStorage sto) (RefAlias2 mempty h)
          liftIO $ print $ "ref-deleted" <+> pretty h

        e -> throwIO (BadFormException @c (mkList e))

      entry $ bindMatch "merge" $ nil_ $ const do
         ncqSetFlag ncqMergeReq
         liftIO $ print $ "merge"

      entry $ bindMatch "compact" $ nil_ $ const do
         ncqSetFlag ncqCompactReq
         liftIO $ print $ "compact"

      entry $ bindMatch "sweep" $ nil_ $ const do
         ncqSetFlag ncqSweepReq
         liftIO $ print $ "sweep"


