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

import HBS2.Storage
import HBS2.Storage.Simple
import HBS2.Storage.Operations.ByteString

import HBS2.System.Logger.Simple.ANSI

import HBS2.Storage.NCQ
import HBS2.Data.Log.Structured.NCQ

import HBS2.CLI.Run.Internal.Merkle

import Data.Config.Suckless.Syntax
import Data.Config.Suckless.Script as SC
import Data.Config.Suckless.System

import DBPipe.SQLite hiding (field)

import Data.Bits
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Text.Encoding qualified as TE
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Builder
import Data.Maybe
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
import Safe
import Lens.Micro.Platform
import Control.Concurrent.STM qualified as STM
import System.IO.Temp qualified as Temp

import UnliftIO

import Text.InterpolatedString.Perl6 (qc)

import Streaming.Prelude qualified as S
import System.TimeIt

import System.IO.Unsafe (unsafePerformIO)

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


testNCQFuckupRecovery1 :: MonadUnliftIO m
                       => FilePath
                       -> m ()

testNCQFuckupRecovery1 prefix = flip runContT pure do

  mkdir prefix

  tmp <- liftIO (Temp.createTempDirectory prefix "ncq-test")
  let ncqDir   = tmp </> "ncq-test-data"

  ContT $ bracket none $ const do
    none

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


testNCQ1 :: MonadUnliftIO m
         => Bool
         -> FilePath
         -> Int
         -> m ()

testNCQ1 keep prefix n = flip runContT pure do

    mkdir prefix

    tmp <- liftIO (Temp.createTempDirectory prefix "ncq-test")

    let inputDir = tmp </> "input"
    let ncqDir   = tmp </> "ncq-test-data"

    for_ [inputDir] mkdir

    ContT $ bracket none $ const do
      unless keep $ rm tmp

    twritten <- newTVarIO (mempty :: HashSet HashRef)

    nSize <- newTVarIO 0

    fss <- for [1..n] $ \i -> liftIO do
      let fname = inputDir </> show i <> ".bin"
      size <- randomRIO (1, 256*1024)
      atomically $ modifyTVar nSize (+size)
      file <- LBS.take size <$> LBS.readFile "/dev/urandom"
      BS.writeFile fname (BS.toStrict file)
      pure fname

    ncq <- liftIO $ ncqStorageOpen ncqDir
    r   <- liftIO $ async (ncqStorageRun ncq)

    let sto = AnyStorage ncq

    nWq     <- newTVarIO 0
    nCu     <- newTVarIO 0
    nFo     <- newTVarIO 0
    nMissed <- newTVarIO 0

    let
      updateStats :: forall m . MonadIO m => NCQStorage -> HashRef -> m (Maybe Location)
      updateStats ncq h = do
          w <- ncqLocate ncq (coerce h)

          case w of
            Just (InWriteQueue _) -> atomically $ modifyTVar nWq succ
            Just (InCurrent _)    -> atomically $ modifyTVar nCu succ
            Just (InFossil _ _)   -> atomically $ modifyTVar nFo succ
            Nothing               -> atomically $ modifyTVar nMissed succ

          pure w


    t1 <- ContT $ withAsync $ fix \loop -> do

            what <- readTVarIO twritten
            p <- randomRIO (0.01, 0.5)
            pause @'Seconds (realToFrac p)

            forConcurrently_ what $ \h -> do

              w <- updateStats ncq h

              what <- ncqStorageHasBlockEither ncq (coerce h)
              case what of
                Left LocationNotFound | isJust w -> do
                  error $ show $ "FUCKING RACE!" <+> pretty w

                Left e -> throwIO e
                Right _ -> none

            done <- readTVarIO (ncqStopped ncq)
            unless done loop

    link t1
    --

    out <- newTQueueIO

    liftIO do
      forConcurrently_ fss $ \f -> do
        -- debug $ "process file" <+> pretty f
        blk <- BS.readFile f
        h <- putBlock sto (LBS.fromStrict blk)  `orDie` ("Can't store block " <> f)
        atomically do
          writeTQueue out (HashRef h)
          modifyTVar twritten (HS.insert (coerce h))

    blkQ <- atomically do
      STM.flushTQueue out

    notice $ "WAIT BLOCKS DONE" <+> pretty (List.length blkQ)

    lift $ ncqStorageFlush ncq

    for_ blkQ $ \h -> liftIO do
      void $ updateStats ncq h
      hasBlock sto (coerce h)
             `orDie` show ("missed" <+> pretty h)

    liftIO $ ncqStorageStop ncq

    wait t1

    let vars = zip [ "write-q"
                   , "current"
                   , "fossil"
                   , "missed"
                   , "size"
                   ]
                   [nWq, nCu, nFo, nMissed, nSize]

    liftIO $ wait r

    lift $ withNCQ id  ncqDir $ \ncq1 -> do
      for_ blkQ $ \h -> liftIO do
        void $ updateStats ncq1 h
        hasBlock (AnyStorage ncq1) (coerce h) >>= \case
          Nothing -> print $ "missed" <+> pretty h
          Just x  -> none

      results <- for vars $ \(k,w) -> do
          v <- readTVarIO w
          pure $ mkList @C [ mkSym k, mkInt v]

      liftIO $ print $ pretty $ mkList (mkSym "results" : results)



main :: IO ()
main = do

  tvd <- newTVarIO mempty

  let dict = makeDict @C do

        entry $ bindMatch "--help" $ nil_ \case
          HelpEntryBound what -> helpEntry what
          [StringLike s]      -> helpList False (Just s)
          _                   -> helpList False Nothing

        internalEntries

        entry $ bindMatch "#!" $ nil_ $ const none

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

        entry $ bindMatch "ncq:test:ncq:fuckup-recovery1" $ nil_ $ \_ -> lift do
          debug $ "ncq:test:ncq:fuckup-recovery1"
          testNCQFuckupRecovery1 "./tmp-ncq"

        entry $ bindMatch "test:ncq:test1" $ nil_ $ \syn -> lift do
          let (opts, argz) = splitOpts [("-n",1)] syn
          let n = headDef 100 [ x | ListVal [ StringLike "-n", LitIntVal x ] <- opts  ]
          debug $ "ncq:test1" <+> pretty n
          testNCQ1 False "./tmp-ncq" (fromIntegral n)

  setupLogger

  argz <- liftIO getArgs

  forms <- parseTop (unlines $ unwords <$> splitForms argz)
           & either  (error.show) pure

  atomically $ writeTVar tvd dict

  (runEval tvd forms >>= eatNil display)
    `finally` flushLoggers



