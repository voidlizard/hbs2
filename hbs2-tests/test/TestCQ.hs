{-# Language AllowAmbiguousTypes #-}
{-# Language UndecidableInstances #-}
{-# Language MultiWayIf #-}
{-# Language RecordWildCards #-}
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


import Data.Config.Suckless.Syntax
import Data.Config.Suckless.Script
import Data.Config.Suckless.System

import DBPipe.SQLite hiding (field)

import Data.Bits
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Builder
import Data.Maybe
import Data.Word
import Data.List qualified as List
import Data.Vector qualified as V
import Data.Vector ((!))
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Maybe
import Network.ByteOrder qualified as N
import Data.Coerce
import Data.HashPSQ qualified as HPSQ
import Data.HashSet qualified as HS
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.IntMap qualified as IntMap
import Data.IntMap (IntMap)
import Data.Fixed
import System.Environment
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

import UnliftIO

import Text.InterpolatedString.Perl6 (qc)

import Streaming.Prelude qualified as S
import System.TimeIt

import System.IO.Unsafe (unsafePerformIO)

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



main :: IO ()
main = do

  let dict = makeDict @C do

        entry $ bindMatch "--help" $ nil_ \case
          HelpEntryBound what -> helpEntry what
          [StringLike s]      -> helpList False (Just s)
          _                   -> helpList False Nothing

        internalEntries

        entry $ bindMatch "test:sqlite" $ nil_ $ \case
          [StringLike fn] -> liftIO do
            hashes <- readFile fn <&> mapMaybe (fromStringMay @HashRef) . lines

            let dbname = "jopakita.db"
            rm  dbname
            newDb <- newDBPipeEnv dbPipeOptsDef dbname

            withDB newDb do
              ddl [qc|CREATE TABLE kv (k BLOB PRIMARY KEY, v int)|]

            timeItNamed "sqlite -- test insert" do
              withDB newDb $ transactional do
                for_ hashes $ \h -> do
                  let k = coerce @_ @ByteString h
                  insert [qc|insert into kv (k,v) values(?,?)|] (k,0)

            replicateM_ 5 do
              withDB newDb do
                timeItNamed "sqlite -- select test" do
                  -- fn <- newTVarIO 0
                  -- fns <- newTVarIO 0
                  q <- newTQueueIO
                  for_ hashes $ \h -> do
                    let k = coerce @_ @ByteString h

                    founds <- select [qc|select k,v from kv where k = ?|] (Only k)

                    for_ founds $ \(s :: ByteString,n :: Int) -> do
                      atomically $ writeTQueue q (s,n)

                  found <- atomically (STM.flushTQueue q) <&> List.length
                  liftIO $ IO.hPrint stderr $ "FOUND" <+> pretty found

          e -> throwIO $ BadFormException @C (mkList e)

        entry $ bindMatch "test:hashmap" $ nil_ $ \case
          [StringLike fn] -> liftIO do
            hashes <- readFile fn <&> mapMaybe (fromStringMay @HashRef) . lines
            let hma = HM.fromList [(h,()) | h <- hashes ]

            replicateM_ 5 do
              timeItNamed (show $ "HashMap lookup test" <+> pretty (List.length hashes)) do
                q <- newTQueueIO
                for_ hashes $ \h -> do
                  when (HM.member h hma) do
                    atomically $ writeTQueue q h

                n <- atomically ( STM.flushTQueue q) <&> List.length
                liftIO $ print $ "FOUND" <+> pretty n

          e -> throwIO $ BadFormException @C (mkList e)


        entry $ bindMatch "test:nway:scan" $ nil_ $ \case
          [ StringLike fn ]-> liftIO do
            (mmaped,meta@NWayHash{..}) <- nwayHashMMapReadOnly fn >>= orThrow (NWayHashInvalidMetaData fn)
            let emptyKey = BS.replicate nwayKeySize 0
            nwayHashScanAll meta mmaped $ \o k v -> do
              unless (k == emptyKey) do
                liftIO $ print $ "scan:found" <+> fill 44 (pretty (coerce @_ @HashRef k)) <+> pretty o

          e -> throwIO $ BadFormException @C (mkList e)


        entry $ bindMatch "test:nway:lookup" $ nil_ $ \case

          [ StringLike fn ] -> liftIO do

            hashes <- getContents  <&> mapMaybe (fromStringMay @HashRef) . lines

            (mmaped, nw) <- nwayHashMMapReadOnly fn >>= orThrow (NWayHashInvalidMetaData fn)

            replicateM_ 5 do
              timeItNamed (show $ "lookup:nway" <+> pretty (List.length hashes)) do
                rQ <- newTQueueIO

                for_ hashes $ \h -> do
                  r <- nwayHashLookup nw mmaped (coerce @_ @ByteString h)
                  when (isJust r) do
                    atomically $ writeTQueue rQ (h,r)

                found <- atomically $ STM.flushTQueue rQ
                liftIO $ print $ "FOUND" <+> pretty (List.length found)

          e -> throwIO $ BadFormException @C (mkList e)

        entry $ bindMatch "test:nway:stats" $ \case
          [StringLike fn] -> liftIO do

            mt_    <- newTVarIO 0
            total_ <- newTVarIO 0

            (mmaped,meta@NWayHash{..}) <- nwayHashMMapReadOnly fn >>= orThrow (NWayHashInvalidMetaData fn)

            let emptyKey = BS.replicate nwayKeySize 0
            nwayHashScanAll meta mmaped $ \o k v -> do
              atomically do
                modifyTVar total_ succ
                when (k == emptyKey) do
                  modifyTVar mt_ succ

            mt    <- readTVarIO mt_
            total <- readTVarIO total_
            let used = total - mt

            let ratio = realToFrac @_ @(Fixed E3) (realToFrac used / realToFrac total)

            let stats = mkForm @C "stats" [ mkForm "empty" [mkInt mt]
                                          , mkForm "used"  [mkInt used]
                                          , mkForm "total" [mkInt total]
                                          , mkForm "ratio" [mkDouble ratio]
                                          ]

            pure $ mkList [mkForm "metadata" [mkSyntax meta], stats]

          e -> throwIO $ BadFormException @C (mkList e)

        entry $ bindMatch "test:nway:metadata" $ \case
          [StringLike fn] -> liftIO do
            (_, nw) <- nwayHashMMapReadOnly fn >>= orThrowUser "can't mmape file"
            pure $ mkSyntax nw

          e -> throwIO $ BadFormException @C (mkList e)

        entry $ bindMatch "test:nway:write" $ nil_ $ \case
          [StringLike fn] -> liftIO do
            hashes <- getContents  <&> mapMaybe (fromStringMay @HashRef) . lines
            let items = [ (coerce @_ @ByteString x, N.bytestring64 0) | x <- hashes ]
            nwayWriteBatch (nwayAllocDef 1.10 32 8 8) "." fn items

          e -> throwIO $ BadFormException @C (mkList e)

        entry $ bindMatch "test:ncq:index" $ \case
          [ StringLike p, StringLike fsrc ]-> lift $ flip runContT pure  do

            ncq <- lift $ ncqStorageOpen p
            writer <- ContT $ withAsync $ ncqStorageRun ncq
            link writer

            (fres,_) <- lift $ ncqIndexFile ncq fsrc

            pure $ mkSym fres

          e -> throwIO $ BadFormException @C (mkList e)


        entry $ bindMatch "test:ncq:raw:get:stdout" $ nil_ \case

          [StringLike fn, HashLike h] -> lift $ withNCQ id fn $ \ncq -> do
            w <- ncqStorageGet ncq h
            maybe1 w exitFailure LBS.putStr

          e -> throwIO $ BadFormException @C (mkList e)

        entry $ bindMatch "test:ncq:has" $ \case

          [StringLike fn, HashLike h] -> liftIO $ flip runContT pure do

            ncq <- lift $ ncqStorageOpen fn
            writer <- ContT $ withAsync $ ncqStorageRun ncq
            link writer

            lift do
              ncqStorageHasBlock ncq h >>= \case
                Nothing -> pure nil
                Just x  -> pure $ mkInt x

          e -> throwIO $ BadFormException @C (mkList e)

        entry $ bindMatch "test:ncq:raw:up" $ nil_ $ \case

          [StringLike fn] -> liftIO $ flip runContT pure do

            ncq@NCQStorage{..} <- lift $ ncqStorageOpen fn

            writer <- ContT $ withAsync $ ncqStorageRun ncq
            link writer

            trf <- readTVarIO ncqTrackedFiles <&> HPSQ.keys

            for_ trf $ \tf -> do
              notice $ "tracked" <+> pretty tf

          e -> throwIO $ BadFormException @C (mkList e)

        entry $ bindMatch "test:ncq:raw" $ \case
          [StringLike fn] -> liftIO $ flip runContT pure do

            debug "SHIT"

            ncq <- lift $ ncqStorageOpen fn

            writer <- ContT $ withAsync $ ncqStorageRun ncq
            link writer

            h <- lift $ ncqStoragePut ncq "JOPAKITA!"
            h2 <- lift $ ncqStoragePut ncq "PECHENTRESKI!"

            liftIO $ ncqStorageStop ncq
            wait writer

            pure $ mkList [mkSym (show $ pretty h), mkSym (show $ pretty h2)]

          e -> throwIO $ BadFormException @C (mkList e)

        entry $ bindMatch "test:ncq:raw:list" $ nil_ \case
          [StringLike p, StringLike f] -> liftIO $ flip runContT pure do

            ncq <- lift $ ncqStorageOpen p

            writer <- ContT $ withAsync $ ncqStorageRun ncq
            link writer

            lift $ ncqStorageScanDataFile ncq f $ \o _ k v -> do
              liftIO $ print $ pretty k -- <+> pretty o <+> pretty (BS.length v)

          e -> throwIO $ BadFormException @C (mkList e)

        entry $ bindMatch "test:ncq:raw:find-some" $ nil_ \case
          [StringLike fn] -> liftIO $ flip runContT pure do
            hashes <- liftIO $ getContents  <&> mapMaybe (fromStringMay @HashRef) . lines

            ncq <- lift $ ncqStorageOpen fn

            writer <- ContT $ withAsync $ ncqStorageRun ncq
            link writer

            liftIO $ for_ hashes $ \h -> runMaybeT do
              what <- liftIO (ncqStorageHasBlock ncq h) >>= toMPlus
              -- let h1 = hashObject @HbSync what
              -- liftIO $ print $ "block" <+> pretty h <+> pretty h1 <+> pretty (LBS.length what)
              liftIO $ print $ "block" <+> pretty h <+> pretty what -- (LBS.length what)

          e -> throwIO $ BadFormException @C (mkList e)

        entry $ bindMatch "test:ncq:raw:dump-some" $ nil_ \case
          [StringLike fn] -> liftIO $ flip runContT pure do
            hashes <- liftIO $ getContents  <&> mapMaybe (fromStringMay @HashRef) . lines

            xdg <- liftIO $ getXdgDirectory XdgData "hbs2" <&> fromString @StoragePrefix

            s <- simpleStorageInit @HbSync (Just xdg)

            w <- ContT $ withAsync $ simpleStorageWorker s
            link w

            let sto = AnyStorage s

            rm fn
            dump <- openFile fn WriteMode

            for_ hashes $ \h -> runMaybeT do
              blk <- getBlock sto (coerce h) >>= toMPlus
              debug $ "read" <+> pretty (LBS.length blk)
              none
              -- liftIO $ LBS.hPut dump blk

            hClose dump

          e -> throwIO $ BadFormException @C (mkList e)

        entry $ bindMatch "test:ncq:raw:locate:one" $ nil_ \case
          [StringLike fn, HashLike h] -> lift $ withNCQ id fn $ \ncq -> do
            ncqLocate ncq h >>= \case
              Nothing -> print $ pretty "not-found" <+> pretty h
              Just l  -> print $ pretty "found" <+> pretty h <+> pretty l

          e -> throwIO $ BadFormException @C (mkList e)

        entry $ bindMatch "test:ncq:raw:put:stdin" $ \case
          [StringLike fn] -> lift $ withNCQ id fn $ \ncq -> do
            what <- liftIO BS.getContents
            href <- liftIO $ ncqStoragePut ncq (LBS.fromStrict what)
            pure $ maybe nil (mkSym . show . pretty) href

          e -> throwIO $ BadFormException @C (mkList e)

        entry $ bindMatch "test:ncq:raw:get" $ nil_ \case
          [StringLike fn, HashLike href] -> lift $ withNCQ id fn $ \ncq -> do
            mbs <- ncqStorageGet ncq href
            maybe1 mbs exitFailure LBS.putStr

          e -> throwIO $ BadFormException @C (mkList e)

        entry $ bindMatch "test:ncq:raw:merkle:write" $ nil_ \case
          [StringLike fn, StringLike what] -> liftIO $ flip runContT pure do

            ncq <- lift $ ncqStorageOpen fn

            writer <- ContT $ withAsync $ ncqStorageRun ncq
            link writer

            ContT $ bracket none $ const do
              none

            lbs <- liftIO $ LBS.readFile what

            ta <- getTimeCoarse

            (t1,hashes)  <- timeItT $ liftIO do
              chu <- S.toList_ (readChunkedBS lbs (256*1024))
              forConcurrently chu $ \chunk -> do
                ncqStoragePut ncq chunk >>= orThrowUser "can't save"

            tb <- getTimeCoarse

            notice $ "stored in" <+> pretty t1
               <+> pretty (realToFrac @_ @(Fixed E6) (realToFrac (toMicroSeconds (TimeoutTS (tb - ta))) / 1e6))

            -- FIXME: handle-hardcode
            let pt = toPTree (MaxSize 1024) (MaxNum 256) hashes -- FIXME: settings

            m <- makeMerkle 0 pt $ \(_,_,bss) -> liftIO do
                   void $ ncqStoragePut ncq bss >>= orThrowUser "can't save"

            liftIO $ print $ pretty m

            debug "stopping"
            liftIO $ ncqStorageStop ncq
            debug "stopping done"

            wait writer

          e -> throwIO $ BadFormException @C (mkList e)

        entry $ bindMatch "test:ncq:one-ref" $ nil_ $ \case
          [StringLike fn] -> liftIO $ flip runContT pure do

            ncq <- lift $ ncqStorageOpen fn

            writer <- ContT $ withAsync $ ncqStorageRun ncq
            link writer

            ContT $ bracket none $ const do
              none

            none

          e -> throwIO $ BadFormException @C (mkList e)

        entry $ bindMatch "test:ncq:raw:write-some" $ nil_ \case
          [StringLike fn] -> liftIO $ flip runContT pure do

            hashes <- liftIO $ getContents  <&> mapMaybe (fromStringMay @HashRef) . lines

            xdg <- liftIO $ getXdgDirectory XdgData "hbs2" <&> fromString @StoragePrefix

            s <- simpleStorageInit @HbSync (Just xdg)

            w <- ContT $ withAsync $ simpleStorageWorker s
            link w

            let sto = AnyStorage s

            ncq <- lift $ ncqStorageOpen fn

            writer <- ContT $ withAsync $ ncqStorageRun ncq
            link writer

            ContT $ bracket none $ const do
              none

            for_ hashes $ \h -> runMaybeT do
              already <- liftIO (ncqStorageHasBlock ncq h <&> isJust)
              guard (not already)
              -- debug $ "write" <+> pretty h
              blk <- getBlock sto (coerce h) >>= toMPlus
              liftIO do
                let l = LBS.length blk
                -- print $ pretty h <+> pretty l
                ncqStoragePut ncq blk

            warn "about to stop storage!"
            liftIO $ ncqStorageStop ncq

            wait writer

          e -> throwIO $ BadFormException @C (mkList e)

        entry $ bindMatch "test:ncq:raw:del-some" $ nil_ \case
          [StringLike fn] -> liftIO $ flip runContT pure do

            hashes <- liftIO $ getContents  <&> mapMaybe (fromStringMay @HashRef) . lines

            ncq <- lift $ ncqStorageOpen fn

            writer <- ContT $ withAsync $ ncqStorageRun ncq
            link writer

            ContT $ bracket none $ const do
              none

            debug $ "TO DELETE" <+> pretty (length hashes)

            for_ hashes $ \h -> runMaybeT do
              liftIO do
                -- print $ "delete" <+> pretty h
                ncqStorageDel ncq h

            liftIO $ ncqStorageStop ncq

            wait writer

          e -> throwIO $ BadFormException @C (mkList e)

        entry $ bindMatch "test:ncq:run" $ nil_ \case
          [StringLike p] -> lift do
            withNCQ id p $ \_ -> do
              display_ $ "hello from ncq" <+> pretty p

          e -> throwIO $ BadFormException @C (mkList e)


  setupLogger

  argz <- liftIO getArgs

  forms <- parseTop (unlines $ unwords <$> splitForms argz)
           & either  (error.show) pure

  tvd <- newTVarIO dict

  (runEval tvd forms >>= eatNil display)
    `finally` flushLoggers

