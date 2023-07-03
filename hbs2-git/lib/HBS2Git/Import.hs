{-# Language TemplateHaskell #-}
module HBS2Git.Import where

import HBS2.Prelude.Plated
import HBS2.Data.Types.Refs
import HBS2.OrDie
import HBS2.System.Logger.Simple
import HBS2.Merkle
import HBS2.Hash
import HBS2.Net.Proto.RefLog
import Text.InterpolatedString.Perl6 (qc)
import HBS2.Data.Detect hiding (Blob)

import HBS2.Git.Local
import HBS2Git.GitRepoLog
import HBS2Git.App
import HBS2Git.State
import HBS2.Git.Local.CLI

import Data.Fixed
import Control.Monad.Trans.Maybe
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue qualified as Q
import Control.Monad.Reader
import Data.Maybe
import Data.ByteString.Lazy.Char8 qualified as LBS
import Lens.Micro.Platform
import Data.Set qualified as Set
import Codec.Serialise
import Control.Monad.Catch
import Control.Monad.Trans.Resource
import System.Directory
import System.IO.Temp
import UnliftIO.IO
import System.IO (openBinaryFile)
import System.FilePath.Posix
import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as Text
import Data.Config.Suckless
import Data.Either

import Streaming.ByteString qualified as SB
import Streaming.Zip qualified as SZip

data RunImportOpts =
  RunImportOpts
  { _runImportDry    :: Maybe Bool
  , _runImportRefVal :: Maybe HashRef
  }

makeLenses 'RunImportOpts

isRunImportDry :: RunImportOpts -> Bool
isRunImportDry o = view runImportDry o == Just True

walkHashes :: HasCatAPI m => TQueue HashRef -> Hash HbSync -> m ()
walkHashes q h = walkMerkle h (readBlock . HashRef) $ \(hr :: Either (Hash HbSync) [HashRef]) -> do
  case hr of
    Left hx -> die $ show $ pretty "missed block:" <+> pretty hx
    Right (hrr :: [HashRef]) -> do
       forM_ hrr $ liftIO . atomically . Q.writeTQueue q

blockSource :: (MonadIO m, HasCatAPI m) => HashRef -> SB.ByteStream m Integer
blockSource h = do
  tsize <- liftIO $ newTVarIO 0
  deepScan ScanDeep (const none) (fromHashRef h) (lift . readBlock . HashRef) $ \ha -> do
    sec <- lift $ readBlock (HashRef ha) `orDie` [qc|missed block {pretty ha}|]
    -- skip merkle tree head block, write only the data
    liftIO $ atomically $ modifyTVar tsize (+ LBS.length sec)
    when (h /= HashRef ha) do
      SB.fromLazy sec

  liftIO $ readTVarIO tsize <&> fromIntegral

getLogFlags :: MonadIO m
            => (HashRef -> m (Maybe LBS.ByteString))
            -> HashRef
            -> m (Maybe [Text])

getLogFlags doRead h = do

  runMaybeT do

    treeBs   <- MaybeT $ doRead h

    let something =  tryDetect (fromHashRef h) treeBs
    let meta = mconcat $ rights [ parseTop (Text.unpack s) | ShortMetadata s <- universeBi something ]

    -- TODO: check-if-it-is-hbs2-git-log
    let tp = lastMay [ "hbs2-git-push-log"
                     | (ListVal (Key "type:" [SymbolVal "hbs2-git-push-log"]) ) <- meta
                     ]

    guard ( tp == Just "hbs2-git-push-log" )

    pure $ mconcat [ Text.splitOn ":" (Text.pack (show $ pretty s))
                   | (ListVal (Key "flags:" [SymbolVal s]) ) <- meta
                   ]

class HasImportOpts a where
  importForce :: a -> Bool
  importDontWriteGit :: a -> Bool

instance HasImportOpts Bool where
  importForce f = f
  importDontWriteGit = const False

instance HasImportOpts (Bool, Bool) where
  importForce = fst
  importDontWriteGit = snd

-- FIXME: ASAP-will-work-only-for-one-repo
--  сейчас все транзакции помечаются, как обработанные
--  в глобальном стейте для ссылки. таким образом,
--  если мы вызвали для одного репозитория,
--  то import не будет работать для остальных, т.к. решит,
--  что всё обработано.
--
--  Решение:
--    Вариант N1. Держать стейт локально в каждом
--    каталоге git.
--    Минусы:
--      - большой оверхед по данным
--      - мусор в каталоге git
--      - например, git-hbs2-http вообще работает без "репозитория",
--        как ему быть
--
--    Вариант N2. сделать развязку через какой-то ID
--    репозитория или путь к нему.
--    Минусы:
--      - выглядит хрупко
--      - например, git-hbs2-http вообще работает без "репозитория",
--        как ему быть
--
--    Вариант N3. БД обновлять отдельно, объекты git - отдельно
--    для каждого репозитория, запоминать (где?) проигранные для
--    него логи.
--    Минусы:
--      - двойное сканирование файлов логов - получение, распаковка,
--        сканирование и т.п. сначала для БД, потом для непосредственно
--        репозитория
--
importRefLogNew :: ( MonadIO m
                   , MonadUnliftIO m
                   , MonadCatch m
                   , MonadMask m
                   , HasCatAPI m
                   , HasImportOpts opts
                   )
                => opts -> RepoRef -> m ()

importRefLogNew opts ref = runResourceT do

  let force = importForce opts

  let myTempDir = "hbs-git"
  temp <- liftIO getCanonicalTemporaryDirectory
  (_,dir) <- allocate (createTempDirectory temp myTempDir) removeDirectoryRecursive

  db <- makeDbPath ref >>= dbEnv

  do
    trace $ "importRefLogNew" <+> pretty ref
    logRoot <- lift $ readRef ref `orDie` [qc|can't read ref {pretty ref}|]
    trace $ "ROOT" <+> pretty logRoot

    trans <- withDB db $ stateGetAllTranImported <&> Set.fromList
    done <- withDB db $ stateGetRefImported logRoot

    when (not done || force) do

      logQ <- liftIO newTQueueIO

      lift $ walkHashes logQ (fromHashRef logRoot)

      let notSkip n = force || not (Set.member n trans)
      entries <- liftIO $ atomically $ flushTQueue logQ <&> filter notSkip

      pCommit <- liftIO $ startGitHashObject Commit
      pTree <- liftIO $ startGitHashObject Tree
      pBlob <- liftIO $ startGitHashObject Blob

      let hCommits = getStdin pCommit
      let hTrees = getStdin pTree
      let hBlobs = getStdin pBlob

      let handles = [hCommits, hTrees, hBlobs]

      sp0 <- withDB db savepointNew
      withDB db $ savepointBegin sp0

      forM_ entries $ \e -> do

        missed <- lift $ readBlock e <&> isNothing

        when missed do
          debug $ "MISSED BLOCK" <+> pretty e

        let fname = show (pretty e)
        let fpath = dir </> fname

        (keyFh, fh) <- allocate (openBinaryFile fpath AppendMode) hClose

        runMaybeT $ do
          bs <- MaybeT $ lift $ readBlock e
          refupd <- MaybeT $ pure $ deserialiseOrFail @(RefLogUpdate HBS2L4Proto) bs & either (const Nothing) Just
          payload <- MaybeT $ pure $ deserialiseOrFail (LBS.fromStrict $ view refLogUpdData refupd) & either (const Nothing) Just
          let (SequentialRef _ (AnnotatedHashRef _ h)) = payload
          trace $ "PUSH LOG HASH" <+> pretty h

          treeBs   <- MaybeT $ lift $ readBlock h

          let something =  tryDetect (fromHashRef h) treeBs
          let meta = mconcat $ rights [ parseTop (Text.unpack s) | ShortMetadata s <- universeBi something ]

          -- TODO: check-if-it-is-hbs2-git-log

          let flags = mconcat [ Text.splitOn ":" (Text.pack (show $ pretty s))
                              | (ListVal (Key "flags:" [SymbolVal s]) ) <- meta
                              ]

          let gzipped = "gz" `elem` flags

          debug $ "FOUND LOG METADATA " <+> pretty flags
                                        <+> pretty "gzipped:" <+> pretty gzipped

          here <- withDB db $ stateGetLogImported h

          unless (here && not force) do

            sz <- if gzipped then do
              SB.toHandle fh $ SZip.gunzip (blockSource h)
            else
              SB.toHandle fh (blockSource h)

            release keyFh

            let fpathReal = fpath

            tnum <- liftIO $ newTVarIO 0
            liftIO $ gitRepoLogScan True fpathReal $ \_ _ -> do
              liftIO $ atomically $ modifyTVar tnum succ

            num <- liftIO $ readTVarIO tnum
            trace $ "LOG ENTRY COUNT" <+> pretty  num

            let pref = take 16 (show (pretty e))
            let name = [qc|import {pref}... {realToFrac sz / (1024*1024) :: Fixed E3}|]

            oMon  <- newProgressMonitor name  num

            lift $ gitRepoLogScan True fpathReal $ \entry s -> do

              updateProgress oMon 1

              lbs <- pure s `orDie` [qc|git object not read from log|]

              withDB db do

                case view gitLogEntryType entry of
                  GitLogEntryCommit -> do
                    bss <- lift (pure s) `orDie` [qc|git object not read from log|]
                    let co = view gitLogEntryHash entry
                    hx <- pure (view gitLogEntryHash entry) `orDie` [qc|empty git hash|]

                    trace $ "logobject" <+> pretty h <+> "commit" <+> pretty (view gitLogEntryHash entry)

                    writeIfNew hCommits dir hx (GitObject Commit lbs)
                    statePutLogObject (h, Commit, hx)

                    let parents = gitCommitGetParentsPure bss

                    forM_ parents $ \p -> do
                      trace $ "fact" <+> "commit-parent" <+> pretty co <+> pretty p
                      statePutLogCommitParent (hx,p)

                  GitLogEntryBlob   -> do
                    trace $ "logobject" <+> pretty h <+> "blob" <+> pretty (view gitLogEntryHash entry)
                    hx <- pure (view gitLogEntryHash entry) `orDie` [qc|empty git hash|]
                    writeIfNew hBlobs dir hx (GitObject Blob lbs)
                    statePutLogObject (h, Blob, hx)

                  GitLogEntryTree   -> do
                    trace $ "logobject" <+> pretty h <+> "tree" <+> pretty (view gitLogEntryHash entry)
                    hx <- pure (view gitLogEntryHash entry) `orDie` [qc|empty git hash|]
                    writeIfNew hTrees dir hx (GitObject Tree lbs)
                    statePutLogObject (h, Tree, hx)

                  GitLogContext -> do
                    trace $ "logobject" <+> pretty h <+> "context" <+> pretty (view gitLogEntryHash entry)

                    let co = fromMaybe mempty $ deserialiseOrFail @GitLogContextEntry
                                <$> s >>= either (const Nothing) Just <&> commitsOfGitLogContextEntry

                    forM_ co  (statePutLogContextCommit h)

                  GitLogEntryHead   -> do
                    trace $ "HEAD ENTRY" <+> viaShow s
                    let mbrh = fromStringMay @RepoHead (maybe mempty LBS.unpack s)
                    rh <- pure mbrh `orDie` [qc|invalid log header in {pretty h} {s}|]

                    forM_ (HashMap.toList $ view repoHeads rh) $ \(re,ha) -> do
                      trace $ "logrefval" <+> pretty h <+> pretty re <+> pretty ha
                      statePutLogRefVal (h,re,ha)

                  _ -> pure ()

                -- otherwise we wan't process those logs next time.
                unless (importDontWriteGit opts) do
                  statePutLogImported h
                  statePutTranImported e

      mapM_ hClose handles

      withDB db $ do
        stateUpdateCommitDepths
        statePutRefImported logRoot
        savepointRelease sp0

  where

    writeIfNew gitHandle dir h (GitObject tp s) = do
      unless (importDontWriteGit opts) do
        let nf = dir </> show (pretty h)
        liftIO $ LBS.writeFile nf s
        hPutStrLn gitHandle nf
        hFlush gitHandle
        trace $ "WRITTEN OBJECT" <+> pretty tp <+> pretty h <+> pretty nf

