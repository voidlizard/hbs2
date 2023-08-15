{-# Language AllowAmbiguousTypes #-}
{-# Language RankNTypes #-}
{-# Language TemplateHaskell #-}
module HBS2Git.Export
  ( exportRefDeleted
  , exportRefOnly
  , runExport
  , ExportRepoOps
  ) where

import HBS2.Prelude.Plated
import HBS2.Data.Types.Refs
import HBS2.OrDie
import HBS2.System.Logger.Simple
import HBS2.Net.Proto.Definition()
import HBS2.Base58

import HBS2.Git.Local
import HBS2.Git.Local.CLI

import HBS2Git.App
import HBS2Git.State
import HBS2Git.Config
import HBS2Git.GitRepoLog

import Control.Applicative
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Concurrent.STM
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Foldable (for_)
import Data.Functor
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.HashSet (HashSet)
import Data.Maybe
import Data.Set qualified as Set
import Data.Map qualified as Map
import Data.List qualified as List
import Lens.Micro.Platform
import Prettyprinter.Render.Terminal
import System.Directory
import System.FilePath
import Text.InterpolatedString.Perl6 (qc)
import UnliftIO.IO
import System.IO hiding (hClose,hPrint,hPutStrLn,hFlush)
import System.IO.Temp
import Control.Monad.Trans.Resource
import Data.List.Split (chunksOf)
import Codec.Compression.GZip

class ExportRepoOps a where

instance ExportRepoOps ()

data ExportEnv =
  ExportEnv
  { _exportDB          :: DBEnv
  , _exportWritten     :: TVar (HashSet GitHash)
  , _exportFileName    :: FilePath
  , _exportDir         :: FilePath
  , _exportRepo        :: RepoRef
  , _exportReadObject  :: GitHash -> IO (Maybe GitObject)
  }

makeLenses 'ExportEnv


exportRefDeleted :: forall  o m . ( MonadIO m
                                  , MonadCatch m
                                  , MonadMask m
                                  , MonadUnliftIO m
                                  , HasCatAPI m
                                  , HasConf m
                                  , HasRefCredentials m
                                  , HasProgress m
                                  , ExportRepoOps o
                                  )
              => o
              -> RepoRef
              -> GitRef
              -> m HashRef
exportRefDeleted _ repo ref = do
  trace $ "exportRefDeleted" <+> pretty repo <+> pretty ref

  dbPath <- makeDbPath repo
  db <- dbEnv dbPath

  let opts = ()

  -- это "ненормальный" лог, т.е удаление ссылки в текущем контексте
  --  мы удаляем ссылку "там", то есть нам нужно "то" значение ссылки
  --  удалить её локально мы можем и так, просто гитом.
  --  NOTE: empty-log-post
  --    мы тут  постим пустой лог (не содержащий коммитов)
  --    нам нужно будет найти его позицию относитеьлно прочих логов.
  --    его контекст = текущее значение ссылки, которое мы удаляем
  --    но вот если мы удаляем уже удаленную ссылку, то для ссылки 00..0
  --    будет ошибка где-то.

  vals <- withDB db $ stateGetLastKnownCommits 10
  let (ctxHead, ctxBs) = makeContextEntry vals

  trace $ "DELETING REF CONTEXT" <+> pretty vals

  let repoHead = RepoHead Nothing (HashMap.fromList [(ref,"0000000000000000000000000000000000000000")])
  let repoHeadStr =  (LBS.pack . show . pretty . AsGitRefsFile) repoHead
  let ha = gitHashObject (GitObject Blob repoHeadStr)
  let headEntry = GitLogEntry GitLogEntryHead (Just ha) ( fromIntegral $ LBS.length repoHeadStr )

  let content  =  gitRepoLogMakeEntry opts ctxHead ctxBs
                   <>  gitRepoLogMakeEntry opts headEntry repoHeadStr

  -- FIXME: remove-code-dup
  let meta = fromString $ show
                          $    "hbs2-git" <> line
                            <> "type:" <+> "hbs2-git-push-log"
                            <> line

  logMerkle <- storeObject meta content `orDie` [qc|Can't store push log|]
  postRefUpdate repo 0 logMerkle
  pure logMerkle

makeContextEntry :: [GitHash] -> (GitLogEntry, LBS.ByteString)
makeContextEntry hashes = (entryHead, payload)
  where
    ha = Nothing
    payload = GitLogContextCommits (HashSet.fromList hashes) & serialise
    entryHead = GitLogEntry GitLogContext ha undefined


newtype ExportT m a = ExportT { fromExportT :: ReaderT ExportEnv m a }
                      deriving newtype ( Functor
                                       , Applicative
                                       , Monad
                                       , MonadIO
                                       , MonadTrans
                                       , MonadReader ExportEnv
                                       , MonadMask
                                       , MonadCatch
                                       , MonadThrow
                                       )

withExportEnv :: MonadIO m => ExportEnv -> ExportT m a -> m a
withExportEnv env f = runReaderT (fromExportT f) env

writeLogSegments :: forall m . ( MonadIO m
                               , HasCatAPI m
                               , MonadMask m
                               , HasRefCredentials m
                               , HasConf m
                               )
                 => ( Int -> m () )
                 -> GitHash
                 -> [GitHash]
                 -> Int
                 -> [(GitLogEntry, LBS.ByteString)]
                 -> ExportT m [HashRef]

writeLogSegments onProgress _val objs chunkSize trailing = do

  db          <- asks $ view exportDB
  written     <- asks $ view exportWritten
  fname       <- asks $ view exportFileName
  dir         <- asks $ view exportDir
  remote      <- asks $ view exportRepo
  readGit     <- asks $ view exportReadObject

  let opts = CompressWholeLog

  -- TODO: options-for-compression-level
  --   помним, что всё иммутабельное. как один раз запостим,
  --   такое и будет жить всегда
  let compressOpts = defaultCompressParams { compressLevel = bestSpeed }

  -- FIXME: fix-code-dup
  let meta = fromString $ show
                        $     "hbs2-git"
                           <> line
                           <> "type:"    <+> "hbs2-git-push-log"
                           <> line
                           <> "flags:"   <+> "gz:sgmt"
                           <> line

  let segments = chunksOf chunkSize objs
  let totalSegments = length segments

  forM (zip segments [1..]) $ \(segment, segmentIndex) -> do
    let fpath = dir </> fname <> "_" <> show segmentIndex
    bracket (liftIO $ openBinaryFile fpath AppendMode)
            (const $ pure ()) $ \fh -> do
      for_ segment $ \d -> do
        here <- liftIO $ readTVarIO written <&> HashSet.member d
        inState <- withDB db (stateIsLogObjectExists d)

        lift $ onProgress 1

        unless (here || inState) do

          GitObject tp o  <- liftIO $ readGit d `orDie` [qc|error reading object {pretty d}|]

          let entry = GitLogEntry ( gitLogEntryTypeOf tp ) (Just d) ( fromIntegral $ LBS.length o )
          gitRepoLogWriteEntry opts fh entry o
          liftIO $ atomically $ modifyTVar written (HashSet.insert d)

          -- gitRepoLogWriteEntry fh ctx ctxBs

          trace $ "writing" <+> pretty tp <+> pretty d

      when (segmentIndex == totalSegments) $ do
        for_ trailing $ \(e, bs) -> do
          gitRepoLogWriteEntry opts fh e bs

      -- finalize log section
      hClose fh

      content   <- liftIO $ LBS.readFile fpath

      let gzipped = compressWith compressOpts content

      logMerkle <- lift $ storeObject meta gzipped `orDie` [qc|Can't store push log|]

      trace $ "PUSH LOG HASH: " <+> pretty logMerkle
      trace $ "POSTING REFERENCE UPDATE TRANSACTION" <+> pretty remote <+> pretty logMerkle

      lift $ postRefUpdate remote 0 logMerkle

      pure logMerkle

-- | Exports only one ref to the repo.
--   Corresponds to a single ```git push``` operation
exportRefOnly :: forall  o m . ( MonadIO m
                               , MonadCatch m
                               , MonadMask m
                               , MonadUnliftIO m
                               , HasCatAPI m
                               , HasConf m
                               , HasRefCredentials m
                               , HasProgress m
                               , ExportRepoOps o
                               )
              => o
              -> RepoRef
              -> Maybe GitRef
              -> GitRef
              -> GitHash
              -> m (Maybe HashRef)

exportRefOnly _ remote rfrom ref val = do


  let repoHead = RepoHead Nothing (HashMap.fromList [(ref,val)])

  let repoHeadStr =  (LBS.pack . show . pretty . AsGitRefsFile) repoHead

  dbPath <- makeDbPath remote
  db <- dbEnv dbPath

  trace $ "exportRefOnly" <+> pretty remote <+> pretty ref <+> pretty val

  -- 1. get max ref value for known REMOTE branch
  -- 2. if unkwnown - get max branch ref value for known LOCAL branch (known from the state)
  -- 3. if unkwnown - then Nothing
  -- therefore, we export only the delta for the objects for push between known state and current
  -- git repot state
  -- if it's a new branch push without any objects commited -- then empty log
  -- only with HEAD section should be created
  lastKnownRev <- withDB db do
                    rThat <- stateGetActualRefValue  ref
                    rThis <- maybe1 rfrom (pure Nothing) stateGetActualRefValue
                    pure $ rThat  <|> rThis

  trace $ "LAST_KNOWN_REV" <+> braces (pretty rfrom) <+> braces (pretty ref) <+> braces (pretty lastKnownRev)

  entries <- traceTime "gitRevList" $ gitRevList lastKnownRev val

  let _entryNum = length entries

  -- NOTE: just-for-test-new-non-empty-push-to-another-branch-112

  -- FIXME: may-blow-on-huge-repo-export
  types <- traceTime "gitGetObjectTypeMany" $ gitGetObjectTypeMany entries <&> Map.fromList

  let lookupType t = Map.lookup t types

  let onEntryType e = (fx $ lookupType e, e)
         where fx = \case
                 Just Blob   -> 0
                 Just Tree   -> 1
                 Just Commit -> 2
                 Nothing     -> 3

  trace $ "ENTRIES:" <+> pretty (length entries)

  trace "MAKING OBJECTS LOG"

  let fname = [qc|{pretty val}.data|]

  -- TODO: investigate-on-signal-behaviour
  --   похоже, что в случае прилёта сигнала он тут не обрабатывается,
  --   и временный каталог остаётся
  runResourceT $ do

    gitCatFile <- startGitCatFile

    written <- liftIO $ newTVarIO (HashSet.empty :: HashSet GitHash)

    let myTempDir = "hbs-git"
    temp <- liftIO getCanonicalTemporaryDirectory

    (_,dir) <- allocate (createTempDirectory temp myTempDir) removeDirectoryRecursive

    let (blobs, notBlobs) = List.partition (\e -> fst (onEntryType e) == 0) entries
    let (trees, notTrees) = List.partition (\e -> fst (onEntryType e) == 1) notBlobs
    -- FIXME: others-might-be-tags
    let (commits, others) = List.partition (\e -> fst (onEntryType e) == 2) notTrees

    -- FIXME: hbs2-git-size-hardcode-to-args
    let batch = 20000
    let objects = blobs <> trees <> others <> commits
    mon <- newProgressMonitor "write objects"   (length objects)

    let env = ExportEnv
              { _exportDB = db
              , _exportWritten = written
              , _exportFileName = fname
              , _exportDir = dir
              , _exportRepo = remote
              , _exportReadObject = gitReadFromCatFileBatch gitCatFile
              }


    let ha = gitHashObject (GitObject Blob repoHeadStr)
    let headEntry = GitLogEntry GitLogEntryHead (Just ha) ( fromIntegral $ LBS.length repoHeadStr )

    let upd = updateProgress mon

    vals <- withDB db $ stateGetLastKnownCommits 10
    let (ctx, ctxBs) = makeContextEntry (List.nub $ val:vals)

    -- we need context entries to determine log HEAD operation sequence
    -- so only the last section needs it alongwith headEntry
    logz  <- lift $ withExportEnv env (writeLogSegments upd val objects batch [ (ctx, ctxBs)
                                                                              , (headEntry, repoHeadStr)
                                                                              ])

    -- NOTE: отдаём только последнюю секцию лога,
    --       что бы оставить совместимость
    pure $ lastMay logz

runExport :: forall m . ( MonadIO m
                        , MonadUnliftIO m
                        , MonadCatch m
                        , HasProgress (App m)
                        , MonadMask (App m)
                        )

          => Maybe FilePath -> RepoRef -> App m ()
runExport fp repo = do


  liftIO $ putDoc $
       line
    <> green "Exporting to reflog" <+> pretty (AsBase58 repo)
    <> section
    <> "it may take some time on the first run"
    <> section

  git <- asks (view appGitDir)

  trace $ "git directory is" <+> pretty git

  loadCredentials (maybeToList fp)

  -- FIXME: wtf-runExport
  branchesGr <- cfgValue @ConfBranch <&> Set.map normalizeRef

  headBranch <- gitGetBranchHEAD `orDie` "undefined HEAD for repo"

  refs <- gitListLocalBranches
             <&> filter (\x -> Set.null branchesGr ||  Set.member (fst x) branchesGr)

  trace $ "REFS" <+> pretty refs

  fullHead <- gitHeadFullName headBranch

  -- debug $ "HEAD" <+> pretty fullHead

  -- let repoHead = RepoHead (Just fullHead)
  --                         (HashMap.fromList refs)

  -- trace $ "NEW REPO HEAD" <+> pretty (AsGitRefsFile repoHead)

  val <- gitGetHash fullHead `orDie` [qc|Can't resolve ref {pretty fullHead}|]

   -- _ <- exportRefOnly () remote br gh
  hhh <- exportRefOnly () repo Nothing fullHead val

  -- NOTE: ???
  -- traceTime "importRefLogNew (export)" $ importRefLogNew False repo

  shutUp

  configPath  <- asks $ view appConfPath
  let krf = fromMaybe "keyring-file" fp & takeFileName

  liftIO $ putStrLn ""
  liftIO $ putDoc $
    "exported" <+> pretty hhh
    <> section
    <> green "Repository config:" <+> pretty configPath
    <> section
    <>  "Put the keyring file" <+> yellow (pretty krf) <+> "into a safe place," <> line
    <> "like encrypted directory or volume."
    <> section
    <> "You will need this keyring to push into the repository."
    <> section
    <> green "Add keyring into the repo's config:"
    <> section
    <> "keyring" <+> pretty [qc|"/my/safe/place/{krf}"|]
    <> section
    <> green "Add git remote:"
    <> section
    <> pretty [qc|git remote add remotename hbs2://{pretty repo}|]
    <> section
    <> green "Work with git as usual:"
    <> section
    <> "git pull remotename" <> line
    <> "(or git fetch remotename && git reset --hard remotename/branch)" <> line
    <> "git push remotename" <> line
    <> line


