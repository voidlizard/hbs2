{-# Language AllowAmbiguousTypes #-}
module HBS2Git.Export where

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
import UnliftIO.Async
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
import System.IO hiding (hClose,hPrint)
import System.IO.Temp
import Control.Monad.Trans.Resource

class ExportRepoOps a where

instance ExportRepoOps ()

exportRefDeleted :: forall  o m . ( MonadIO m
                               , MonadCatch m
                               -- , MonadMask m
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

  -- это "ненормальный" лог, т.е удаление ссылки в текущем контексте
  --  мы удаляем ссылку "там", то есть нам нужно "то" значение ссылки
  --  удалить её локально мы можем и так, просто гитом.
  --  NOTE: empty-log-post
  --    мы тут  постим пустой лог (не содержащий коммитов)
  --    нам нужно будет найти его позицию относитеьлно прочих логов.
  --    его контекст = текущее значение ссылки, которое мы удаляем
  --    но вот если мы удаляем уже удаленную ссылку, то для ссылки 00..0
  --    будет ошибка где-то.

  vals <- withDB db $ stateGetActualRefs <&> List.nub . fmap snd

  let (ctxHead, ctxBs) = makeContextEntry vals

  trace $ "DELETING REF CONTEXT" <+> pretty vals

  let repoHead = RepoHead Nothing (HashMap.fromList [(ref,"0000000000000000000000000000000000000000")])
  let repoHeadStr =  (LBS.pack . show . pretty . AsGitRefsFile) repoHead
  let ha = gitHashObject (GitObject Blob repoHeadStr)
  let headEntry = GitLogEntry GitLogEntryHead (Just ha) ( fromIntegral $ LBS.length repoHeadStr )

  let content  =  gitRepoLogMakeEntry ctxHead ctxBs
                   <>  gitRepoLogMakeEntry headEntry repoHeadStr

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

-- | Exports only one ref to the repo.
--   Corresponds to a single ```git push``` operation
exportRefOnly :: forall  o m . ( MonadIO m
                               , MonadCatch m
                               -- , MonadMask m
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
              -> m HashRef

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

  entries <- gitRevList lastKnownRev val

  -- NOTE: just-for-test-new-non-empty-push-to-another-branch-112

  -- FIXME: may-blow-on-huge-repo-export
  types <- gitGetObjectTypeMany entries <&> Map.fromList

  let lookupType t = Map.lookup t types
  let justOrDie msg x = pure x `orDie` msg

  trace $ "ENTRIES:" <+> pretty (length entries)

  trace "MAKING OBJECTS LOG"

  let fname = [qc|{pretty val}.data|]

  runResourceT $ do

    written <- liftIO $ newTVarIO (HashSet.empty :: HashSet GitHash)

    let myTempDir = "hbs-git"
    temp <- liftIO getCanonicalTemporaryDirectory
    (_,dir) <- allocate (createTempDirectory temp myTempDir) removeDirectoryRecursive

    let fpath = dir </> fname
    fh <-  liftIO $ openBinaryFile fpath AppendMode

    expMon <- newProgressMonitor "export objects"  (length entries)

    enq <- liftIO newTQueueIO

    -- FIXME: export-wtf?

    aread <- async $ do
                for_ entries $ \d -> do
                  here <- liftIO $ readTVarIO written <&> HashSet.member d
                  inState <- withDB db (stateIsLogObjectExists d)
                  updateProgress expMon 1
                  unless (here || inState) do
                    tp <- lookupType d & justOrDie [qc|no object type for {pretty d}|]
                    o <- gitReadObject (Just tp) d
                    let entry = GitLogEntry ( gitLogEntryTypeOf tp ) (Just d) ( fromIntegral $ LBS.length o )
                    liftIO $ atomically $ writeTQueue enq (Just (d,tp,entry,o))

                liftIO $ atomically $ writeTQueue enq Nothing

    fix \next -> do
      mbEntry <- liftIO $ atomically $ readTQueue enq
      case mbEntry of
        Nothing -> pure ()
        Just (d,tp,entry,o)  -> do
          gitRepoLogWriteEntry fh entry o
          liftIO $ atomically $ modifyTVar written (HashSet.insert d)
          trace $ "writing" <+> pretty tp <+> pretty d
          -- TODO: here-split-log-to-parts
          next

    mapM_ wait [aread]

    -- FIXME: problem-log-is-not-assotiated-with-commit
    --  Если так получилось, что в журнале подъехала только ссылка,
    --  и больше нет никакой информации -- мы не можем определить
    --  глубину(высоту?) этой ссылки, и, соответственно, вычислить
    --  её depth в стейте.
    --  Решение: в этом (или иных) случаях добавлять информацию о контексте,
    --  например, состояние других известных ссылок в моменте. Список ссылок
    --  берём из state, полагая, что раз ссылка в стейте, значит, она является
    --  важной. Имея эту информацию, мы можем хоть как-то вычислять depth
    --  этого лога. Похоже на векторные часы, кстати.

    -- это "нормальный" лог. даже если хвост его приедет пустым (не будет коммитов)
    -- тут мы запомним, что его контекст = коммит, на который он устанавливает ссылку
    -- и этот коммит должен быть в секциях лога, которые приехали перед ним.
    -- следствие: у предыдущего лога будет такая же глубина, как и у этого.

    vals <- withDB db $ stateGetActualRefs <&> List.nub . fmap snd
    let (e, bs) = makeContextEntry (val:vals)
    trace $ "writing context entry" <+> pretty [val]
    gitRepoLogWriteEntry fh e bs

    let ha = gitHashObject (GitObject Blob repoHeadStr)
    let headEntry = GitLogEntry GitLogEntryHead (Just ha) ( fromIntegral $ LBS.length repoHeadStr )
    gitRepoLogWriteEntry fh headEntry repoHeadStr

    -- TODO: find-prev-push-log-and-make-ref
    gitRepoLogWriteHead fh (GitLogHeadEntry Nothing)

    hClose fh

    trace "STORING PUSH LOG"

    let meta = fromString $ show
                          $     "hbs2-git" <> line
                             <> "type:" <+> "hbs2-git-push-log"
                             <> line

    content <- liftIO $ LBS.readFile fpath
    logMerkle <- lift $ storeObject meta content `orDie` [qc|Can't store push log|]

    trace $ "PUSH LOG HASH: " <+> pretty logMerkle
    trace $ "POSTING REFERENCE UPDATE TRANSACTION" <+> pretty remote <+> pretty logMerkle

    -- FIXME: calculate-seqno-as-topsort-order
    lift $ postRefUpdate remote 0 logMerkle

    pure logMerkle

runExport :: forall m . (MonadIO m, MonadUnliftIO m, MonadCatch m, HasProgress (App m))
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

  cwd <- liftIO getCurrentDirectory
  cfgPath <- configPath cwd
  let krf = fromMaybe "keyring-file" fp & takeFileName

  liftIO $ putStrLn ""
  liftIO $ putDoc $
    "exported" <+> pretty hhh
    <> section
    <> green "Repository config:" <+> pretty (cfgPath </> "config")
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


