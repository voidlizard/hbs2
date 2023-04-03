{-# Language AllowAmbiguousTypes #-}
module HBS2Git.Export where

import HBS2.Prelude.Plated
import HBS2.Clock
import HBS2.Data.Types.Refs
import HBS2.OrDie
import HBS2.System.Logger.Simple
import HBS2.Merkle
import HBS2.Net.Proto.Definition()
import HBS2.Base58

import HBS2.Git.Local
import HBS2.Git.Local.CLI

import HBS2Git.App
import HBS2Git.State
import HBS2Git.Update
import HBS2Git.Config

import Data.Functor
import Data.List (sortBy)
import Control.Applicative
import Control.Monad.Reader
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Cache as Cache
import Data.Foldable (for_)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.Maybe
import Data.Set qualified as Set
import Data.Set (Set)
import Lens.Micro.Platform
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Monad.Catch
import Text.InterpolatedString.Perl6 (qc)
import System.Directory
import System.FilePath
import Prettyprinter.Render.Terminal

data HashCache =
  HashCache
  { hCache :: Cache GitHash (Set GitHash)
  , hDb    :: DBEnv
  }

instance Hashable GitHash => HasCache HashCache GitHash (Set GitHash) IO where
  cacheInsert (HashCache cache _) = Cache.insert cache

  cacheLookup (HashCache cache db) k = do
    refs <- withDB db (stateGetDeps k)
    case refs of
      [] -> Cache.lookup' cache k
      xs -> pure $ Just $ Set.fromList xs

newHashCache :: MonadIO m => DBEnv -> m HashCache
newHashCache db = do
  ca <- liftIO $ Cache.newCache Nothing
  pure $ HashCache ca db


export :: forall  m . ( MonadIO m
                      , MonadCatch m
                      , HasCatAPI m
                      , HasConf m
                      , HasRefCredentials m
                      , HasProgress m
                      ) => RepoRef -> RepoHead -> m (HashRef, HashRef)
export h repoHead = do

  let refs = HashMap.toList (view repoHeads repoHead)

  let repoHeadStr =  (LBS.pack . show . pretty . AsGitRefsFile) repoHead

  dbPath <- makeDbPath h

  trace $ "dbPath" <+> pretty dbPath

  db <- dbEnv dbPath

  sp <- withDB db savepointNew

  withDB db $ savepointBegin sp

  rr <- try $ do

    skip <- withDB db stateGetExported <&> HashSet.fromList

    -- TODO: process-only-commits-to-make-first-run-faster
    ooo <- gitListAllObjects <&> filter (not . (`HashSet.member` skip))

    cached0 <- withDB db stateGetAllDeps
    let cached  = HashMap.fromListWith (<>) [ (k, [v]) | (k,v) <- cached0 ]
    let lookup h = pure $ HashMap.lookup h cached & fromMaybe mempty

    monDep <- newProgressMonitor "calculate dependencies" (length ooo)

    allDeps <- gitGetAllDependencies 4 ooo lookup (const $ updateProgress monDep 1)

    let sz  = length allDeps
    mon1 <- newProgressMonitor "storing dependencies" sz

    withDB db $ transactional do
      for_ allDeps $ \(obj,dep) -> do
        updateProgress mon1 1
        stateAddDep dep obj

    deps <- withDB db $ do
              x <- forM refs $ stateGetDepsRec . snd
              pure $ mconcat x

    withDB db $ transactional do -- to speedup inserts

      let metaApp = "application:" <+> "hbs2-git" <> line

      let metaHead = fromString $ show
                                $ metaApp <> "type:" <+> "head" <> line

      -- let gha = gitHashObject (GitObject Blob repoHead)
      hh  <- lift $ storeObject metaHead repoHeadStr `orDie` "cant save repo head"

      mon3 <- newProgressMonitor "export objects from repo" (length deps)

      for_ deps $ \d -> do
        here <- stateGetHash d <&> isJust
        -- FIXME: asap-check-if-objects-is-in-hbs2
        unless here do
          lbs <- gitReadObject Nothing d

          -- TODO: why-not-default-blob
          --   anything is blob
          tp <- gitGetObjectType d <&> fromMaybe Blob --

          let metaO = fromString $ show
                                 $ metaApp
                                 <> "type:" <+> pretty tp <+> pretty d
                                 <> line

          hr' <- lift $ storeObject metaO lbs

          maybe1 hr' (pure ()) $ \hr -> do
            statePutHash tp d hr

          updateProgress mon3 1

      hashes <- (hh : ) <$> stateGetAllHashes

      let pt = toPTree (MaxSize 512) (MaxNum 512) hashes -- FIXME: settings

      tobj <- liftIO newTQueueIO
      -- FIXME: progress-indicator
      root <- makeMerkle 0 pt $ \(ha,_,bss) -> do
        liftIO $ atomically $ writeTQueue tobj (ha,bss)

      objs <- liftIO $ atomically $ flushTQueue tobj

      mon2 <- newProgressMonitor "store objects" (length objs)

      for_ objs $ \(ha,bss) -> do
        updateProgress mon2 1
        here <- lift $ getBlockSize (HashRef ha) <&> isJust
        unless here do
          void $ lift $ storeObject (fromString (show metaApp)) bss

      trace "generate update transaction"

      trace $ "objects:" <+> pretty (length hashes)

      seqno <- stateGetSequence <&> succ
      -- FIXME: same-transaction-different-seqno

      postRefUpdate h seqno (HashRef root)

      let noRef = do
            pause @'Seconds 20
            shutUp
            die $ show $ pretty "No reference appeared for" <+> pretty h

      wmon <- newProgressMonitor "waiting for ref" 20
      void $ liftIO $ race noRef $ do
              runApp NoLog do
                fix \next -> do
                  v <- readRefHttp h
                  updateProgress wmon 1
                  case v of
                    Nothing -> pause @'Seconds 1 >> next
                    Just{}  -> pure ()


      withDB db $ transactional $ mapM_ statePutExported ooo

      pure (HashRef root, hh)

  case rr of
    Left ( e :: SomeException ) -> do
      withDB db (savepointRollback sp)
      err $ viaShow e
      shutUp
      die "aborted"

    Right r -> do
      withDB db (savepointRelease sp)
      pure r


runExport :: forall m . (MonadIO m, MonadCatch m, HasProgress (App m))
          => Maybe FilePath -> RepoRef -> App m ()
runExport fp h = do

  let green = annotate (color Green)
  let yellow = annotate (color Yellow)
  let section = line <> line

  liftIO $ putDoc $
       line
    <> green "Exporting to reflog" <+> pretty (AsBase58 h)
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

  debug $ "HEAD" <+> pretty fullHead

  let repoHead = RepoHead (Just fullHead)
                          (HashMap.fromList refs)

  trace $ "NEW REPO HEAD" <+> pretty (AsGitRefsFile repoHead)

  (root, hhh) <- export h repoHead

  updateLocalState h

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
    <> pretty [qc|git remote add remotename hbs2://{pretty h}|]
    <> section
    <> green "Work with git as usual:"
    <> section
    <> "git pull remotename" <> line
    <> "(or git fetch remotename && git reset --hard remotename/branch)" <> line
    <> "git push remotename" <> line
    <> line


