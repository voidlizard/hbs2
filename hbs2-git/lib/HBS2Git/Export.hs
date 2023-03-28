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

import Data.Functor
import Data.List (sortBy)
import Control.Applicative
import Control.Monad.Reader
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Cache as Cache
import Data.Foldable (for_)
import Data.HashMap.Strict qualified as HashMap
import Data.Maybe
import Data.Set qualified as Set
import Data.Set (Set)
import Lens.Micro.Platform
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Monad.Catch

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

  cache <- newHashCache db

  notice "calculate dependencies"

  for_ refs $ \(_, h) -> do
    liftIO $ gitGetTransitiveClosure cache mempty h <&> Set.toList

  -- notice "store dependencies to state"
  -- hashes <- readHashesFromBlock undefined

  sz   <- liftIO $ Cache.size (hCache cache)
  mon1 <- newProgressMonitor "storing dependencies" sz

  withDB db $ transactional do
    els <- liftIO $ Cache.toList (hCache cache)
    for_ els $ \(k,vs,_) -> do
      updateProgress mon1 1
      for_ (Set.toList vs) $ \h -> do
        stateAddDep k h

  deps <- withDB db $ do
            x <- forM refs $ stateGetDeps . snd
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

    pure (HashRef root, hh)


runExport :: forall m . (MonadIO m, MonadCatch m, HasProgress (App m))
          => Maybe FilePath -> RepoRef -> App m ()
runExport fp h = do

  trace $ "Export" <+> pretty (AsBase58 h)

  git <- asks (view appGitDir)

  trace $ "git directory is" <+> pretty git

  loadCredentials (maybeToList fp)

  branches   <- cfgValue @ConfBranch

  -- FIXME: wtf-runExport
  branchesGr <- cfgValue @ConfBranch <&> Set.map normalizeRef
  headBranch' <- cfgValue @HeadBranch

  trace $ "BRANCHES" <+> pretty (Set.toList branches)

  let defSort a b = case (a,b) of
        ("master",_) -> LT
        ("main", _)  -> LT
        _            -> GT

  let sortedBr = sortBy defSort $ Set.toList branches

  let headBranch = fromMaybe "master"
                     $ headBranch' <|> (fromString <$> headMay sortedBr)

  refs <- gitListLocalBranches
             <&> filter (\x -> Set.member (fst x) branchesGr)

  trace $ "REFS" <+> pretty refs

  fullHead <- gitHeadFullName headBranch

  debug $ "HEAD" <+> pretty fullHead

  let repoHead = RepoHead (Just fullHead)
                          (HashMap.fromList refs)

  trace $ "NEW REPO HEAD" <+> pretty (AsGitRefsFile repoHead)

  (root, hhh) <- export h repoHead

  updateLocalState h

  info  $ "head:" <+> pretty hhh
  info  $ "merkle:" <+> pretty root

