{-# Language AllowAmbiguousTypes #-}
module HBS2.Git.Local.CLI
  ( module HBS2.Git.Local.CLI
  , getStdin
  , getStdout
  , stopProcess
  ) where

import HBS2.Prelude.Plated
import HBS2.Git.Types

import HBS2.System.Logger.Simple

import Control.Concurrent.STM
import Control.Monad.Writer
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Functor
import Data.Function
import Data.Maybe
import Data.Set qualified as Set
import Data.Set (Set)
import Data.List qualified as List
import Data.Text.Encoding qualified as Text
import Data.Text.Encoding (decodeLatin1)
import Data.Text qualified as Text
import System.Process.Typed
import Text.InterpolatedString.Perl6 (qc)
import Control.Monad.Trans.Maybe
import System.IO

-- FIXME: specify-git-dir

parseHash :: BS8.ByteString -> GitHash
parseHash = fromString . BS8.unpack

parseHashLazy :: LBS.ByteString -> GitHash
parseHashLazy = fromString . BS8.unpack . LBS.toStrict

gitGetDepsPure :: GitObject -> Set GitHash

gitGetDepsPure (GitObject Tree bs) = Set.fromList $ execWriter (go bs)
  where
    go :: ByteString -> Writer [GitHash] ()
    go s = case LBS.uncons s of
      Nothing -> pure ()
      Just ('\x00', rest) -> do
        let (hash, rest') = LBS.splitAt 20 rest
        tell [GitHash (LBS.toStrict hash)]
        go rest'

      Just (_, rest) -> go rest

gitGetDepsPure (GitObject Commit bs) = Set.fromList (recurse ls)
  where
    ls = LBS.lines bs
    recurse :: [LBS.ByteString] -> [GitHash]
    recurse [] = []
    recurse ("":_)  = []
    recurse (x:xs) =
      case LBS.words x of
        ["tree", s]   -> fromString (LBS.unpack s) : recurse xs
        ["parent", s] -> fromString (LBS.unpack s) : recurse xs
        _             -> recurse xs


gitGetDepsPure _ = mempty

gitCommitGetParentsPure :: LBS.ByteString -> [GitHash]
gitCommitGetParentsPure bs = foldMap seek pairs
  where
    pairs = take 2 . LBS.words <$> LBS.lines bs
    seek = \case
      ["parent", x] -> [fromString (LBS.unpack x)]
      _             -> mempty

data GitParsedRef = GitCommitRef GitHash
                  | GitTreeRef GitHash
                    deriving stock (Data,Eq,Ord)

gitGetParsedCommit :: MonadIO m => GitObject -> m [GitParsedRef]
gitGetParsedCommit (GitObject Commit bs)  = do
  let ws = fmap LBS.words (LBS.lines bs)
  oo <- forM ws $ \case
          ["tree", s]   -> pure [GitTreeRef (fromString (LBS.unpack s))]
          ["commit", s] -> pure [GitCommitRef (fromString (LBS.unpack s))]
          _             -> pure mempty

  pure $ mconcat oo

gitGetParsedCommit _ = pure mempty

-- FIXME: use-fromStringMay
gitGetObjectType :: MonadIO m => GitHash -> m (Maybe GitObjectType)
gitGetObjectType hash = do
  (_, out, _) <- readProcess (shell [qc|git cat-file -t {pretty hash}|])
  case headMay (LBS.words out) of
    Just "commit" -> pure (Just Commit)
    Just "tree"   -> pure (Just Tree)
    Just "blob"   -> pure (Just Blob)
    _        -> pure Nothing



gitGetCommitDeps :: MonadIO m => GitHash -> m [GitHash]
gitGetCommitDeps hash  = do
  (_, out, _) <- readProcess (shell [qc|git cat-file commit {pretty hash}|])
  pure $ Set.toList (gitGetDepsPure (GitObject Commit out))

gitGetTreeDeps :: MonadIO m => GitHash -> m [GitHash]
gitGetTreeDeps hash = do
  (_, out, _) <- readProcess (shell [qc|git ls-tree {pretty hash}|])
  let ls =  fmap parseHash . getHash <$> BS8.lines (LBS.toStrict out)
  pure (catMaybes ls)
  where
    getHash = flip atMay 2 . BS8.words


gitGetDependencies :: MonadIO m => GitHash -> m [GitHash]
gitGetDependencies hash = do
  ot <- gitGetObjectType hash
  case ot of
    Just Commit -> gitGetCommitDeps hash
    Just Tree   -> gitGetTreeDeps hash
    _           -> pure mempty


-- | calculates all dependencies of given list
--   of git objects
gitGetAllDependencies :: MonadIO m
                       => Int  -- ^ number of threads
                       -> [ GitHash ] -- ^ initial list of objects to calculate deps
                       -> ( GitHash -> IO [GitHash] ) -- ^ lookup function
                       -> ( GitHash -> IO () ) -- ^ progress update function
                       -> m [(GitHash, GitHash)]

gitGetAllDependencies n objects lookup' progress = liftIO do
  input <- newTQueueIO
  output <- newTQueueIO

  memo  <- newTVarIO ( mempty :: HashSet GitHash )
  work  <- newTVarIO ( mempty :: HashMap Int Int )
  num   <- newTVarIO 1

  atomically $ mapM_ (writeTQueue input) objects

  replicateConcurrently_ n $ do

    i <- atomically $ stateTVar num ( \x -> (x, succ x) )

    fix \next -> do
      o <- atomically $ tryReadTQueue input
      case o of
        Nothing -> do
          todo <- atomically $ do
            modifyTVar work (HashMap.delete i)
            readTVar work <&> HashMap.elems <&> sum

          when (todo > 0) next

        Just h -> do

          progress h

          done <- atomically $ do
            here <- readTVar memo <&> HashSet.member h
            modifyTVar memo (HashSet.insert h)
            pure here

          unless done do
            cached <- lookup' h

            deps <- if null cached then do
                      gitGetDependencies h
                    else
                      pure cached

            forM_ deps $ \d -> do
              atomically $ writeTQueue output (h,d)

            atomically $ modifyTVar work (HashMap.insert i (length deps))

          next

  liftIO $ atomically $ flushTQueue output


gitGetTransitiveClosure :: forall cache . (HasCache cache GitHash (Set GitHash) IO)
                        => cache
                        -> Set GitHash
                        -> GitHash
                        -> IO (Set GitHash)

gitGetTransitiveClosure cache exclude hash = do
    -- trace $ "gitGetTransitiveClosure" <+> pretty hash
    r <- cacheLookup cache hash :: IO (Maybe (Set GitHash))
    case r of
      Just xs -> pure xs
      Nothing -> do
        deps <- gitGetDependencies hash
        clos <- mapM (gitGetTransitiveClosure cache exclude) deps
        let res = (Set.fromList (hash:deps) <> Set.unions clos) `Set.difference` exclude
        cacheInsert cache hash res
        pure res


-- gitGetAllDepsByCommit :: GitHash -> IO [GitHash]
-- gitGetAllDepsByCommit h = do
-- -- FIXME: error-handling
--   (_, out, _) <- liftIO $ readProcess (shell [qc|git rev-list {pretty h}|])
--   let ls =  LBS.lines out & fmap ( fromString . LBS.unpack )

--   forM ls $ \l -> do
--     o <- liftIO $ gitReadObject (Just Commit) l
--     let tree = gitGetDepsPure (GitObject Commit o)
--     (_, out, _) <- liftIO $ readProcess (shell [qc|git rev-list {pretty h}|])

--     print tree

--   -- mapM_ (print.pretty) ls
--   pure []
  -- deps <- mapM gitGetDependencies ls <&> mconcat
  -- pure $ List.nub $ ls <> deps

-- FIXME: inject-git-working-dir-via-typeclass

gitConfigGet :: MonadIO m => Text -> m (Maybe Text)
gitConfigGet k = do
  let cmd = [qc|git config {k}|]
  (code, out, _) <- liftIO $ readProcess (shell cmd)

  case code of
    ExitSuccess -> pure (Just $ Text.strip [qc|{LBS.unpack out}|])
    _           -> pure Nothing


gitConfigSet :: MonadIO m => Text -> Text -> m ()
gitConfigSet k v = do
  let cmd = [qc|git config --add {k} {v}|]
  liftIO $ putStrLn cmd
  runProcess_ (shell cmd)

gitGetRemotes :: MonadIO m => m [(Text,Text)]
gitGetRemotes = do
  let cmd = [qc|git config --get-regexp '^remote\..*\.url$'|]
  (_, out, _) <- liftIO $ readProcess (shell cmd)

  let txt = Text.decodeUtf8 (LBS.toStrict out)

  let ls = Text.lines txt -- & foldMap (drop 1 . Text.words)

  remotes <- forM ls $ \l ->
                case Text.words l of
                  [r,val] | Text.isPrefixOf "remote." r -> pure $ (,val) <$> stripRemote r
                  _       -> pure Nothing

  pure $ catMaybes remotes

  where
    stripRemote x = headMay $ take 1 $ drop 1 $ Text.splitOn "." x

-- FIXME: respect-git-workdir
gitHeadFullName :: MonadIO m => GitRef -> m GitRef
gitHeadFullName (GitRef r) = do
  let r' = Text.stripPrefix "refs/heads" r & fromMaybe r
  pure $ GitRef $ "refs/heads/" <> r'

-- FIXME: error handling!
gitReadObject :: MonadIO m => Maybe GitObjectType -> GitHash -> m LBS.ByteString
gitReadObject mbType' hash = do

  mbType'' <- case mbType' of
    Nothing -> gitGetObjectType hash
    Just tp -> pure (Just tp)

  oType <- maybe (error [qc|unknown type of {pretty hash}|]) pure mbType''

  -- liftIO $ hPutStrLn stderr [qc|git cat-file {pretty oType} {pretty hash}|]

  (_, out, _) <- readProcess (shell [qc|git cat-file {pretty oType} {pretty hash}|])

  pure out


gitRemotes :: MonadIO m => m (Set GitRef)
gitRemotes = do
  let cmd = setStdin closed $ setStdout closed
                            $ setStderr closed
                            $ shell [qc|git remote|]

  (_, out, _) <- readProcess cmd
  let txt = decodeLatin1 (LBS.toStrict out)
  pure $ Set.fromList (GitRef . Text.strip <$> Text.lines txt)


gitNormalizeRemoteBranchName :: MonadIO  m => GitRef -> m GitRef
gitNormalizeRemoteBranchName orig@(GitRef ref) = do
  remotes <- gitRemotes
  stripped <- forM (Set.toList remotes) $ \(GitRef remote) -> do
     pure $ GitRef <$> (("refs/heads" <>) <$> Text.stripPrefix remote ref)


  let GitRef r = headDef orig (catMaybes stripped)

  if Text.isPrefixOf "refs/heads" r
      then pure (GitRef r)
      else pure (GitRef $ "refs/heads/" <> r)


gitStoreObject :: MonadIO m => GitObject -> m (Maybe GitHash)
gitStoreObject (GitObject t s) = do
  let cmd = [qc|git hash-object -t {pretty t} -w --stdin|]
  let procCfg = setStdin (byteStringInput s) $ setStderr closed
                                             (shell cmd)
  (code, out, _) <- readProcess procCfg
  case code of
    ExitSuccess -> pure $ Just (parseHashLazy out)
    ExitFailure{} -> pure Nothing

gitCheckObject :: MonadIO m => GitHash -> m Bool
gitCheckObject gh = do
  let cmd = [qc|git cat-file -e {pretty gh}|]
  let procCfg = setStderr closed (shell cmd)
  (code, _, _) <- readProcess procCfg
  case code of
    ExitSuccess -> pure True
    ExitFailure{} -> pure False

gitListAllObjects :: MonadIO m => m [(GitObjectType, GitHash)]
gitListAllObjects = do
  let cmd = [qc|git cat-file --batch-check --batch-all-objects|]
  let procCfg = setStdin closed $ setStderr closed (shell cmd)
  (_, out, _) <- readProcess procCfg

  pure $ LBS.lines out & foldMap (fromLine . LBS.words)

  where
    fromLine = \case
      [ha, tp, _] -> [(fromString (LBS.unpack tp), fromString (LBS.unpack ha))]
      _          -> []

-- FIXME: better error handling
gitGetHash :: MonadIO m => GitRef -> m (Maybe GitHash)
gitGetHash ref = do

  trace $ "gitGetHash" <+> [qc|git rev-parse {pretty ref}|]

  (code, out, _) <- readProcess (shell [qc|git rev-parse {pretty ref}|])

  if code == ExitSuccess then do
    let hash = fromString . LBS.unpack <$> headMay (LBS.lines out)
    pure hash
  else
    pure Nothing

gitGetBranchHEAD :: MonadIO m => m (Maybe GitRef)
gitGetBranchHEAD = do
  (code, out, _) <- readProcess (shell [qc|git rev-parse --abbrev-ref HEAD|])

  if code == ExitSuccess then do
    let hash = fromString . LBS.unpack <$> headMay (LBS.lines out)
    pure hash
  else
    pure Nothing


gitListLocalBranches :: MonadIO m => m [(GitRef, GitHash)]
gitListLocalBranches  = do
  let cmd = [qc|git branch --format='%(objectname) %(refname)'|]
  let procCfg = setStdin closed $ setStderr closed (shell cmd)
  (_, out, _) <- readProcess procCfg

  pure $ LBS.lines out & foldMap (fromLine . LBS.words)

  where
    fromLine = \case
      [h, n] -> [(fromString (LBS.unpack n), fromString (LBS.unpack h))]
      _      -> []


gitListAllCommits :: MonadIO m => m [GitHash]
gitListAllCommits = do
  let cmd = [qc|git log --all --pretty=format:'%H'|]
  let procCfg = setStdin closed $ setStderr closed (shell cmd)
  (_, out, _) <- readProcess procCfg
  pure $ fmap (fromString . LBS.unpack) (LBS.lines out)

gitRunCommand :: MonadIO m => String -> m (Either ExitCode ByteString)
gitRunCommand cmd = do
  let procCfg = setStdin closed $ setStderr closed (shell cmd)
  (code, out, _) <- readProcess procCfg
  case code of
    ExitSuccess -> pure (Right out)
    e           -> pure (Left e)

-- | list all commits from the given one in order of date
gitListAllCommitsExceptBy :: MonadIO m => Set GitHash -> Maybe GitHash -> GitHash -> m [GitHash]
gitListAllCommitsExceptBy excl l h = do
  let from = maybe mempty (\r -> [qc|{pretty r}..|] ) l
  let cmd = [qc|git rev-list --reverse --date-order {from}{pretty h}|]
  let procCfg = setStdin closed $ setStderr closed (shell cmd)
  (_, out, _) <- readProcess procCfg
  let res = fmap (fromString . LBS.unpack) (LBS.lines out)
  pure $ List.reverse $ filter ( not . flip Set.member excl) res

-- | list all objects for the given commit range in order of date
gitRevList :: MonadIO m => Maybe GitHash -> GitHash -> m [GitHash]
gitRevList l h = do
  let from = maybe mempty (\r -> [qc|{pretty r}..|] ) l
  -- let cmd = [qc|git rev-list --objects --in-commit-order --reverse --date-order {from}{pretty h}|]
  -- let cmd = [qc|git rev-list --objects --reverse --in-commit-order {from}{pretty h}|]
  let cmd = [qc|git rev-list --reverse --in-commit-order --objects {from}{pretty h}|]
  let procCfg = setStdin closed $ setStderr closed (shell cmd)
  (_, out, _) <- readProcess procCfg
  pure $ mapMaybe (fmap (fromString . LBS.unpack) . headMay . LBS.words) (LBS.lines out)

-- TODO: handle-invalid-input-somehow
gitGetObjectTypeMany :: MonadIO m =>  [GitHash] -> m [(GitHash, GitObjectType)]
gitGetObjectTypeMany hashes = do
  let hss = LBS.unlines $ fmap (LBS.pack.show.pretty) hashes
  let cmd = [qc|git cat-file --batch-check='%(objectname) %(objecttype)'|]
  let procCfg = setStdin (byteStringInput hss) $ setStderr closed (shell cmd)
  (_, out, _) <- readProcess procCfg
  pure $ mapMaybe (parse . fmap LBS.unpack . LBS.words) (LBS.lines out)
  where
    parse [h,tp] = (,) <$> fromStringMay h <*> fromStringMay tp
    parse _ = Nothing

gitGetCommitImmediateDeps :: MonadIO m => GitHash -> m [GitHash]
gitGetCommitImmediateDeps h = do
  o <- gitReadObject (Just Commit) h
  let lws = LBS.lines o & fmap LBS.words

  t <- forM lws $ \case
        ["tree", hs]  -> pure (Just ( fromString @GitHash (LBS.unpack hs) ))
        _             -> pure Nothing

  let tree = take 1 $ catMaybes t

  deps <- gitRunCommand [qc|git rev-list --objects {pretty (headMay tree)}|]
            >>= either (const $ pure mempty)
                       (pure . mapMaybe withLine . LBS.lines)

  pure $ List.nub $ tree <> deps
  where
    withLine :: LBS.ByteString -> Maybe GitHash
    withLine l = do
      let wordsInLine = LBS.words l
      firstWord <- listToMaybe wordsInLine
      pure $ fromString @GitHash $ LBS.unpack firstWord


startGitHashObject :: MonadIO m => GitObjectType -> m (Process Handle () ())
startGitHashObject objType = do
  let cmd = "git"
  let args = ["hash-object", "-w", "-t", show (pretty objType), "--stdin-paths"]
  let config = setStdin createPipe $ setStdout closed $ setStderr inherit $ proc cmd args
  startProcess config

startGitCatFile :: MonadIO m => m (Process Handle Handle ())
startGitCatFile = do
  let cmd = "git"
  let args = ["cat-file", "--batch"]
  let config = setStdin createPipe $ setStdout createPipe $ setStderr closed $ proc cmd args
  startProcess config

gitReadFromCatFileBatch :: MonadIO m
                        => Process Handle Handle a
                        -> GitHash
                        -> m (Maybe GitObject)

gitReadFromCatFileBatch prc gh = do

    let ssin = getStdin prc
    let sout = getStdout prc

    liftIO $ hPrint  ssin (pretty gh) >> hFlush ssin

    runMaybeT do

      here <- liftIO $ hWaitForInput sout 1000

      guard here

      header <- liftIO $ BS8.hGetLine sout

      case BS8.unpack <$> BS8.words header of
        [ha, t, s] -> do
          (_, tp, size) <- MaybeT $ pure $ (,,) <$> fromStringMay @GitHash ha
                                                <*> fromStringMay @GitObjectType t
                                                <*> readMay s

          content <- liftIO $ LBS.hGet sout size

          guard (LBS.length content == fromIntegral size)

          void $ liftIO $ LBS.hGet sout 1

          let object = GitObject tp content

          -- TODO: optionally-check-hash
          -- guard (gh== gitHashObject object)

          pure object

        _                -> MaybeT $ pure Nothing


