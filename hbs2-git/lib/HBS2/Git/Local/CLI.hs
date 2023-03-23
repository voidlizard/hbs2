{-# Language AllowAmbiguousTypes #-}
module HBS2.Git.Local.CLI where

import HBS2.Git.Types

import HBS2.System.Logger.Simple

import Control.Concurrent.Async
import Control.Monad.IO.Class
import Control.Monad.Writer
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Function
import Data.Maybe
import Data.Set qualified as Set
import Data.Set (Set)
import Data.String
import Data.Text.Encoding qualified as Text
import Data.Text.Encoding (decodeLatin1)
import Data.Text qualified as Text
import Data.Text (Text)
import Prettyprinter
import Safe
import System.Process.Typed
import Text.InterpolatedString.Perl6 (qc)

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
        clos <- mapConcurrently (gitGetTransitiveClosure cache exclude) deps
        let res = (Set.fromList (hash:deps) <> Set.unions clos) `Set.difference` exclude
        cacheInsert cache hash res
        pure res


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
  (code, out, _) <- liftIO $ readProcess (shell cmd)

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


gitListAllObjects :: MonadIO m => m [GitHash]
gitListAllObjects = do
  let cmd = [qc|git cat-file --batch-check --batch-all-objects|]
  let procCfg = setStdin closed $ setStderr closed (shell cmd)
  (_, out, _) <- readProcess procCfg

  pure $ LBS.lines out & foldMap (fromLine . LBS.words)

  where
    fromLine = \case
      [ha, _, _] -> [fromString (LBS.unpack ha)]
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


