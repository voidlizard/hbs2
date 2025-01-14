module HBS2.Git3.Git
  ( module HBS2.Git3.Git
  , module HBS2.Git.Local
  , module HBS2.Git.Local.CLI
  ) where

import HBS2.Prelude.Plated
import HBS2.OrDie

import HBS2.Git3.Types
import HBS2.Git.Local
import HBS2.Git.Local.CLI

import Data.Config.Suckless.Script

import Crypto.Hash (hashlazy)
import Crypto.Hash qualified as Crypton
import Control.Monad.Trans.Maybe
import Data.ByteArray qualified as BA
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy ( ByteString )
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.ByteString.Lazy qualified as LBS
import Data.Either
import Data.HashMap.Strict qualified as HM
import Data.List (sortOn)
import Data.Maybe
import Data.Word
import System.FilePath
import System.IO (hPrint,hGetLine)
import System.Process.Typed
import Text.InterpolatedString.Perl6 (qc)
import UnliftIO

{-HLINT Ignore "Functor law"-}

pattern GitHashLike:: forall {c} . GitHash -> Syntax c
pattern GitHashLike x <- (
  \case
    StringLike s      -> fromStringMay @GitHash s
    _                -> Nothing
      -> Just x )

data GitException =
    CompressionError String
  | DecompressionError String
  | InvalidObjectFormat GitObjectType (Maybe GitHash)
  | InvalidGitPack ByteString
  | OtherGitError String
  | UnknownRev String
  | GitReadError String
  | GitImportError String
  deriving stock (Eq,Show,Typeable,Generic)

instance Exception GitException

data GitTreeEntry =
  GitTreeEntry
  { gitEntryAttribs :: Word16
  , gitEntryType    :: GitObjectType
  , gitEntryHash    :: GitHash
  , gitEntrySize    :: Maybe Word32
  , gitEntryName    :: FilePath
  }
  deriving (Show)

pattern GitTreeEntryView :: GitTreeEntry -> [ByteString]
pattern GitTreeEntryView e <- (isGitLsTreeEntry -> Just e)

gitNormaliseRef :: GitRef -> GitRef
gitNormaliseRef r@(GitRef what) =
  if BS8.isPrefixOf "refs/" what || what == "HEAD" then
    r
  else
    fromString (joinPath $ splitPath $ "refs" </> "heads" </> BS8.unpack what)

isGitLsTreeEntry :: [ByteString] -> Maybe GitTreeEntry
isGitLsTreeEntry = \case
  [sa,st,sh,ss,sn] -> do
    GitTreeEntry <$> readMay @Word16 (LBS8.unpack sa)
                 <*> fromStringMay (LBS8.unpack st)
                 <*> fromStringMay (LBS8.unpack sh)
                 <*> pure (readMay (LBS8.unpack ss))
                 <*> pure (LBS8.unpack sn)

  _ -> Nothing

gitReadTreeObjectsOnly :: (Pretty what, MonadIO m) => what -> m [GitHash]
gitReadTreeObjectsOnly what =
  gitRunCommand [qc|git ls-tree -t -r --object-only {pretty what}|]
     >>= orThrow (GitReadError (show $ pretty what))
     <&> fmap LBS8.words . LBS8.lines
     <&> mapMaybe \case
          [ x ] -> fromStringMay @GitHash (LBS8.unpack x)
          _ -> Nothing

gitReadTree :: (Pretty what, MonadIO m) => what -> m [GitTreeEntry]
gitReadTree what =
  gitRunCommand [qc|git ls-tree -t -l -r {pretty what}|]
     >>= orThrow (GitReadError (show $ pretty what))
     <&> fmap LBS8.words . LBS8.lines
     <&> mapMaybe \case
          GitTreeEntryView v -> do
            Just v
          _ -> Nothing
     <&> \s -> HM.elems (HM.fromList [ (gitEntryHash e, e) | e <- s])

class GitObjectReader a where
  gitReadObjectMaybe :: forall m . MonadIO m => a -> GitHash -> m (Maybe (GitObjectType, ByteString))

gitReadObjectOrThrow :: forall m a . (MonadIO m, GitObjectReader a) => a -> GitHash -> m (GitObjectType, ByteString)
gitReadObjectOrThrow r co =
  gitReadObjectMaybe r co >>= orThrow (GitReadError (show $ pretty co))

gitReadObjectThrow :: (Pretty h, MonadIO m) => GitObjectType -> h -> m ByteString
gitReadObjectThrow t h = do
  gitRunCommand [qc|git cat-file {pretty t} {pretty h}|]
    >>= orThrowPassIO

gitRevParse :: (Pretty ref, MonadIO m) => ref -> m (Maybe GitHash)
gitRevParse ref = do
  gitRunCommand [qc|git rev-parse {pretty ref}|]
    >>= orThrowPassIO
    <&> LBS8.words
    <&> maybe Nothing (fromStringMay . LBS8.unpack) . headMay

gitRevParseThrow :: (Pretty ref, MonadIO m) => ref -> m GitHash
gitRevParseThrow r = gitRevParse r >>= orThrow (UnknownRev (show $ pretty r))

gitImportObjectSlow :: MonadIO m => GitObjectType -> ByteString -> m (Either ExitCode ())
gitImportObjectSlow t lbs = do
  let cmd = "git"
  let args = ["hash-object", "-w", "--stdin", "-t", (show $ pretty t)]
  let config = setStdin (byteStringInput lbs) $ setStdout closed $ setStderr closed $ proc cmd args
  code <- runProcess config
  pure $ if (code /= ExitSuccess) then Left code else Right ()

gitReadHEAD :: MonadIO m => m (Maybe GitRef)
gitReadHEAD = runMaybeT do
  gitRunCommand [qc|git symbolic-ref HEAD|]
   >>= toMPlus
   <&> headMay . LBS8.lines
   >>= toMPlus
   <&> GitRef . LBS8.toStrict

withGitCat ::  (MonadIO m) => (Process Handle Handle () -> m a) -> m a
withGitCat action = do
  p <- startGitCat
  action p

startGitCat :: MonadIO m => m (Process Handle Handle ())
startGitCat = do
  let cmd = "git"
  let args = ["cat-file", "--batch"]
  let config = setStdin createPipe $ setStdout createPipe $ setStderr closed $ proc cmd args
  startProcess config

withGitCatCheck ::  (MonadIO m) => (Process Handle Handle () -> m a) -> m a
withGitCatCheck action = do
  let cmd = "git"
  let args = ["cat-file", "--batch-check"]
  let config = setStdin createPipe $ setStdout createPipe $ setStderr closed $ proc cmd args
  p <- startProcess config
  action p


withGitShowIndex ::  (MonadIO m) => (Process Handle Handle () -> m a) -> m a
withGitShowIndex  action = do
  let cmd = "git"
  let args = ["show-index"]
  let config = setStdin createPipe $ setStdout createPipe $ setStderr closed $ proc cmd args
  p <- startProcess config
  action p

gitCheckObjectFromHandle :: MonadIO m => Process Handle Handle a -> GitHash -> m (Maybe (GitObjectType, Int))
gitCheckObjectFromHandle ph gh = liftIO do

  let ssin = getStdin ph
  let ssout = getStdout ph

  hPrint ssin (pretty gh) >> hFlush ssin

  s <- hGetLine ssout

  runMaybeT do

    case words s of
      [_,t,ss] -> do
        n <- readMay @Int ss & toMPlus
        o <- fromStringMay @GitObjectType t & toMPlus
        pure $ (o,n)

      [_,"missing"] -> do
        mzero

      w -> throwIO (GitReadError $ show (pretty w))


gitReadCommitParents :: MonadIO m => Maybe GitHash -> ByteString -> m [GitHash]
gitReadCommitParents _ bs = do
   pure $ LBS8.lines bs
    & takeWhile ( not . LBS8.null )
    & fmap (words . LBS8.unpack)
    & mapMaybe \case
        ["parent", x]  -> fromStringMay @GitHash x
        _ -> Nothing

gitReadCommitTree :: MonadIO m => ByteString -> m GitHash
gitReadCommitTree bs = do

   what <- LBS8.lines bs
             & takeWhile ( not . LBS8.null )
             & LBS8.unpack . LBS8.unlines
             & parseTop
             & orThrow (OtherGitError "invalid commit format")

   let r = [ fromStringMay @GitHash hash
           | ListVal [ StringLike "tree", StringLike hash ] <- what
           ]

   catMaybes r & headMay & orThrow (InvalidObjectFormat Commit Nothing)

gitObjectExists :: (MonadIO m, Pretty what) => what -> m Bool
gitObjectExists what = do
  gitRunCommand [qc|git cat-file -e {pretty what}|] <&> isRight


instance GitObjectReader (Process Handle Handle ()) where
  gitReadObjectMaybe ph co = liftIO do

    let ssin = getStdin ph
    let ssout = getStdout ph

    hPrint ssin $ pretty co
    hFlush ssin

    s <- hGetLine ssout

    runMaybeT do

      case words s of
        [_,t,ss] -> do
          n <- readMay @Int ss & toMPlus
          o <- fromStringMay @GitObjectType t & toMPlus
          bs <- lift $ LBS.hGet ssout n
          void $ lift $ hGetLine ssout
          pure (o,bs)

        [_,"missing"] -> do
          mzero

        w -> throwIO (GitReadError $ show (pretty w))


sortGitTreeEntries :: [GitTreeEntry] -> [GitTreeEntry]
sortGitTreeEntries = sortOn (\e -> (gitEntryType e, gitEntrySize e))

gitHashBlobPure :: ByteString -> GitHash
gitHashBlobPure body = do
  let preamble = [qc|{pretty Blob} {pretty $ LBS.length body}|] <> "\x00" :: LBS8.ByteString
  GitHash $ BS.pack $ BA.unpack $ hashlazy @Crypton.SHA1 (preamble <> body)

