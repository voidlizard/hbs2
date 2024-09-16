{-# Language MultiWayIf #-}
{-# Language PatternSynonyms #-}
{-# Language ViewPatterns #-}
module Fixme.Scan.Git.Local where


import Prelude hiding (init)
import Fixme.Prelude hiding (indent)
import Fixme.Types
import Fixme.State
import Fixme.Scan as Scan

import HBS2.Storage
import HBS2.Storage.Compact
import HBS2.System.Dir
import HBS2.Git.Local.CLI

import DBPipe.SQLite hiding (field)
import Data.Config.Suckless
import Data.Text.Fuzzy.Tokenize

import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.ByteString.Lazy (ByteString)
import Data.Either
import Data.Fixed
import Data.List qualified as List
import Data.List.Split (chunksOf)
import Data.Maybe
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.HashSet (HashSet)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (ignore)
import Data.Word
import Text.InterpolatedString.Perl6 (qc)
import Data.Coerce
import Data.Generics.Product.Fields (field)
import Lens.Micro.Platform
import System.Process.Typed
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Maybe
import System.IO qualified as IO
import System.IO.Temp (emptySystemTempFile)
import System.TimeIt

import Data.Map qualified as Map

import Streaming.Prelude qualified as S


{- HLINT ignore "Functor law" -}


listCommits :: FixmePerks m => FixmeM m [(GitHash, HashMap FixmeAttrName FixmeAttrVal)]
listCommits = do
  gd <- fixmeGetGitDirCLIOpt

  days <- asks fixmeEnvGitScanDays
            >>= readTVarIO
            <&> fmap ( \x -> "--since" <+> squotes (pretty x <+> "days ago"))
            <&> fromMaybe mempty
            <&> show

  let cmd = [qc|git {gd} log --all --format="%H '%cn' '%ce' %ct" {days}|]

  debug $ yellow "listCommits" <+> pretty cmd

  gitRunCommand cmd
    <&> fromRight mempty
    <&> LBS8.lines
    <&> mapMaybe extract

  where
    extract :: ByteString -> Maybe (GitHash, HashMap FixmeAttrName FixmeAttrVal)
    extract lbs = do
      let txt = decodeUtf8With ignore (LBS8.toStrict lbs)
      let r = tokenize @Text spec txt
      case r of
        [co, n, e, t] -> do
          let gh = fromStringMay @GitHash (Text.unpack co)

          let bag = [ ("commit", co)
                    , ("commit-time", t)
                    , ("committer-name", n)
                    , ("committer-email", e)
                    , ("committer", [qc|{n} <{e}>|])
                    ] & fmap ( over _1 FixmeAttrName . over _2 FixmeAttrVal)
                      & HM.fromList

          (,) <$> gh <*> pure bag

        _ -> Nothing

    spec = sq <> delims " \t"

listBlobs :: (FixmePerks m, MonadReader FixmeEnv m) => Maybe GitHash -> m [(FilePath,GitHash)]
listBlobs mco = do
  gd <- fixmeGetGitDirCLIOpt

  let what = maybe "HEAD" (show . pretty) mco

  gitRunCommand [qc|git {gd} ls-tree -r -l -t {what}|]
    <&> fromRight mempty
    <&> fmap LBS8.words . LBS8.lines
    <&> mapMaybe
          (\case
             [a,"blob",h,_,fn] -> (LBS8.unpack fn,) <$> fromStringMay @GitHash (LBS8.unpack h)
             _                 -> Nothing)

filterBlobs0 :: FixmePerks m
            => [(Bool,FilePattern)]
            -> [(FilePath,GitHash)]
            -> FixmeM m [(FilePath,GitHash)]

filterBlobs0 pat xs = do
  -- pat <- asks fixmeEnvFileMask >>= readTVarIO <&> fmap (True,)
  let src = [ ((f,h),f) | (f,h) <- xs ]
  let r = [(h,f) | (_,(f,h),_) <- matchMany pat src] & HM.fromList & HM.toList
  pure $ [ (b,a) | (a,b) <- r ]

filterBlobs :: FixmePerks m
            => [(FilePath,GitHash)]
            -> FixmeM m [(FilePath,GitHash)]

filterBlobs xs = do
  pat <- asks fixmeEnvFileMask >>= readTVarIO <&> fmap (True,)
  filterBlobs0 pat xs

listRelevantBlobs :: FixmePerks m
                  => FixmeM m [(FilePath, GitHash)]
listRelevantBlobs =  do
  commits <- listCommits
  S.toList_ $ do
    for_ commits $ \(co, _) -> do
      found <- lift $ listBlobs (Just co) >>= filterBlobs
      S.each found

listFixmies :: FixmePerks m
            => FixmeM m [Fixme]
listFixmies = do

  flip runContT pure do

      blobs <- lift listRelevantBlobs

      gitCat <- ContT $ bracket startGitCatFile (hClose . getStdin)

      let ssin   = getStdin gitCat
      let ssout  = getStdout gitCat

      liftIO $ IO.hSetBuffering ssin LineBuffering

      for_ blobs $ \(fp,h) -> do
        liftIO $ IO.hPrint ssin (pretty h) >> IO.hFlush ssin
        prefix <- liftIO (BS.hGetLine ssout) <&> BS.words

        case prefix of
          [bh, "blob", ssize] -> do
            let mslen = readMay @Int (BS.unpack ssize)
            len <- ContT $ maybe1 mslen (pure ())
            blob <- liftIO $ LBS8.hGet ssout len
            void $ liftIO $ BS.hGetLine ssout

            poor <- lift (Scan.scanBlob (Just fp) blob)

            liftIO $ mapM_ (print . pretty) poor

          _ -> pure ()



  pure mempty


gitListStage :: (FixmePerks m)
             => FixmeM m [Either (FilePath, GitHash) (FilePath, GitHash)]
gitListStage = do
  gd <- fixmeGetGitDirCLIOpt
  modified <- gitRunCommand [qc|git {gd} status --porcelain|]
                <&> fromRight mempty
                <&> fmap LBS8.words . LBS8.lines
                <&> mapMaybe ( \case
                       ["M", fn] -> Just (LBS8.unpack fn)
                       _         -> Nothing
                    )

  new <- S.toList_ $ do
            for_ modified $ \fn -> void $ runMaybeT do

              e <- gitRunCommand [qc|git {gd} hash-object {fn}|]
                     >>= toMPlus
                     <&> maybe mempty LBS8.unpack . headMay . LBS8.words
                     <&> fromStringMay @GitHash
                     >>= toMPlus

              lift (S.yield $ (fn,e))

  old <- gitRunCommand [qc|git {gd} ls-files -s|]
          <&> fromRight mempty
          <&> fmap LBS8.words . LBS8.lines
          <&> mapMaybe ( \case
                [_, h, _, fn] -> (LBS8.unpack fn,) <$> fromStringMay @GitHash (LBS8.unpack h)
                _             -> Nothing
                )

  new1 <- filterBlobs new <&> fmap Left
  old1 <- filterBlobs old <&> fmap Right

  pure (old1 <> new1)


getMetaDataFromGitBlame :: FixmePerks m => FilePath -> Fixme -> FixmeM m Fixme
getMetaDataFromGitBlame f fx0 = do
  gd <- fixmeGetGitDirCLIOpt
  fromMaybe mempty <$> runMaybeT do
    l0 <- fixmeStart fx0 & toMPlus <&> fromIntegral <&> succ
    let cmd = [qc|git {gd} blame {f} -L{l0},{l0} -t -l -p|]

    s0 <- gitRunCommand cmd
             <&> LBS8.unpack . fromRight mempty

    s <- parseTop s0 & toMPlus

    let ko = headMay (words <$> lines s0)
                   >>= headMay
                   >>= (\z -> do
                         if z == "0000000000000000000000000000000000000000"
                           then Nothing
                           else Just z )
                   >>= fromStringMay @GitHash

    pieces <- for s $ \case
      ListVal (SymbolVal "committer" : StringLikeList  w) | isJust ko -> do
        let co = FixmeAttrVal $ fromString $ unwords w
        pure $ mempty { fixmeAttr = HM.singleton "committer-name" co }

      ListVal (SymbolVal "committer-mail" : StringLikeList  w) | isJust ko -> do
        let co = FixmeAttrVal $ fromString $ unwords w
        pure $ mempty { fixmeAttr = HM.singleton "committer-email" co }

      ListVal [SymbolVal "committer-time", TimeStampLike t] | isJust ko  -> do
        let ct = FixmeAttrVal $ fromString $ show t
        pure $ mempty { fixmeAttr = HM.singleton "commit-time" ct, fixmeTs = Just t }

      _ -> pure mempty

    let coco = mempty { fixmeAttr = maybe mempty (HM.singleton "commit" . fromString . show . pretty) ko }

    pure $ mconcat pieces <> coco

gitExtractFileMetaData :: FixmePerks m => [FilePath] -> FixmeM m (HashMap FilePath Fixme)
gitExtractFileMetaData fns = do
  -- FIXME: magic-number
  let chunks = chunksOf 64 fns

  gd <- fixmeGetGitDirCLIOpt

  commitz <- S.toList_ $ for_ chunks $ \chu -> do
      let filez = unwords chu
      let cmd = [qc|git {gd} log --diff-filter=AMR --pretty=format:'entry %H %at "%an" "%ae"' -- {filez}|]
      ss <- gitRunCommand cmd
             <&> fromRight mempty
             <&> fmap LBS8.unpack . LBS8.lines

      for_ ss $ \s -> do
        let syn = parseTop s & fromRight mempty
        case syn of
          [ListVal [SymbolVal "entry", SymbolVal (Id e), LitIntVal t, StringLike n, StringLike m]] -> do
            -- liftIO $ print $ pretty e <+> pretty syn
            S.yield (fromString @GitHash (Text.unpack e), (t,n,m) )

          _ -> pure ()

  let co = HM.fromList commitz
             & HM.toList

  rich0 <- S.toList_ $ do
    for_ co $ \(c, (t,n,m)) -> do
      let pat = [ (True, f) | f <- fns ]
      blobz <- lift $ listBlobs (Just c) >>= filterBlobs0 pat

      for_ blobz $ \(f,h) -> do
        let attr = HM.fromList [ ("commit",          FixmeAttrVal (fromString $ show $ pretty c))
                               , ("commit-time",     FixmeAttrVal (fromString $ show $ pretty t))
                               , ("committer-name",  FixmeAttrVal (fromString n))
                               , ("committer-email", FixmeAttrVal (fromString m))
                               , ("committer",       FixmeAttrVal (fromString $ [qc|{n} <{m}>|]))
                               , ("file",            FixmeAttrVal (fromString f))
                               , ("blob",            FixmeAttrVal (fromString $ show $ pretty $ h))
                               ]
        let what = mempty { fixmeAttr = attr }
        S.yield (f,t,what)

  let rich = List.sortBy (\a b -> compare (view _2 a) (view _2 b)) rich0

  pure $ HM.fromList [ (view _1 w, view _3 w) | w <- rich ]


data GitBlobInfo = GitBlobInfo FilePath GitHash
                   deriving stock (Eq,Ord,Data,Generic,Show)

instance Hashable GitBlobInfo

data GitIndexEntry =
       GitCommit Word64 (HashSet GitBlobInfo)
       deriving stock (Eq,Ord,Data,Generic,Show)

instance Serialise GitBlobInfo
instance Serialise GitIndexEntry

listCommitForIndex :: forall m . (FixmePerks m, MonadReader FixmeEnv m) => ( (GitHash, GitIndexEntry) -> m ()) -> m ()
listCommitForIndex fn = do

  gd <- fixmeGetGitDirCLIOpt
  let cmd = [qc|git {gd} log --all --format="%H %ct"|]

  debug $ yellow "listCommits" <+> pretty cmd

  s0 <- gitRunCommand cmd
    <&> fromRight mempty
    <&> fmap (words . LBS8.unpack) . LBS8.lines
    <&> mapMaybe ( \case
         [a,b] -> (,) <$> fromStringMay @GitHash a <*> makeIndexEntry0 a b
         _     -> Nothing
         )

  for_ s0 $ \(h, GitCommit w _) -> do
    blobz <- listBlobs (Just h) <&> HS.fromList . fmap ( uncurry GitBlobInfo  )
    fn (h, GitCommit w blobz)

  where
    makeIndexEntry0 _ t = GitCommit <$> readMay t <*> pure mempty

gitCatBlob :: (FixmePerks m, MonadReader FixmeEnv m) => GitHash -> m ByteString
gitCatBlob h = do
  gd <- fixmeGetGitDirCLIOpt
  (_,s,_)  <- readProcess $ shell [qc|git {gd} cat-file blob {pretty h}|]
  pure s


startGitHash ::  (FixmePerks m, MonadReader FixmeEnv m) => m (Process Handle Handle ())
startGitHash = do
  gd <- fixmeGetGitDirCLIOpt
  let cmd = [qc|git {gd} hash-object --stdin-paths|]
  debug $ pretty cmd
  let config = setStdin createPipe $ setStdout createPipe $ setStderr closed $ shell cmd
  startProcess config

gitHashPathStdin :: FixmePerks m => (Process Handle Handle e) -> FilePath -> FixmeM m (Maybe GitHash)
gitHashPathStdin prc file  = do
  let ssin = getStdin prc
  let sout = getStdout prc
  liftIO $ IO.hPutStrLn ssin file >> IO.hFlush ssin
  liftIO (IO.hGetLine sout) <&> fromStringMay @GitHash

startGitCatFile ::  (FixmePerks m, MonadReader FixmeEnv m) => m (Process Handle Handle ())
startGitCatFile = do
  gd <- fixmeGetGitDirCLIOpt
  let cmd = [qc|git {gd} cat-file --batch|]
  debug $ pretty cmd
  let config = setStdin createPipe $ setStdout createPipe $ setStderr closed $ shell cmd
  -- ssin <- getStdin config
  startProcess config



