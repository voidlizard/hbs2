{-# Language MultiWayIf #-}
{-# Language PatternSynonyms #-}
{-# Language ViewPatterns #-}
module Fixme.Run where

import Prelude hiding (init)
import Fixme.Prelude hiding (indent)
import Fixme.Types
import Fixme.Scan.Git as Git
import Fixme.Scan as Scan

import HBS2.Git.Local.CLI

import HBS2.System.Dir

import Data.Config.Suckless
import Data.Text.Fuzzy.Tokenize

import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.ByteString.Lazy (ByteString)
import Data.Either
import System.Environment
import Data.Maybe
import Data.HashSet qualified as HS
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (ignore)
import Text.InterpolatedString.Perl6 (qc)
import Data.Coerce
import Control.Monad.Identity
import Lens.Micro.Platform
import System.Process.Typed
import Control.Monad.Trans.Cont
import System.IO qualified as IO

import Streaming.Prelude qualified as S

{- HLINT ignore "Functor law" -}

pattern Init :: forall {c}. Syntax c
pattern Init <- ListVal [SymbolVal "init"]

pattern ScanGitLocal :: forall {c}. [ScanGitArgs] -> Syntax c
pattern ScanGitLocal e <- ListVal (SymbolVal "scan-git" : (scanGitArgs -> e))



pattern ReadFixmeStdin :: forall {c}.  Syntax c
pattern ReadFixmeStdin <- ListVal [SymbolVal "read-fixme-stdin"]

pattern FixmeFiles :: forall {c} . [FilePattern] -> Syntax c
pattern FixmeFiles e  <- ListVal (SymbolVal "fixme-files" : (fileMasks -> e))


pattern FixmePrefix :: forall {c} . FixmeTag -> Syntax c
pattern FixmePrefix s <- ListVal [SymbolVal "fixme-prefix", fixmePrefix -> Just s]

pattern FixmeGitScanFilterDays :: forall {c}. Integer -> Syntax c
pattern FixmeGitScanFilterDays d <- ListVal [ SymbolVal "fixme-git-scan-filter-days", LitIntVal d ]

pattern StringLike :: forall {c} . String -> Syntax c
pattern StringLike e <- (stringLike -> Just e)

pattern StringLikeList :: forall {c} . [String] -> [Syntax c]
pattern StringLikeList e <- (stringLikeList -> e)


data ScanGitArgs =
    PrintBlobs
  | PrintFixme
  deriving stock (Eq,Ord,Show,Data,Generic)

pattern ScanGitArgs :: forall {c} . ScanGitArgs -> Syntax c
pattern ScanGitArgs w <- ( scanGitArg -> Just w )

scanGitArg :: Syntax c -> Maybe ScanGitArgs
scanGitArg = \case
  SymbolVal "print-blobs" -> Just PrintBlobs
  SymbolVal "print-fixme" -> Just PrintFixme
  _ -> Nothing

scanGitArgs :: [Syntax c] -> [ScanGitArgs]
scanGitArgs syn = [ w | ScanGitArgs w <- syn ]

stringLike :: Syntax c -> Maybe String
stringLike = \case
  LitStrVal s -> Just $ Text.unpack s
  SymbolVal (Id s) -> Just $ Text.unpack s
  _ -> Nothing

stringLikeList :: [Syntax c] -> [String]
stringLikeList syn = [ stringLike s | s <- syn ] & takeWhile isJust & catMaybes

fileMasks :: [Syntax c] -> [FilePattern]
fileMasks what = [ show (pretty s) | s <- what ]

fixmePrefix :: Syntax c -> Maybe FixmeTag
fixmePrefix = \case
  SymbolVal s -> Just (FixmeTag (coerce s))
  _ -> Nothing

binName :: FixmePerks m => m FilePath
binName = liftIO getProgName

localConfigDir :: FixmePerks m => m FilePath
localConfigDir = do
  p <- pwd
  b <- binName
  pure (p </> ("." <> b))

localConfig:: FixmePerks m => m FilePath
localConfig = localConfigDir <&> (</> "config")


readConfig :: FixmePerks m => FixmeM m [Syntax C]
readConfig = do
  localConfig
     >>= try @_ @IOException . liftIO . readFile
     <&> fromRight mempty
     <&> parseTop
     <&> fromRight mempty

init :: FixmePerks m => FixmeM m ()
init = do
  lo <- localConfigDir

  let lo0 = takeFileName lo

  mkdir lo
  touch (lo </> "config")

  let gitignore = lo </> ".gitignore"
  here <- doesPathExist gitignore

  unless here do
    liftIO $ writeFile gitignore $ show $
      vcat [ "./state.db"
           ]

  notice $ yellow "run" <> line <> vcat [
      "git add" <+> pretty (lo0  </> ".gitignore")
    , "git add" <+> pretty (lo0  </> "config")
    ]

listCommits :: FixmePerks m => FixmeM m [(GitHash, HashMap FixmeAttrName FixmeAttrVal)]
listCommits = do
  gd <- fixmeGetGitDirCLIOpt

  days <- asks fixmeEnvGitScanDays
            >>= readTVarIO
            <&> fmap ( \x -> "--since" <+> squotes (pretty x <+> "days ago"))
            <&> fromMaybe mempty
            <&> show

  let cmd = [qc|git {gd} log --all --format="%H '%cn' '%ce' %ct" {days}|]

  -- FIXME: git-dir
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

          let bag = [ ("commit-hash", co)
                    , ("commit-time", t)
                    , ("committer-name", n)
                    , ("committer-email", e)
                    , ("committer", [qc|{n} <{e}>|])
                    ] & fmap ( over _1 FixmeAttrName . over _2 FixmeAttrVal)
                      & HM.fromList

          (,) <$> gh <*> pure bag

        _ -> Nothing

    spec = sq <> delims " \t"


listBlobs :: FixmePerks m => GitHash -> m [(FilePath,GitHash)]
listBlobs co = do
  -- FIXME: git-dir
  gitRunCommand [qc|git ls-tree -r -l -t {pretty co}|]
    <&> fromRight mempty
    <&> fmap LBS8.words . LBS8.lines
    <&> mapMaybe
          (\case
             [a,"blob",h,_,fn] -> (LBS8.unpack fn,) <$> fromStringMay @GitHash (LBS8.unpack h)
             _                 -> Nothing)

filterBlobs :: FixmePerks m
            => [(FilePath,GitHash)]
            -> FixmeM m [(FilePath,GitHash)]

filterBlobs xs = do
  pat <- asks fixmeEnvFileMask >>= readTVarIO <&> fmap (True,)
  let src = [ ((f,h),f) | (f,h) <- xs ]
  let r = [(h,f) | (_,(f,h),_) <- matchMany pat src] & HM.fromList & HM.toList
  pure $ [ (b,a) | (a,b) <- r ]

scanGitLocal :: FixmePerks m => [ScanGitArgs] -> Maybe FilePath -> FixmeM m ()
scanGitLocal args p =  do

  flip runContT pure do

    co <- lift listCommits

    blobs <- lift $ mconcat <$> for co (\c -> do
                            listBlobs (fst c) >>= filterBlobs )

    when ( PrintBlobs `elem` args ) do
      for_ blobs $ \(fp,h) -> do
        liftIO $ print $ pretty h <+> pretty fp

    gitCat <- ContT $ bracket startGitCatFile (hClose . getStdin)

    let ssin   = getStdin gitCat
    let ssout  = getStdout gitCat

    liftIO $ IO.hSetBuffering ssin LineBuffering

    callCC \fucked -> do

      when ( PrintFixme `elem` args ) do

        for_ blobs $ \(fp,h) ->  do
          liftIO $ IO.hPrint ssin (pretty h) >> IO.hFlush ssin
          prefix <- liftIO (BS.hGetLine ssout) <&> BS.words

          case prefix of
            [_, "blob", ssize] -> do
              let mslen = readMay @Int (BS.unpack ssize)
              len <- ContT $ maybe1 mslen (pure ())
              blob <- liftIO $ LBS8.hGet ssout len
              void $ liftIO $ BS.hGetLine ssout
              fixmies <- lift $ Scan.scanBlob (Just fp) blob

              for_ fixmies $ \fixme -> do
                liftIO $ print $ pretty fixme

            _ -> fucked ()

      debug $ red "NOW WHAT?"


startGitCatFile ::  (FixmePerks m, MonadReader FixmeEnv m) => m (Process Handle Handle ())
startGitCatFile = do
  gd <- fixmeGetGitDirCLIOpt
  let cmd = [qc|git {gd} cat-file --batch|]
  debug $ pretty cmd
  let config = setStdin createPipe $ setStdout createPipe $ setStderr closed $ shell cmd
  startProcess config

extractFixmeFromGitBlob :: FixmePerks m => FilePath -> GitHash -> FixmeM m [Fixme]
extractFixmeFromGitBlob fp gh = do
  pure mempty

exractFixme :: FixmePerks m => ByteString -> m [Fixme]
exractFixme bs = do

  let ls = LBS8.lines bs

  pure mempty


readUtf8 :: ByteString -> Text
readUtf8 bs = LBS8.toStrict bs & Text.decodeUtf8


readFixmeStdin :: FixmePerks m => FixmeM m ()
readFixmeStdin = do
  what <- liftIO LBS8.getContents
  fixmies <- Scan.scanBlob Nothing what
  liftIO $ print $ vcat (fmap pretty fixmies)

printEnv :: FixmePerks m => FixmeM m ()
printEnv = do
  g <- asks fixmeEnvGitDir
  masks <- asks fixmeEnvFileMask >>= readTVarIO
  tags  <- asks fixmeEnvTags >>= readTVarIO
  days  <- asks fixmeEnvGitScanDays >>= readTVarIO
  comments1 <- asks fixmeEnvDefComments >>= readTVarIO <&> HS.toList

  comments2 <- asks fixmeEnvFileComments >>= readTVarIO
                 <&> HM.toList
                 <&> fmap  (over _2 HS.toList)

  for_ tags $ \m -> do
    liftIO $ print $ "fixme-prefix" <+> pretty m

  for_ masks $ \m -> do
    liftIO $ print $ "fixme-files" <+> dquotes (pretty m)

  for_ days $ \d -> do
    liftIO $ print $ "fixme-git-scan-filter-days" <+> pretty d

  for_ comments1 $ \d -> do
    liftIO $ print $ "fixme-comments" <+> dquotes (pretty d)

  for_ comments2 $ \(ft, comm') -> do
    for_ comm' $ \comm -> do
      liftIO $ print $ "fixme-file-comments"
                  <+> dquotes (pretty ft) <+> dquotes (pretty  comm)


help :: FixmePerks m => m ()
help = do
  notice "this is help  message"


splitForms :: [String] -> [[String]]
splitForms s0 = runIdentity $ S.toList_ (go mempty s0)
  where
    go acc ( "then" : rest ) = emit acc >> go mempty rest
    go acc ( x : rest ) = go ( x : acc ) rest
    go acc [] = emit acc

    emit = S.yield . reverse

run :: FixmePerks m => [String] -> FixmeM m ()
run what = do

  sc <- readConfig

  let s0 = fmap (parseTop . unwords) (splitForms what)
             & rights
             & mconcat


  for_ (sc <> s0) $ \s -> do

    debug $ pretty s

    case s of

      FixmeFiles xs -> do
        t <- asks fixmeEnvFileMask
        atomically (modifyTVar t (<> xs))

      FixmePrefix tag -> do
        t <- asks fixmeEnvTags
        atomically (modifyTVar t (HS.insert tag))

      FixmeGitScanFilterDays d -> do
        t <- asks fixmeEnvGitScanDays
        atomically (writeTVar t (Just d))

      ListVal [SymbolVal "fixme-file-comments", StringLike ft, StringLike b] -> do
        let co = Text.pack b & HS.singleton
        t <- asks fixmeEnvFileComments
        atomically (modifyTVar t (HM.insertWith (<>) (commentKey ft) co))

      ListVal (SymbolVal "fixme-comments" : StringLikeList xs) -> do
        t <- asks fixmeEnvDefComments
        let co = fmap Text.pack xs & HS.fromList
        atomically $ modifyTVar t (<> co)

      Init         -> init

      ScanGitLocal args -> scanGitLocal args Nothing

      ReadFixmeStdin -> readFixmeStdin

      ListVal [SymbolVal "print-env"] -> do
        printEnv

      ListVal [SymbolVal "no-debug"] -> do
        setLoggingOff @DEBUG

      ListVal [SymbolVal "debug"] -> do
        setLogging @DEBUG  $ toStderr . logPrefix "[debug] "

      w         -> err (pretty w)

