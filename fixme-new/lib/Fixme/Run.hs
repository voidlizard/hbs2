{-# Language PatternSynonyms #-}
{-# Language ViewPatterns #-}
module Fixme.Run where

import Prelude hiding (init)
import Fixme.Prelude hiding (indent)
import Fixme.Types
import Fixme.Scan.Git as Git

import HBS2.Git.Local.CLI

import HBS2.System.Dir

import Data.Config.Suckless
import Data.Text.Fuzzy.Tokenize

import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.ByteString.Lazy (ByteString)
import Data.Either
import System.Environment
import Data.Maybe
import Data.HashSet qualified as HS
import Data.HashMap.Strict qualified as HM
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Text.InterpolatedString.Perl6 (qc)
import Data.Coerce
import Control.Monad.Identity
import Lens.Micro.Platform

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
  deriving stock (Eq,Ord,Show,Data,Generic)

scanGitArgs :: [Syntax c] -> [ScanGitArgs]
scanGitArgs syn = [ PrintBlobs | SymbolVal "print-blobs" <- syn ]

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
     >>= liftIO . readFile
     <&> parseTop
     <&> fromRight mempty

init :: FixmePerks m => FixmeM m ()
init = do
  lo <- localConfigDir

  let lo0 = takeFileName lo

  touch (lo </> "config")
  mkdir lo

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

listCommits :: FixmePerks m => FixmeM m [GitHash]
listCommits = do
  let gd = ""

  days <- asks fixmeEnvGitScanDays
            >>= readTVarIO
            <&> fmap ( \x -> "--since" <+> squotes (pretty x <+> "days ago"))
            <&> fromMaybe mempty
            <&> show

  let cmd = [qc|git log --all --format="%H" {days}|]

  -- FIXME: git-dir
  gitRunCommand cmd
    <&> fromRight mempty
    <&> mapMaybe (headMay . LBS8.words) . LBS8.lines
    <&> mapMaybe (fromStringMay @GitHash . LBS8.unpack)


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
  debug $ yellow "scan for fixmies, wtf?"
  co <- listCommits

  blobs <- mconcat <$> for co (\c -> do
                          debug $ "commit" <+> pretty c
                          listBlobs c >>= filterBlobs )

  when (PrintBlobs `elem` args) do
    for_ blobs $ \(fp,h) -> do
      liftIO $ print $ pretty h <+> pretty fp


exractFixme :: FixmePerks m => ByteString -> m [Fixme]
exractFixme bs = do

  let ls = LBS8.lines bs

  pure mempty


readUtf8 :: ByteString -> Text
readUtf8 bs = LBS8.toStrict bs & Text.decodeUtf8


readFixmeStdin :: FixmePerks m => FixmeM m ()
readFixmeStdin = do
  what <- liftIO LBS8.getContents <&> LBS8.lines
  pure ()

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


