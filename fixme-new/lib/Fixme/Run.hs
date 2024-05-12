{-# Language MultiWayIf #-}
{-# Language PatternSynonyms #-}
{-# Language ViewPatterns #-}
module Fixme.Run where

import Prelude hiding (init)
import Fixme.Prelude hiding (indent)
import Fixme.Types
import Fixme.Config
import Fixme.State
import Fixme.Scan.Git as Git
import Fixme.Scan as Scan

import HBS2.Git.Local.CLI

import HBS2.System.Dir
import DBPipe.SQLite hiding (field)
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
import Data.Word
import Text.InterpolatedString.Perl6 (qc)
import Data.Coerce
import Control.Monad.Identity
import Data.Generics.Product.Fields (field)
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

pattern Update :: forall {c}. [ScanGitArgs] -> Syntax c
pattern Update e <- ListVal (SymbolVal "update" : (scanGitArgs -> e))

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
  | ScanRunDry
  | ScanAllCommits
  deriving stock (Eq,Ord,Show,Data,Generic)

pattern ScanGitArgs :: forall {c} . ScanGitArgs -> Syntax c
pattern ScanGitArgs w <- ( scanGitArg -> Just w )

scanGitArg :: Syntax c -> Maybe ScanGitArgs
scanGitArg = \case
  SymbolVal "print-blobs" -> Just PrintBlobs
  SymbolVal "print-fixme" -> Just PrintFixme
  SymbolVal "dry"         -> Just ScanRunDry
  SymbolVal "all-commits" -> Just ScanAllCommits
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


runFixmeCLI :: FixmePerks m => FixmeM m a -> m a
runFixmeCLI m = do
  db <- newDBPipeEnv dbPipeOptsDef =<< localDBPath
  env <- FixmeEnv Nothing db
            <$>  newTVarIO mempty
            <*>  newTVarIO mempty
            <*>  newTVarIO mempty
            <*>  newTVarIO mempty
            <*>  newTVarIO mempty
            <*>  newTVarIO defCommentMap
            <*>  newTVarIO Nothing

  runReaderT ( setupLogger >> fromFixmeM (evolve >> m) ) env
                 `finally` flushLoggers
  where
    setupLogger = do
      setLogging @ERROR  $ toStderr . logPrefix "[error] "
      setLogging @WARN   $ toStderr . logPrefix "[warn] "
      setLogging @NOTICE $ toStdout . logPrefix ""
      pure ()

    flushLoggers = do
      silence


silence :: FixmePerks m => m ()
silence = do
  setLoggingOff @DEBUG
  setLoggingOff @ERROR
  setLoggingOff @WARN
  setLoggingOff @NOTICE



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
      vcat [ pretty ("." </> localDBName)
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

scanGitLocal :: FixmePerks m
             => [ScanGitArgs]
             -> Maybe FilePath
             -> FixmeM m ()
scanGitLocal args p =  do

  env <- ask

  dbpath <- localDBPath

  flip runContT pure do

    (dbFn, _) <- ContT $ withSystemTempFile "fixme-db" . curry

    tempDb <- newDBPipeEnv dbPipeOptsDef dbFn

    withDB tempDb do
      ddl [qc| create table co
               ( cohash text not null
               , ts   int null
               , primary key (cohash)
               )
             |]

      ddl [qc| create table coattr
               ( cohash text not null
               , name   text not null
               , value  text not null
               , primary key (cohash,name)
               )
             |]

      ddl [qc| create table blob
               ( hash   text not null
               , cohash text not null
               , path   text not null
               , primary key (hash,cohash,path)
               )
             |]

      -- update_ [qc|ATTACH DATABASE '{dbpath}' as fixme|]

    let onlyNewCommits xs
            | ScanAllCommits `elem` args = pure xs
            | otherwise = lift $ filterM (newCommit . view _1) xs

    co <- lift listCommits >>= onlyNewCommits

    lift do
      withDB tempDb $ transactional do
        for_ co $ \(commit, attr) -> do

          debug $ "commit" <+> pretty commit

          blobs <- listBlobs commit >>= withFixmeEnv env . filterBlobs

          let ts = HM.lookup "commit-time" attr
                     >>= readMay @Word64 . Text.unpack . coerce

          insert [qc|
              insert into co (cohash,ts) values (?,?) on conflict (cohash) do nothing
              |] (commit,ts)

          for_ (HM.toList attr) $ \(a,b) -> do
            insert [qc|
                insert into coattr(cohash,name,value) values(?,?,?)
                on conflict (cohash,name) do nothing
                |] (commit,a,b)

          for_ blobs $ \(fp,h) -> do
            insert [qc| insert into blob (hash,cohash,path)
                        values (?,?,?)
                        on conflict (hash,cohash,path) do nothing
                      |] (h,commit,fp)


    blobs <- withDB tempDb  do
                select_ @_ @(GitHash, FilePath) [qc|select distinct hash, path from blob order by path|]

    when ( PrintBlobs `elem` args ) do
        for_ blobs $ \(h,fp) -> do
          notice $ pretty h <+> pretty fp

    callCC \fucked -> do

      gitCat <- ContT $ bracket startGitCatFile (hClose . getStdin)

      let ssin   = getStdin gitCat
      let ssout  = getStdout gitCat

      liftIO $ IO.hSetBuffering ssin LineBuffering

      for_ blobs $ \(h,fp) ->  do
        liftIO $ IO.hPrint ssin (pretty h) >> IO.hFlush ssin
        prefix <- liftIO (BS.hGetLine ssout) <&> BS.words

        case prefix of
          [_, "blob", ssize] -> do
            let mslen = readMay @Int (BS.unpack ssize)
            len <- ContT $ maybe1 mslen (pure ())
            blob <- liftIO $ LBS8.hGet ssout len
            void $ liftIO $ BS.hGetLine ssout

            poor <- lift (Scan.scanBlob (Just fp) blob)

            rich <- withDB tempDb do
                      let q = [qc|

            WITH CommitAttributes AS (
              SELECT co.cohash, co.ts, coattr.name, coattr.value
              FROM co
              JOIN coattr ON co.cohash = coattr.cohash
            ),
            MinCommitTimes AS (
              SELECT blob.hash, MIN(co.ts) as mintime
              FROM blob
              JOIN co ON blob.cohash = co.cohash
              WHERE co.ts IS NOT NULL
              GROUP BY blob.hash
            ),
            RelevantCommits AS (
              SELECT blob.hash, blob.cohash, blob.path
              FROM blob
              JOIN MinCommitTimes ON blob.hash = MinCommitTimes.hash
              JOIN co ON blob.cohash = co.cohash AND co.ts = MinCommitTimes.mintime
            )
            SELECT CommitAttributes.name, CommitAttributes.value
            FROM RelevantCommits
            JOIN CommitAttributes ON RelevantCommits.cohash = CommitAttributes.cohash
            WHERE RelevantCommits.hash = ?
                      |]

                      what <- select @(FixmeAttrName,FixmeAttrVal) q (Only h)
                                <&> HM.fromList
                                <&> (<> HM.fromList [ ("blob",fromString $ show (pretty h))
                                                    , ("file",fromString fp)
                                                    ])

                      for poor $ \f -> do
                        let ts = HM.lookup "commit-time" what
                                  <&> Text.unpack . coerce
                                  >>= readMay
                                  <&> FixmeTimestamp

                        pure $ set (field @"fixmeTs") ts $ over (field @"fixmeAttr") (<> what) f

            let fixmies = rich

            when ( PrintFixme `elem` args ) do
              for_ fixmies $ \fixme -> do
                notice $ pretty fixme

            when ( ScanRunDry `elem` args ) $ fucked ()

            debug $ "actually-import-fixmies" <+> pretty h

            liftIO $ withFixmeEnv env $ withState $ transactional do
              for_ fixmies insertFixme

          _ -> fucked ()


      liftIO $ withFixmeEnv env $ withState $ transactional do
        for_ co $ \w -> do
          insertCommit (view _1 w)


startGitCatFile ::  (FixmePerks m, MonadReader FixmeEnv m) => m (Process Handle Handle ())
startGitCatFile = do
  gd <- fixmeGetGitDirCLIOpt
  let cmd = [qc|git {gd} cat-file --batch|]
  debug $ pretty cmd
  let config = setStdin createPipe $ setStdout createPipe $ setStderr closed $ shell cmd
  startProcess config


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

  attr <- asks fixmeEnvAttribs >>= readTVarIO <&> HS.toList
  vals <- asks fixmeEnvAttribValues >>= readTVarIO <&> HM.toList

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

  for_ attr $ \a -> do
      liftIO $ print $ "fixme-attribs"
                  <+> pretty a

  for_ vals$ \(v, vs) -> do
      liftIO $ print $ "fixme-value-set" <+> pretty v <+> hsep (fmap pretty (HS.toList vs))

help :: FixmePerks m => m ()
help = do
  notice "this is help  message"


splitForms :: [String] -> [[String]]
splitForms s0 = runIdentity $ S.toList_ (go mempty s0)
  where
    go acc ( "then" : rest ) = emit acc >> go mempty rest
    go acc ( "and" : rest ) = emit acc >> go mempty rest
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

      ListVal (SymbolVal "fixme-attribs" : StringLikeList xs) -> do
        ta <- asks fixmeEnvAttribs
        atomically $ modifyTVar ta (<> HS.fromList (fmap fromString xs))

      ListVal (SymbolVal "fixme-value-set" : StringLike n : StringLikeList xs) -> do
        t <- asks fixmeEnvAttribValues
        let name = fromString n
        let vals = fmap fromString xs & HS.fromList
        atomically $ modifyTVar t (HM.insertWith (<>) name vals)

      Init         -> init

      ScanGitLocal args -> scanGitLocal args Nothing

      Update args -> scanGitLocal args Nothing

      ReadFixmeStdin -> readFixmeStdin

      ListVal [SymbolVal "print-env"] -> do
        printEnv

      ListVal [SymbolVal "no-debug"] -> do
        setLoggingOff @DEBUG

      ListVal [SymbolVal "silence"] -> do
        silence

      ListVal [SymbolVal "builtin:evolve"] -> do
        evolve

      ListVal [SymbolVal "trace"] -> do
        setLogging @TRACE (logPrefix "[trace] " . toStderr)
        trace "trace on"

      ListVal [SymbolVal "no-trace"] -> do
        trace "trace off"
        setLoggingOff @TRACE

      ListVal [SymbolVal "debug"] -> do
        setLogging @DEBUG  $ toStderr . logPrefix "[debug] "

      w         -> err (pretty w)


