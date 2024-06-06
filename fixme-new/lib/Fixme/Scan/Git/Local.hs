{-# Language MultiWayIf #-}
{-# Language PatternSynonyms #-}
{-# Language ViewPatterns #-}
module Fixme.Scan.Git.Local where


import Prelude hiding (init)
import Fixme.Prelude hiding (indent)
import Fixme.Types
import Fixme.State
import Fixme.Scan as Scan
import Fixme.Log

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
import Data.Maybe
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
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
import System.IO qualified as IO
import System.IO.Temp (emptySystemTempFile)
import System.TimeIt

import Data.Map qualified as Map

import Streaming.Prelude qualified as S

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


listRefs :: FixmePerks m => FixmeM m [(GitHash, GitRef)]
listRefs = do
  gd <- fixmeGetGitDirCLIOpt
  gitRunCommand [qc|git {gd} show-ref --dereference|]
    <&> fromRight mempty
    <&> fmap LBS8.words . LBS8.lines
    <&> mapMaybe
          (\case
             [h,b] -> (,) <$> fromStringMay @GitHash (LBS8.unpack h) <*> pure (GitRef (LBS8.toStrict b))
             _     -> Nothing
          )

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


scanGitLogLocal :: FixmePerks m
                => FilePath
                -> ( [Syntax C] -> FixmeM m () )
                -> FixmeM m ()
scanGitLogLocal refMask play = do
  warn $ red "scanGitLogLocal" <+> pretty refMask

  (t,refs) <- timeItT listRefs

  let hashes = fmap fst refs

  warn $ yellow "listRefs in" <+> pretty (realToFrac t :: Fixed E6)

  let pat = [(True, refMask)]

  -- FIXME: use-cache-to-skip-already-processed-tips
  logz <- S.toList_ $ for_ hashes $ \h -> do
              done <- lift $ withState (isProcessed (ViaSerialise h))
              unless done do
                blobs <- lift (listBlobs h >>= filterBlobs0 pat)
                for_ blobs $ \(_,b) -> do
                  S.yield (h,b)

  warn $ yellow "STEP 3" <+> "for each tree   --- find log"

  warn $ vcat (fmap pretty logz)

  warn $ yellow "STEP 4" <+> "for each log    --- scan log"

  withState $ transactional do

    flip runContT pure do
      for_ logz $ \(commitHash, h) -> callCC \shit -> do
          warn $ blue "SCAN BLOB" <+> pretty h
          tmp <- ContT $ bracket (liftIO (emptySystemTempFile "fixme-log")) rm
          blob <- lift $ lift $ gitCatBlob h
          liftIO (LBS8.writeFile tmp blob)

          esto <- lift $ try @_ @CompactStorageOpenError $ compactStorageOpen @HbSync readonly tmp

          -- skip even problematic commit
          lift $ insertProcessed (ViaSerialise commitHash)

          either (const $ warn $ "skip malformed/unknown log" <+> pretty h) (const none) esto
          sto <- either (const $ shit ()) pure esto

          lift $ lift $ loadAllEntriesFromLog sto >>= play

          compactStorageClose sto

scanGitLocal :: FixmePerks m
             => [ScanGitArgs]
             -> Maybe FilePath
             -> FixmeM m ()
scanGitLocal args p =  do

  env <- ask

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

      for_ blobs $ \(h,fp) ->  callCC \next -> do

        seen <-  lift (withState $ selectObjectHash h) <&> isJust

        when seen do
          trace $ red "ALREADY SEEN BLOB" <+> pretty h
          next ()

        liftIO $ IO.hPrint ssin (pretty h) >> IO.hFlush ssin
        prefix <- liftIO (BS.hGetLine ssout) <&> BS.words

        case prefix of
          [bh, "blob", ssize] -> do
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
                        let lno = maybe mempty ( HM.singleton "line"
                                               . FixmeAttrVal
                                               . Text.pack
                                               . show
                                               )
                                       (fixmeStart f)

                        let ts = HM.lookup "commit-time" what
                                  <&> Text.unpack . coerce
                                  >>= readMay
                                  <&> FixmeTimestamp

                        pure $ set (field @"fixmeTs") ts $ over (field @"fixmeAttr") (<> (what <> lno)) f


            let fxpos1 = [ (fixmeTitle fx, [i :: Int])
                         | (i,fx) <- zip [0..] rich
                         -- , fixmeTitle fx /= mempty
                         ] & Map.fromListWith (flip (<>))

            let mt e = do
                  let seed = [ (fst e, i) | i <- snd e ]
                  flip fix (0,[],seed) $ \next (num,acc,rest) ->
                    case rest of
                      [] -> acc
                      (x:xs) -> next (succ num, (x,num) : acc, xs)

            let fxpos2 = [ mt e
                         | e <- Map.toList fxpos1
                         ] & mconcat
                           & Map.fromList

            fixmies <- for (zip [0..] rich) $ \(i,fx) -> do
                         let title = fixmeTitle fx
                         let kb = Map.lookup (title,i) fxpos2
                         let ka = HM.lookup "file" (fixmeAttr fx)
                         let kk = (,,) <$> ka <*> pure title <*> kb

                         case kk of
                          Nothing -> pure fx
                          Just (a,b,c) -> do
                            let ks = [qc|{show (pretty a)}#{show (pretty b)}:{show c}|] :: Text
                            let ksh = hashObject @HbSync (serialise ks) & pretty & show & Text.pack & FixmeAttrVal
                            let kh = HM.singleton "fixme-key" ksh
                            let kv = HM.singleton "fixme-key-string" (FixmeAttrVal ks) <> kh
                            pure $ over (field @"fixmeAttr") (<> kv) fx

            when ( PrintFixme `elem` args ) do
              for_ fixmies $ \fixme -> do
                notice $ pretty fixme

            when ( ScanRunDry `elem` args ) $ fucked ()

            debug $ "actually-import-fixmies" <+> pretty h

            liftIO $ withFixmeEnv env $ withState $ transactional do
              insertBlob h
              for_ fixmies insertFixme

          _ -> fucked ()

      unless ( ScanRunDry `elem` args ) do
        lift runLogActions

      liftIO $ withFixmeEnv env $ withState $ transactional do
        for_ co $ \w -> do
          insertCommit (view _1 w)

runLogActions :: FixmePerks m => FixmeM m ()
runLogActions = do
  debug $ yellow "runLogActions"
  actions <- asks fixmeEnvReadLogActions >>= readTVarIO

  for_ actions $ \(ReadLogAction a) -> do
    liftIO (a (List noContext []))

  updateIndexes


gitCatBlob :: (FixmePerks m, MonadReader FixmeEnv m) => GitHash -> m ByteString
gitCatBlob h = do
  gd <- fixmeGetGitDirCLIOpt
  (_,s,_)  <- readProcess $ shell [qc|git {gd} cat-file blob {pretty h}|]
  pure s

startGitCatFile ::  (FixmePerks m, MonadReader FixmeEnv m) => m (Process Handle Handle ())
startGitCatFile = do
  gd <- fixmeGetGitDirCLIOpt
  let cmd = [qc|git {gd} cat-file --batch|]
  debug $ pretty cmd
  let config = setStdin createPipe $ setStdout createPipe $ setStderr closed $ shell cmd
  startProcess config

