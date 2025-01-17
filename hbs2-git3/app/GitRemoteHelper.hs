module Main where

import Prelude hiding (getLine)

import HBS2.Git3.Prelude
import HBS2.Git3.Run
import HBS2.Git3.Config.Local
import HBS2.Git3.State.Index
import HBS2.Git3.Import

import System.Posix.Signals
import System.IO qualified as IO
import System.Exit qualified as Exit

import Data.Config.Suckless.Script

import System.Exit hiding (die)

{- HLINT ignore "Use isEOF" -}
{- HLINT ignore "Use putStrLn" -}

done :: MonadIO m => m Bool
done = hIsEOF stdin

getLine :: MonadIO m => m String
getLine = liftIO IO.getLine

sendLine :: MonadIO m => String -> m ()
sendLine = liftIO . IO.putStrLn

die :: (MonadIO m, Pretty a) => a -> m b
die s = liftIO $ Exit.die (show $ pretty s)

-- parseURL :: String -> Maybe (LWWRefKey 'HBS2Basic)
-- parseURL s = eitherToMaybe $ Atto.parseOnly p (BS8.pack s)
--   where
--     p = do
--       void $ string "hbs21://" <|> string "hbs2://"

--       Atto.takeWhile1 (`elem` getAlphabet)
--        <&> BS8.unpack
--        <&> fromStringMay @(LWWRefKey 'HBS2Basic)
--        >>= maybe (fail "invalid reflog key") pure

-- parsePush :: String -> Maybe (Maybe GitRef, GitRef)
-- parsePush s = eitherToMaybe $ Atto.parseOnly p (BS8.pack s)
--   where
--     gitref = fromString @GitRef . BS8.unpack
--     p = do
--       a <- optional (Atto.takeWhile1 (/= ':')) <&> fmap gitref
--       char ':'
--       b <- Atto.takeWhile1 (const True) <&> gitref
--       pure (a,b)

data S =
    Plain
  | Push
  | End
  deriving stock (Eq,Ord,Show,Enum)



localDict  :: forall m . ( HBS2GitPerks m
                      -- , HasClientAPI PeerAPI UNIX m
                      -- , HasStorage m
                      -- , HasGitRemoteKey m
                      -- , HasStateDB m
                      ) => Dict C (Git3 m)
localDict = makeDict @C do
  entry $ bindMatch "r:capabilities" $ nil_ $ \syn -> do
    sendLine "push"
    sendLine "fetch"
    sendLine ""

  entry $ bindMatch "r:list" $ nil_ $ \syn -> lift do
    importGitRefLog

    rrefs <- importedRefs

    for_ rrefs $ \(r,h) -> do
      debug $ pretty h <+> pretty r
      sendLine $ show $ pretty h <+> pretty r

    sendLine ""

  entry $ bindMatch "r:" $ nil_ $ \syn -> lift do
    none

runTop :: (ParseSExp what, MonadUnliftIO m) => Dict C m -> what -> m ()
runTop dict s = parseTop s & either (const none) (void . run dict)

{- HLINT ignore "Functor law" -}
main :: IO ()
main =  flip runContT pure do
  hSetBuffering stdin  LineBuffering
  hSetBuffering stdout LineBuffering

  setupLogger

  ContT $ bracket none $ const do
    silence

  lift $ void $ installHandler sigPIPE Ignore Nothing
  env <- nullGit3Env

  let dict = theDict <> localDict

  void $ lift $ withGit3Env env do

    conf <- readLocalConf

    void $ run dict conf

    recover $ connectedDo do

      flip fix Plain $ \next -> \case
        Plain -> do

          eof <- done

          when eof $ next End

          inp <- getLine

          when (null (words inp)) $ next End

          debug $ pretty "INPUT" <+> pretty inp

          runTop dict ("r:"<>inp)

          next Plain

        _ -> do
          liftIO exitSuccess

-- debugPrefix :: LoggerEntry -> LoggerEntry
debugPrefix = toStderr . logPrefix "[debug] "

setupLogger :: MonadIO m => m ()
setupLogger = do
  -- setLogging @DEBUG  $ toStderr . logPrefix "[debug] "
  setLogging @ERROR  $ toStderr . logPrefix "[error] "
  setLogging @WARN   $ toStderr . logPrefix "[warn] "
  setLogging @NOTICE $ toStderr . logPrefix ""
  pure ()

flushLoggers :: MonadIO m => m ()
flushLoggers = do
  silence

silence :: MonadIO m => m ()
silence = do
  setLoggingOff @DEBUG
  setLoggingOff @ERROR
  setLoggingOff @WARN
  setLoggingOff @NOTICE


  -- runGitCLI mempty $ do
  --   env <- ask

    -- flip runContT pure do

    --   lift $ withGitEnv (env & set gitApplyHeads False) do

    --     debug $ red "run" <+> pretty args

    --     sto <- asks _storage
    --     ip <- asks _progress

    --     importRepoWait puk
    --       `catch` (\(_ :: ImportRefLogNotFound) -> do
    --                   onProgress ip ImportAllDone
    --                   let url = headMay (catMaybes [ parseURL a | a <- args]) <&> AsBase58
    --                   pause @'Seconds 0.25
    --                   liftIO $ hFlush stderr
    --                   liftIO $ hPutDoc stderr $ ""
    --                               <> ul (yellow "Reference" <+> pretty url <+>  yellow "is not available yet.") <> line
    --                               <> "If you sure it's a new one -- make sure you've added the key to hbs2-keyman and then run"
    --                               <> line <> line
    --                               <> "hbs2-keyman update" <> line <> line
    --                               <> "git" <+> pretty hbs2Name <+> "export --new" <+> pretty url <> line <> line
    --                               <> "to init the reflog first." <> line
    --                               <> "Pushing to an existing reflog as a new one may cause unwanted data duplication." <> line
    --                               <> line
    --                               <> "Note: what ever pushed -- can not be unpushed" <> line
    --                               <> "If it's not a new reflog --- just wait until it became available"
    --                   liftIO exitFailure
    --               )
    --       `catch` ( \(ImportTxApplyError h) -> do
    --                   onProgress ip ImportAllDone
    --                   pause @'Seconds 0.25
    --                   liftIO $ hFlush stderr
    --                   liftIO $ hPutDoc stderr $ red "Can not apply tx" <+>  pretty h <> line <> line
    --                                   <> "It means you don't have a key do decrypt this tx or the data is not completely downloaded yet"
    --                                   <> line

    --                   liftIO exitFailure
    --               )

    --     void $ runExceptT do

    --         tpush <- newTQueueIO -- @(GitRef, Maybe GitHash)

    --         flip fix Plain $ \next s -> do

    --           eof <- done

    --           when eof $ pure ()

    --           cmd <- ExceptT (try @_ @IOError (getLine <&> words))

    --           debug $ "C:" <+> pretty cmd

    --           case cmd of

    --             [] | s == Plain -> do
    --               onProgress ip (ImportSetQuiet True)
    --               pure ()

    --             [] | s == Push -> do
    --                    refs <- atomically (STM.flushTQueue tpush)
    --                              <&> HM.toList . HM.fromList

    --                    importRepoWait puk
    --                    export puk refs
    --                    sendLine ""
    --                    next Plain

    --             ["capabilities"] -> do
    --               debug $ "send capabilities"
    --               sendLine "push"
    --               sendLine "fetch"
    --               sendLine ""
    --               next Plain

    --             ("list" : _) -> do


    --                 -- FIXME: may-cause-reference-inconsistency
    --                 --   надо брать max(head) для lwwref
    --                 --   а не максимальную транзу, накаченную на репо
    --                 r'  <- runMaybeT do
    --                         -- tx <- selectMaxAppliedTx >>= lift  . toMPlus <&> fst

    --                         -- (_,rh) <- TX.readRepoHeadFromTx sto tx >>= lift . toMPlus
    --                         rh <- liftIO (withGitEnv env (readActualRepoHeadFor puk))
    --                                 >>= toMPlus

    --                         pure (view repoHeadRefs rh)

    --                 let r = fromMaybe mempty r'

    --                 forM_ (fmap (show . formatRef) r) sendLine

    --                 sendLine ""

    --                 next Plain

    --             ("push" : pargs : _ ) -> do
    --               (fromRef, toRef) <- orThrowUser "can't parse push" (parsePush pargs)

    --               r <- readProcess (setStderr closed $ shell [qc|git rev-parse {pretty $ fromRef}|])
    --                     <&> headDef "" . LBS8.words . view _2
    --                     <&> fromStringMay @GitHash . LBS8.unpack

    --               let val = const r =<< fromRef

    --               atomically $ writeTQueue tpush (toRef, val)

    --               sendLine [qc|ok {pretty toRef}|]
    --               next Push

    --             _ -> next Plain

    --           pure ()

  -- `finally` liftIO do
    -- hPutStrLn stdout "" >> hFlush stdout
    -- -- notice $ red "BYE"
    -- hPutStrLn stderr ""


