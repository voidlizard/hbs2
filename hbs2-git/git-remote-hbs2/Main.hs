module Main where

import Prelude hiding (getLine)

import HBS2.Git.Client.Prelude
import HBS2.Git.Client.App
import HBS2.Git.Client.Import
import HBS2.Git.Client.Export
import HBS2.Git.Client.State
import HBS2.Git.Client.Progress
import HBS2.Git.Client.Config
import HBS2.Git.Data.RefLog
import HBS2.Git.Data.Tx.Git qualified as TX
import HBS2.Git.Data.Tx.Git (RepoHead(..))
import HBS2.Git.Data.LWWBlock

import HBS2.System.Dir

import Control.Concurrent.STM qualified as STM
import System.Posix.Signals
import System.Environment
import System.IO (hPutStrLn)
import System.IO qualified as IO
import System.Exit qualified as Exit

import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Attoparsec.ByteString.Char8 hiding (try)
import Data.Attoparsec.ByteString.Char8 qualified as Atto
import Data.Maybe
import Data.HashMap.Strict qualified as HM
import Data.List qualified as L
import Text.InterpolatedString.Perl6 (qc)
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

parseURL :: String -> Maybe (LWWRefKey HBS2Basic)
parseURL s = eitherToMaybe $ Atto.parseOnly p (BS8.pack s)
  where
    p = do
      void $ string "hbs21://" <|> string "hbs2://"

      Atto.takeWhile1 (`elem` getAlphabet)
       <&> BS8.unpack
       <&> fromStringMay @(LWWRefKey HBS2Basic)
       >>= maybe (fail "invalid reflog key") pure

parsePush :: String -> Maybe (Maybe GitRef, GitRef)
parsePush s = eitherToMaybe $ Atto.parseOnly p (BS8.pack s)
  where
    gitref = fromString @GitRef . BS8.unpack
    p = do
      a <- optional (Atto.takeWhile1 (/= ':')) <&> fmap gitref
      char ':'
      b <- Atto.takeWhile1 (const True) <&> gitref
      pure (a,b)

data S =
    Plain
  | Push
  deriving stock (Eq,Ord,Show,Enum)


{- HLINT ignore "Functor law" -}
main :: IO ()
main = do
  hSetBuffering stdin  LineBuffering
  hSetBuffering stdout LineBuffering

  void $ installHandler sigPIPE Ignore Nothing

  args <- getArgs

  (remote, puk) <- case args of
                     [s, u] ->
                        (s,) <$> pure (parseURL u)
                                  `orDie` show ("invalid reflog" <+> pretty u)

                     _ -> die "bad args"

  runGitCLI mempty $ do

    env <- ask

    flip runContT pure do

      lift $ withGitEnv (env & set gitApplyHeads False) do

        debug $ red "run" <+> pretty args

        sto <- asks _storage
        ip <- asks _progress

        importRepoWait puk
          `catch` (\(_ :: ImportRefLogNotFound) -> do
                      onProgress ip ImportAllDone
                      let url = headMay (catMaybes [ parseURL a | a <- args]) <&> AsBase58
                      pause @'Seconds 0.25
                      liftIO $ hFlush stderr
                      liftIO $ hPutDoc stderr $ ""
                                  <> ul (yellow "Reference" <+> pretty url <+>  yellow "is not available yet.") <> line
                                  <> "If you sure it's a new one -- make sure you've added the key to hbs2-keyman and then run"
                                  <> line <> line
                                  <> "hbs2-keyman update" <> line <> line
                                  <> "git" <+> pretty hbs2Name <+> "export --new" <+> pretty url <> line <> line
                                  <> "to init the reflog first." <> line
                                  <> "Pushing to an existing reflog as a new one may cause unwanted data duplication." <> line
                                  <> line
                                  <> "Note: what ever pushed -- can not be unpushed" <> line
                                  <> "If it's not a new reflog --- just wait until it became available"
                      liftIO exitFailure
                  )
          `catch` ( \(ImportTxApplyError h) -> do
                      onProgress ip ImportAllDone
                      pause @'Seconds 0.25
                      liftIO $ hFlush stderr
                      liftIO $ hPutDoc stderr $ red "Can not apply tx" <+>  pretty h <> line <> line
                                      <> "It means you don't have a key do decrypt this tx or the data is not completely downloaded yet"
                                      <> line

                      liftIO exitFailure
                  )

        void $ runExceptT do

            tpush <- newTQueueIO -- @(GitRef, Maybe GitHash)

            flip fix Plain $ \next s -> do

              eof <- done

              when eof $ pure ()

              cmd <- ExceptT (try @_ @IOError (getLine <&> words))

              debug $ "C:" <+> pretty cmd

              case cmd of

                [] | s == Plain -> do
                  onProgress ip (ImportSetQuiet True)
                  pure ()

                [] | s == Push -> do
                       refs <- atomically (STM.flushTQueue tpush)
                                 <&> HM.toList . HM.fromList

                       importRepoWait puk
                       export puk refs
                       sendLine ""
                       next Plain

                ["capabilities"] -> do
                  debug $ "send capabilities"
                  sendLine "push"
                  sendLine "fetch"
                  sendLine ""
                  next Plain

                ("list" : _) -> do


                    r'  <- runMaybeT $ withState do
                            tx <- selectMaxAppliedTx >>= lift  . toMPlus <&> fst

                            rh <- TX.readRepoHeadFromTx sto tx >>= lift . toMPlus
                            pure (_repoHeadRefs rh)

                    let r = fromMaybe mempty r'

                    forM_ (fmap (show . formatRef) r) sendLine

                    sendLine ""

                    next Plain

                ("push" : pargs : _ ) -> do
                  (fromRef, toRef) <- orThrowUser "can't parse push" (parsePush pargs)

                  r <- readProcess (setStderr closed $ shell [qc|git rev-parse {pretty $ fromRef}|])
                        <&> headDef "" . LBS8.words . view _2
                        <&> fromStringMay @GitHash . LBS8.unpack

                  let val = const r =<< fromRef

                  atomically $ writeTQueue tpush (toRef, val)

                  sendLine [qc|ok {pretty toRef}|]
                  next Push

                _ -> next Plain

              pure ()

  `finally` liftIO do
    hPutStrLn stdout "" >> hFlush stdout
    -- notice $ red "BYE"
    hPutStrLn stderr ""



