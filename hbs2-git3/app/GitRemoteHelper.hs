{-# Language RecordWildCards #-}
module Main where

import Prelude hiding (getLine)

import HBS2.Git3.Prelude
import HBS2.Git3.Run
import HBS2.Git3.Config.Local
import HBS2.Git3.State.Index
import HBS2.Git3.Import
import HBS2.Git3.Export
import HBS2.Git3.Git

import System.Posix.Signals
import System.IO qualified as IO
import System.Exit qualified as Exit
import System.Environment (getArgs)
import Text.InterpolatedString.Perl6 (qc)
import Data.Text qualified as Text
import Data.Maybe

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

parseCLI :: MonadIO m => m [Syntax C]
parseCLI = do
  argz <- liftIO getArgs
  parseTop (unlines $ unwords <$> splitForms argz)
           & either  (error.show) pure

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


data DeferredOps =
  DeferredOps
  { exportQ :: TQueue (GitRef, Maybe GitHash)
  }


localDict  :: forall m . ( HBS2GitPerks m
                      -- , HasClientAPI PeerAPI UNIX m
                      -- , HasStorage m
                      -- , HasGitRemoteKey m
                      -- , HasStateDB m
                      )
            => DeferredOps -> Dict C (Git3 m)

localDict DeferredOps{..} = makeDict @C do
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

  entry $ bindMatch "r:push" $ nil_ $ splitPushArgs $ \pushFrom pushTo -> lift do
    r0 <- for pushFrom gitRevParseThrow

    notice $ pretty $ [qc|ok {pretty pushTo}|]

    case (r0, pushTo) of
      (Nothing, ref) -> do
        export Nothing [(ref, nullHash)]

      (Just h, ref)-> do
        export (Just h) [(ref, h)]

    sendLine [qc|ok {pretty pushTo}|]

  entry $ bindMatch "r:" $ nil_ $ \syn -> lift do
    none

  where
    splitPushArgs :: forall m . MonadIO m => (Maybe GitRef -> GitRef -> m ()) -> [Syntax C] -> m ()
    splitPushArgs action = \case
      [ StringLike params ] -> do
        case Text.splitOn ":" (fromString params) of
          [ b ] -> action Nothing (fromString (Text.unpack b))
          [ a, b ] -> action (Just (fromString (Text.unpack a))) (fromString (Text.unpack b))
          _  -> throwIO (BadFormException @C nil)

      _ ->  throwIO (BadFormException @C nil)

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

  ops <- DeferredOps <$> newTQueueIO

  let dict = theDict <> localDict ops

  void $ lift $ withGit3Env env do

    conf <- readLocalConf

    cli <- parseCLI

    notice $ pretty cli

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
          sendLine ""
          next Plain
          -- liftIO exitSuccess

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

