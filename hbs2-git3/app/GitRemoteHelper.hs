{-# Language RecordWildCards #-}
module Main where

import Prelude hiding (getLine)

import HBS2.Git3.Prelude
import HBS2.Git3.Run
import HBS2.Git3.Config.Local
import HBS2.Git3.State
import HBS2.Git3.Import
import HBS2.Git3.Export
import HBS2.Git3.Git
import HBS2.Git3.Repo
import HBS2.Git3.Logger

import Data.Config.Suckless

import System.Posix.Signals
import System.IO qualified as IO
import System.Posix.IO
import System.Exit qualified as Exit
import System.Environment (getArgs,lookupEnv)
import Text.InterpolatedString.Perl6 (qc)
import Data.Text qualified as Text
import Data.Either
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

parseCLI :: MonadIO m => m [Syntax C]
parseCLI = do
  argz <- liftIO getArgs
  parseTop (unlines $ unwords <$> splitForms argz)
           & either  (error.show) pure

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
                      )
            => DeferredOps -> Dict C (Git3 m)

localDict DeferredOps{..} = makeDict @C do
  entry $ bindMatch "r:capabilities" $ nil_ $ \syn -> do
    sendLine "push"
    sendLine "fetch"
    sendLine ""

  entry $ bindMatch "r:list" $ nil_ $ const $ lift $ connectedDo do
      reflog <- getGitRemoteKey >>= orThrow GitRepoManifestMalformed

      debug $ red "REFLOG" <+> pretty (AsBase58 reflog)

      t0 <- getTimeCoarse

      waitRepo Nothing =<< getGitRepoKeyThrow
      importGitRefLog

      rrefs <- importedRefs

      for_ rrefs $ \(r,h) -> do
        notice $ yellow "REF" <+> pretty h <+> pretty r
        sendLine $ show $ pretty h <+> pretty r

      let l = lastMay rrefs

      for_ l $ \(r,h) -> do
        debug $ pretty h <+> pretty "HEAD"
        sendLine $ show $ pretty h <+> pretty "HEAD"

      sendLine ""

  entry $ bindMatch "r:fetch" $ nil_ $ \syn -> do
    debug $ "FETCH" <+> pretty syn
    sendLine ""

  entry $ bindMatch "r:push" $ nil_ $ splitPushArgs $ \pushFrom pushTo -> lift do
    r0 <- for pushFrom gitRevParseThrow

    debug $ pretty $ [qc|ok {pretty pushTo}|]

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

  -- origStderr <- liftIO $ dup stdError
  -- (readEnd, writeEnd) <- liftIO createPipe
  -- liftIO $ dupTo writeEnd stdError
  -- liftIO $ closeFd writeEnd

  -- rStderr <- liftIO $ fdToHandle readEnd
  -- origHandle <- liftIO $ fdToHandle origStderr

  -- liftIO $ hSetBuffering origHandle NoBuffering

  -- liftIO $ IO.hPutStr origHandle "\n"
  -- ContT $ withAsync $ liftIO $ forever do
    -- pause @'Seconds 0.25
    -- wut <- IO.hGetContents rStderr <&> lines
    -- for_ wut $ \s -> do
    --   IO.hPutStrLn rStderr s
      -- IO.hPutStr origHandle (replicate 100 ' ')
      -- IO.hPutStr origHandle "\r"
      -- IO.hPutStr origHandle s
      -- IO.hPutStr origHandle "\r"
      -- pause @'Seconds 0.05

  -- ContT $ bracket none $ const do
  --   IO.hPutStr origHandle (replicate 100 ' ')
  --   IO.hPutStr origHandle "\r"
  --   silence

  lift $ void $ installHandler sigPIPE Ignore Nothing
  env <- nullGit3Env

  ops <- DeferredOps <$> newTQueueIO

  let dict = theDict <> localDict ops

  git <- liftIO $ lookupEnv "GIT_DIR"
  debug $ red "GIT" <+> pretty git

  void $ lift $ withGit3Env env do

    -- d_ <- asks gitRuntimeDict
    -- atomically $ writeTVar d_ (Just (RuntimeDict fuck))
    --

    conf <- readLocalConf

    cli <- parseCLI

    url <- case cli of
      [ ListVal [_, RepoURL x ] ] -> do
        notice $ "git remote ref set:" <+> green (pretty (AsBase58 x))
        setGitRepoKey x
        pure $ Just x

      _ -> pure Nothing

    recover $ connectedDo $ withStateDo do

      waitRepo Nothing =<< getGitRepoKeyThrow

      notice "WAIT-FOR-REPO-DONE"

      void $ run dict conf

      for_ url updateRepoKey

      flip fix Plain $ \next -> \case
        Plain -> do

          inp <- try @_ @IOError getLine <&> fromRight mempty

          when (null (words inp)) $ next End

          debug $ pretty "INPUT" <+> pretty inp

          r <- try @_ @SomeException (runTop dict ("r:"<>inp))
                 >>= \case
                   Left e -> die (show e)
                   _ -> none

          next Plain

        End -> do
          sendLine ""
          liftIO exitSuccess

        _ -> do
          sendLine ""
          next Plain

          -- liftIO exitSuccess
