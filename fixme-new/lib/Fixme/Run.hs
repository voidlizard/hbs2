module Fixme.Run where

import Prelude hiding (init)
import Fixme.Prelude hiding (indent)
import Fixme.Types
import Fixme.Config
import Fixme.State
import Fixme.Run.Internal
import Fixme.Scan.Git.Local as Git
import Fixme.Scan as Scan
import Fixme.Log

import HBS2.Git.Local.CLI

import HBS2.Base58
import HBS2.Merkle
import HBS2.Data.Types.Refs
import HBS2.Storage
import HBS2.Storage.Compact
import HBS2.System.Dir
import DBPipe.SQLite hiding (field)
import Data.Config.Suckless

import Data.Aeson.Encode.Pretty as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Either
import Data.Maybe
import Data.HashSet qualified as HS
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.HashSet (HashSet)
import Data.Set qualified as Set
import Data.Generics.Product.Fields (field)
import Data.List qualified as List
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Text.InterpolatedString.Perl6 (qc)
import Data.Coerce
import Control.Monad.Identity
import Lens.Micro.Platform
import System.Process.Typed
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Maybe
import System.IO.Temp as Temp
import System.IO qualified as IO



runFixmeCLI :: FixmePerks m => FixmeM m a -> m a
runFixmeCLI m = do
  dbPath <- localDBPath
  git <- findGitDir
  env <- FixmeEnv
            <$>  newMVar ()
            <*>  newTVarIO mempty
            <*>  newTVarIO dbPath
            <*>  newTVarIO Nothing
            <*>  newTVarIO git
            <*>  newTVarIO mempty
            <*>  newTVarIO mempty
            <*>  newTVarIO mempty
            <*>  newTVarIO mempty
            <*>  newTVarIO mempty
            <*>  newTVarIO defCommentMap
            <*>  newTVarIO Nothing
            <*>  newTVarIO mempty
            <*>  newTVarIO mempty
            <*>  newTVarIO defaultCatAction
            <*>  newTVarIO defaultTemplate
            <*>  newTVarIO mempty
            <*>  newTVarIO (1,3)

  -- FIXME: defer-evolve
  --   не все действия требуют БД,
  --   хорошо бы, что бы она не создавалась,
  --   если не требуется
  runReaderT ( setupLogger >> fromFixmeM (handle @_ @SomeException (err . viaShow) evolve >> m) ) env
                 `finally` flushLoggers
  where
    setupLogger = do
      setLogging @ERROR  $ toStderr . logPrefix "[error] "
      setLogging @WARN   $ toStderr . logPrefix "[warn] "
      setLogging @NOTICE $ toStdout . logPrefix ""
      pure ()

    flushLoggers = do
      silence

    -- FIXME: tied-fucking-context
    defaultCatAction = CatAction $ \dict lbs -> do
      LBS.putStr lbs
      pure ()

silence :: FixmePerks m => m ()
silence = do
  setLoggingOff @DEBUG
  setLoggingOff @ERROR
  setLoggingOff @WARN
  setLoggingOff @NOTICE


defaultTemplate :: HashMap Id FixmeTemplate
defaultTemplate = HM.fromList [ ("default", Simple (SimpleTemplate short)) ]
  where
    short = parseTop s & fromRight mempty
    s = [qc|
(trim 10  $fixme-key) " "
(align 6  $fixme-tag) " "
(trim 50  ($fixme-title))
(nl)
    |]


readConfig :: FixmePerks m => FixmeM m [Syntax C]
readConfig = do

  user <- userConfigs
  lo   <- localConfig

  w <- for (lo : user) $ \conf -> do
    try @_ @IOException (liftIO $ readFile conf)
      <&> fromRight mempty
      <&> parseTop
      <&> fromRight mempty

  pure $ mconcat w


runTop :: FixmePerks m => [String] -> FixmeM m ()
runTop argz = do

  forms <- parseTop (unlines $ unwords <$> splitForms argz)
           & either  (error.show) pure

  -- pure ((unlines . fmap unwords . splitForms) what)
  --          >>= either (error.show) pure . parseTop

  let dict = makeDict @C do

       internalEntries

       entry $ bindMatch "--help" $ nil_ \case
         HelpEntryBound what -> helpEntry what
         [StringLike s]      -> helpList False (Just s)
         _                   -> helpList False Nothing

       entry $ bindMatch "fixme-prefix" $ nil_ \case
        [StringLike pref] -> do

          t <- lift $ asks fixmeEnvTags
          atomically (modifyTVar t (HS.insert (FixmeTag $ fromString pref)))

        _ -> throwIO $ BadFormException @C nil


       entry $ bindMatch "fixme-git-scan-filter-days" $ nil_ \case
        [LitIntVal d] -> do
          t <- lift $ asks fixmeEnvGitScanDays
          atomically (writeTVar t (Just d))

        _ -> throwIO $ BadFormException @C nil

       entry $ bindMatch "fixme-attribs" $ nil_ \case
        StringLikeList xs -> do
          ta <- lift $ asks fixmeEnvAttribs
          atomically $ modifyTVar ta (<> HS.fromList (fmap fromString xs))

        _ -> throwIO $ BadFormException @C nil


       entry $ bindMatch "fixme-files" $ nil_ \case
        StringLikeList xs -> do
          t <- lift $ asks fixmeEnvFileMask
          atomically (modifyTVar t (<> xs))

        _ -> throwIO $ BadFormException @C nil

       entry $ bindMatch "fixme-file-comments" $ nil_ $ \case
         [StringLike ft, StringLike b] -> do
            let co = Text.pack b & HS.singleton
            t <- lift $ asks fixmeEnvFileComments
            atomically (modifyTVar t (HM.insertWith (<>) (commentKey ft) co))

         _ -> throwIO $ BadFormException @C nil

       entry $ bindMatch "fixme-comments" $ nil_  \case
         (StringLikeList xs) -> do
            t <- lift $ asks fixmeEnvDefComments
            let co = fmap Text.pack xs & HS.fromList
            atomically $ modifyTVar t (<> co)

         _ -> throwIO $ BadFormException @C nil

       entry $ bindMatch  "fixme-value-set" $ nil_ \case
         (StringLike n : StringLikeList xs)  -> do
           t <- lift $ asks fixmeEnvAttribValues
           let name = fromString n
           let vals = fmap fromString xs & HS.fromList
           atomically $ modifyTVar t (HM.insertWith (<>) name vals)

         _ -> throwIO $ BadFormException @C nil

       entry $ bindMatch "fixme-pager" $ nil_ \case
        _ -> warn $ yellow "fixme-pager" <+> "instruction is not supported yet"

       entry $ bindMatch "fixme-def-context" $ nil_ \case
        [LitIntVal a, LitIntVal b] -> do
          t <- lift $ asks fixmeEnvCatContext
          atomically $ writeTVar t (fromIntegral a, fromIntegral b)

        _ -> throwIO $ BadFormException @C nil

       entry $ bindMatch "env:show" $ nil_ $ const $ do
        lift printEnv

       entry $ bindMatch "git:commits" $ const $ do
        co <- lift listCommits <&> fmap (mkStr @C . view _1)
        pure $ mkList co

       entry $ bindMatch "init" $ nil_ $ const $ do
        lift init

  conf <- readConfig

  run dict (conf <> forms) >>= eatNil display

  -- notice $ red "re-implementing fixme-new"
  -- read refchan
  -- execute settings from refchan
  -- read config


  -- execute config
  -- execute cli
  pure ()
  -- sc <- readConfig

  -- let s0 = fmap (parseTop . unwords) (splitForms what)
  --            & rights
  --            & mconcat

  -- runForms (sc <> s0)



