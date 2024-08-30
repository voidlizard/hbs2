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
import System.Environment
import System.Process.Typed
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Maybe
import System.IO.Temp as Temp
import System.IO qualified as IO


{- HLINT Ignore "Functor law" -}

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
  setLoggingOff @TRACE


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


runCLI :: FixmePerks m => FixmeM m ()
runCLI = do
  argz <- liftIO getArgs
  forms <- parseTop (unlines $ unwords <$> splitForms argz)
           & either  (error.show) pure

  runTop forms

runTop :: FixmePerks m => [Syntax C] -> FixmeM m ()
runTop forms = do

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
         [ListVal cmd0] -> do
            t <- lift $ asks fixmeEnvCatAction
            let action =  CatAction $ \dict lbs -> do

                  let ccmd = case inject dict cmd0 of
                               (StringLike p : StringLikeList xs) -> Just (p, xs)
                               _  -> Nothing


                  debug $ pretty ccmd

                  maybe1 ccmd none $ \(p, args) -> do

                    let input = byteStringInput lbs
                    let cmd = setStdin input $ setStderr closed
                                             $ proc p args
                    void $ runProcess cmd

            atomically $ writeTVar t action

       entry $ bindMatch "fixme-def-context" $ nil_ \case
        [LitIntVal a, LitIntVal b] -> do
          t <- lift $ asks fixmeEnvCatContext
          atomically $ writeTVar t (fromIntegral a, fromIntegral b)

        _ -> throwIO $ BadFormException @C nil

       entry $ bindMatch "cat" $ nil_ \case
        [SymbolVal "metadata", FixmeHashLike hash] -> do
          lift $ catFixmeMetadata hash

        [FixmeHashLike hash] -> do
          lift $ catFixme hash

        _ -> throwIO $ BadFormException @C nil

       entry $ bindMatch "report" $ nil_ \case
        [] -> lift $ list_ Nothing ()

        (SymbolVal "--template" : StringLike name : query) -> do
          lift $ list_ (Just (fromString name)) query

        query -> do
          lift $ list_ mzero query

       entry $ bindMatch "env:show" $ nil_ $ const $ do
        lift printEnv

       entry $ bindMatch "git:commits" $ const $ do
        co <- lift listCommits <&> fmap (mkStr @C . view _1)
        pure $ mkList co

       entry $ bindMatch "git:refs" $ const do
         refs <- lift $ listRefs False

         elems <- for refs $ \(h,r) -> do
          pure $ mkList @C [mkStr h, mkSym ".", mkStr r]

         pure $ mkList elems

       entry $ bindMatch "fixme:log:export" $ nil_ \case
        [StringLike fn] -> do
           lift $ exportToLog fn

        _ -> throwIO $ BadFormException @C nil

       entry $ bindMatch "fixme:log:import" $ nil_ \case
        [StringLike fn] -> do
           lift $ importFromLog fn

        _ -> throwIO $ BadFormException @C nil

       entry $ bindMatch "fixme:list:poor" $ nil_ $ const do
        fme <- lift listFixmies
        pure ()

       entry $ bindMatch "delete" $ nil_ \case
         [FixmeHashLike hash] -> lift $ delete hash

         _ -> throwIO $ BadFormException @C nil

       entry $ bindMatch "modify" $ nil_ \case
         [FixmeHashLike hash, StringLike a, StringLike b] -> do
           lift $ modify_ hash a b

         _ -> throwIO $ BadFormException @C nil

       entry $ bindMatch "fixme:stage:show" $ nil_ $ const do
          stage <- lift selectStage
          liftIO $ print $ vcat (fmap pretty stage)

       entry $ bindMatch "fixme:stage:drop" $ nil_ $ const do
          lift cleanStage

       entry $ bindMatch "fixme:stage:clean" $ nil_ $ const do
          lift cleanStage

       entry $ bindMatch "fixme:scan-git-local" $ nil_ $ const do
        lift $ scanGitLocal mempty Nothing

       entry $ bindMatch "git:blobs" $  \_ -> do
        blobs <- lift listRelevantBlobs

        elems <- for blobs $ \(f,h) -> do
                    pure $ mkList @C [ mkStr f, mkSym ".", mkStr h ]

        pure $ mkList @C elems

       entry $ bindMatch "init" $ nil_ $ const $ do
        lift init

       entry $ bindMatch "set-template" $ nil_ \case
         [SymbolVal who, SymbolVal w] -> do
           templates <- lift $ asks fixmeEnvTemplates
           t <- readTVarIO templates
           for_ (HM.lookup w t) $ \tpl -> do
             atomically $ modifyTVar templates (HM.insert who tpl)

         _ -> throwIO $ BadFormException @C nil

       entry $ bindMatch "define-template" $ nil_ $ \case
         [SymbolVal who, IsSimpleTemplate body ] -> do
          -- notice $ red "define-template" <+> pretty who <+> pretty what
          t <- lift $ asks fixmeEnvTemplates
          atomically $ modifyTVar t (HM.insert who (Simple (SimpleTemplate body)))

         _ -> throwIO $ BadFormException @C nil

       entry $ bindMatch "log:trace:on" $ nil_ $ const do
          lift $ setLogging @TRACE $ toStderr . logPrefix ""

       entry $ bindMatch "log:trace:off" $ nil_ $ const do
          lift $ setLoggingOff @TRACE

  conf <- readConfig

  argz <- liftIO getArgs

  let args = zipWith (\i s -> bindValue (mkId ("$_" <> show i)) (mkStr @C s )) [1..] argz
               & HM.unions

  run (dict <> args) (conf <> forms) >>= eatNil display

