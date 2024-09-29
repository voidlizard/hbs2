module Fixme.Run where

import Prelude hiding (init)
import Fixme.Prelude hiding (indent)
import Fixme.Types
import Fixme.Config
import Fixme.State
import Fixme.Run.Internal
import Fixme.Run.Internal.RefChan
import Fixme.Scan.Git.Local as Git
import Fixme.Scan as Scan
import Fixme.GK as GK

import Data.Config.Suckless.Script.File

import HBS2.KeyMan.Keys.Direct

import HBS2.Git.Local.CLI
import HBS2.Peer.Proto.RefChan.Types

import HBS2.CLI.Run.KeyMan (keymanNewCredentials)

import HBS2.OrDie
import HBS2.Peer.CLI.Detect
import HBS2.Net.Auth.GroupKeySymm
import HBS2.Data.Types.SignedBox
import HBS2.Base58
import HBS2.Storage.Operations.ByteString
import HBS2.Net.Auth.Credentials
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
import Control.Monad
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Maybe
import System.IO.Temp as Temp
import System.IO qualified as IO

import Streaming.Prelude qualified as S

{- HLINT ignore "Functor law" -}


recover :: (FixmePerks m) => FixmeEnv -> m a -> m a
recover env m = flip fix 0 $ \next attempt
    -> do m
        `catch` (\PeerNotConnected  -> do
                    if attempt < 1 then do
                      runWithRPC env $ next (succ attempt)
                    else do
                      throwIO PeerNotConnected
               )

withFixmeCLI :: (FixmePerks m, MonadReader FixmeEnv m) => FixmeEnv -> FixmeM m a -> m a
withFixmeCLI env m = do
  recover env do
    withFixmeEnv env m

runWithRPC :: (FixmePerks m) => FixmeEnv ->  m a -> m a
runWithRPC FixmeEnv{..} m = do

  soname <- detectRPC
              `orDie` "can't locate hbs2-peer rpc"

  flip runContT pure do

    client <- lift $ race (pause @'Seconds 1) (newMessagingUnix False 1.0 soname)
                >>= orThrowUser ("can't connect to" <+> pretty soname)

    void $ ContT $ withAsync $ runMessagingUnix client

    peerAPI    <- makeServiceCaller @PeerAPI (fromString soname)
    refChanAPI <- makeServiceCaller @RefChanAPI (fromString soname)
    storageAPI <- makeServiceCaller @StorageAPI (fromString soname)

    let endpoints = [ Endpoint @UNIX  peerAPI
                    , Endpoint @UNIX  refChanAPI
                    , Endpoint @UNIX  storageAPI
                    ]

    void $ ContT $ withAsync $ liftIO $ runReaderT (runServiceClientMulti endpoints) client


    let newEnv = Just (MyPeerClientEndpoints soname peerAPI refChanAPI storageAPI)
    liftIO $ atomically $ writeTVar fixmeEnvMyEndpoints newEnv
    lift m

runFixmeCLI :: forall a m . FixmePerks m => FixmeM m a -> m a
runFixmeCLI m = do
  git <- findGitDir
  env <- FixmeEnv
            <$>  newMVar ()
            <*>  newTVarIO mempty
            <*>  (pwd >>= newTVarIO)
            <*>  newTVarIO Nothing
            <*>  newTVarIO git
            <*>  newTVarIO mempty
            <*>  newTVarIO mempty
            <*>  newTVarIO mempty
            <*>  newTVarIO builtinAttribs
            <*>  newTVarIO builtinAttribVals
            <*>  newTVarIO mempty
            <*>  newTVarIO defCommentMap
            <*>  newTVarIO Nothing
            <*>  newTVarIO mzero
            <*>  newTVarIO mempty
            <*>  newTVarIO mempty
            <*>  newTVarIO defaultCatAction
            <*>  newTVarIO defaultTemplate
            <*>  newTVarIO mempty
            <*>  newTVarIO (1,3)
            <*>  newTVarIO mzero
            <*>  newTVarIO mzero
            <*>  newTVarIO mzero
            <*>  newTVarIO mzero
            <*>  newTVarIO mempty

  -- FIXME: defer-evolve
  --   не все действия требуют БД,
  --   хорошо бы, что бы она не создавалась,
  --   если не требуется
  recover env do
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


readConfig :: (FixmePerks m) => FixmeM m [Syntax C]
readConfig = do

  user <- userConfigs
  lo   <- localConfig

  w <- for (lo : user) $ \conf -> do
    try @_ @IOException (liftIO $ readFile conf)
      <&> fromRight mempty
      <&> parseTop
      >>= either (error.show) pure

  updateScanMagic

  pure $ mconcat w


runCLI :: FixmePerks m => FixmeM m ()
runCLI = do
  argz <- liftIO getArgs
  forms <- parseTop (unlines $ unwords <$> splitForms argz)
           & either  (error.show) pure

  runTop forms


runTop :: forall m . FixmePerks m => [Syntax C] -> FixmeM m ()
runTop forms = do

  tvd  <- newTVarIO mempty

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


       entry $ bindMatch "fixme-attribs" $ nil_ \case
        StringLikeList xs -> do
          ta <- lift $ asks fixmeEnvAttribs
          atomically $ modifyTVar ta (<> HS.fromList (fmap fromString xs))

        _ -> throwIO $ BadFormException @C nil


       entry $ bindMatch "fixme-files" $ nil_ \case
        StringLikeList xs -> do
          w <- lift fixmeWorkDir
          t <- lift $ asks fixmeEnvFileMask
          atomically (modifyTVar t (<> fmap (w </>) xs))

        _ -> throwIO $ BadFormException @C nil

       entry $ bindMatch "fixme-exclude" $ nil_ \case
        StringLikeList xs -> do
          w <- lift fixmeWorkDir
          t <- lift $ asks fixmeEnvFileExclude
          atomically (modifyTVar t (<> fmap (w </>) xs))

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

         _ -> throwIO $ BadFormException @C nil

       entry $ bindMatch "fixme-def-context" $ nil_ \case
        [LitIntVal a, LitIntVal b] -> do
          t <- lift $ asks fixmeEnvCatContext
          atomically $ writeTVar t (fromIntegral a, fromIntegral b)

        _ -> throwIO $ BadFormException @C nil


       entry $ bindMatch "modify" $ nil_ \case
        [ FixmeHashLike w, StringLike k, StringLike v ] -> lift do
          void $ runMaybeT do
            key <- lift (selectFixmeKey w) >>= toMPlus
            lift $ modifyFixme key [(fromString k, fromString v)]

        _ -> throwIO $ BadFormException @C nil


       entry $ bindMatch "delete" $ nil_ \case
        [ FixmeHashLike w ] -> lift do
          void $ runMaybeT do
            key <- lift (selectFixmeKey w) >>= toMPlus
            lift $ modifyFixme key [("deleted", "true")]

        _ -> throwIO $ BadFormException @C nil


       entry $ bindMatch "cat" $ nil_ $ \case
        [ FixmeHashLike w ] -> lift do
          cat_ w

        _ -> throwIO $ BadFormException @C nil

       entry $ bindMatch "dump" $ nil_ $ \case
        [ FixmeHashLike w ] -> lift $ void $ runMaybeT do
          key <- lift (selectFixmeKey w) >>= toMPlus
          fme <- lift $ getFixme key
          liftIO $ print $ pretty fme

        _ -> throwIO $ BadFormException @C nil
          -- magic <- lift $ asks fixmeEnvScanMagic >>= readTVarIO
          -- liftIO $ print $ pretty magic


       entry $ bindMatch "report" $ nil_ $ lift . \case
        ( SymbolVal "template" : StringLike t : p )  -> do
          report (Just t) p

        ( SymbolVal "--template" : StringLike t : p )  -> do
          report (Just t) p

        p -> do
          report Nothing p

       entry $ bindMatch "fixme:key:show" $ nil_ \case
        [ FixmeHashLike w ] -> lift $ void $ runMaybeT do
          key <- lift (selectFixmeKey w) >>= toMPlus
          liftIO $ print $ pretty key

        _ -> throwIO $ BadFormException @C nil

       entry $ bindMatch "fixme:scan-magic" $ nil_ $ const do
          magic <- lift $ asks fixmeEnvScanMagic >>= readTVarIO
          liftIO $ print $ pretty magic

       entry $ bindMatch "fixme:gk:show" $ nil_ $ const do
        w <- lift loadGroupKey
        case w of
          Just (h,_) -> do
            liftIO $ print $ pretty h
          _ -> do
            liftIO $ print $ pretty "none"

       entry $ bindMatch "fixme:path" $ nil_ $ const do
          path <- lift fixmeWorkDir
          liftIO $ print $ pretty path

       entry $ bindMatch "fixme:files" $ nil_ $ const do
          w <- lift fixmeWorkDir
          incl <- lift (asks fixmeEnvFileMask >>= readTVarIO)
          excl <- lift (asks fixmeEnvFileExclude >>= readTVarIO)
          glob incl excl w  $ \fn -> do
            liftIO $ putStrLn (makeRelative w fn)
            pure True

       entry $ bindMatch "fixme:state:drop" $ nil_ $ const $ lift do
        cleanupDatabase

       entry $ bindMatch "fixme:state:cleanup" $ nil_ $ const $ lift do
        cleanupDatabase


       entry $ bindMatch "fixme:state:count-by-attribute" $ nil_ $ \case
        [StringLike s] -> lift do
            rs <- countByAttribute (fromString s)
            for_ rs $ \(n,v) -> do
              liftIO $ print $ pretty n <+> pretty v

        _ -> throwIO $ BadFormException @C nil

       entry $ bindMatch "fixme:git:import" $ nil_ $ const $ lift do
          import_

       entry $ bindMatch "fixme:git:list" $ nil_ $ const do
          fxs <- lift scanFiles
          for_ fxs $ \fme -> do
            liftIO $ print $ pretty fme

       -- TODO: some-uncommited-shit

       -- TODO: some-shit
       --  one

       -- TODO: some-shit
       --  new text

       entry $ bindMatch "env:show" $ nil_ $ const $ do
        lift printEnv

       entry $ bindMatch "refchan:show" $ nil_ $ const do
          tref <- lift $ asks fixmeEnvRefChan
          r <- readTVarIO tref
          liftIO $ print $ pretty (fmap AsBase58 r)

       entry $ bindMatch "refchan" $ nil_ \case
        [SignPubKeyLike rchan] -> do
          tref<- lift $ asks fixmeEnvRefChan
          atomically  $ writeTVar tref (Just rchan)

        _ -> throwIO $ BadFormException @C nil

       entry $ bindMatch "author" $ nil_ \case
        [SignPubKeyLike au] -> do
          t <- lift $ asks fixmeEnvAuthor
          atomically  $ writeTVar t (Just au)

        _ -> throwIO $ BadFormException @C nil

       entry $ bindMatch "reader" $ nil_ \case
        [EncryptPubKeyLike reader] -> do
          t <- lift $ asks fixmeEnvReader
          atomically  $ writeTVar t (Just reader)

        _ -> throwIO $ BadFormException @C nil

       entry $ bindMatch "git:commits" $ const $ do
        co <- lift listCommits <&> fmap (mkStr @C . view _1)
        pure $ mkList co

       entry $ bindMatch "fixme:refchan:export" $ nil_ $ \case
          [SymbolVal "dry"] -> do
            notice $ yellow "export is running in dry mode"
            void $ lift $ refchanExport [RefChanExportDry]

          _ ->  void $ lift $ refchanExport ()

       entry $ bindMatch "fixme:refchan:import" $ nil_ $ \case
          _ ->  void $ lift $ refchanImport

       entry $ bindMatch "fixme:gk:export" $ nil_ $ \case
          _ ->  void $ lift $ refchanExportGroupKeys

       entry $ bindMatch "source" $ nil_ $ \case
        [StringLike path] -> do

          ppath <- if List.isPrefixOf "." path then do
                      dir <- lift localConfigDir
                      let rest = tail $ splitDirectories path
                      pure $ joinPath (dir:rest)
                   else do
                      canonicalizePath path

          debug $ red "SOURCE FILE" <+> pretty ppath

          dd <- readTVarIO tvd

          -- FIXME: raise-warning?
          content <- liftIO $ try @_ @IOException (readFile ppath)
                        <&> fromRight mempty
                        <&> parseTop
                        >>= either (error.show) pure

          lift $ run dd content

        _ -> throwIO $ BadFormException @C nil

       entry $ bindMatch "update" $ nil_ $ const $ lift do
        refchanUpdate

       entry $ bindMatch "update" $ nil_ $ const $ lift do
        refchanUpdate

       entry $ bindMatch "fixme:refchan:update" $ nil_ $ const $ lift do
        refchanUpdate


       entry $ bindMatch "cache:ignore" $ nil_ $ const $ lift do
        tf <- asks fixmeEnvFlags
        atomically $ modifyTVar tf (HS.insert FixmeIgnoreCached)

       entry $ bindMatch "git:blobs" $  \_ -> do
        blobs <- lift (listBlobs Nothing)

        elems <- for blobs $ \(f,h) -> do
                    pure $ mkList @C [ mkStr f, mkSym ".", mkStr h ]

        pure $ mkList @C elems

       entry $ bindMatch "init" $ nil_ $ const $ do
        lift init

       brief "initializes a new refchan" $
         desc ( vcat [
                   "Refchan is an ACL-controlled CRDT channel useful for syncronizing"
                 , "fixme-new state amongst the different remote setups/peers/directories"
                 , "use it if you want to use fixme-new in a distributed fashion"
                 ]
              ) $
         args [] $
         returns "string" "refchan-key" $ do
         entry $ bindMatch "fixme:refchan:init" $ nil_ $ \case
          [] -> lift $ fixmeRefChanInit Nothing
          [SignPubKeyLike rc] -> lift $ fixmeRefChanInit (Just rc)
          _ -> throwIO $ BadFormException @C nil

       entry $ bindMatch "set-template" $ nil_ \case
         [SymbolVal who, SymbolVal w] -> do
           templates <- lift $ asks fixmeEnvTemplates
           t <- readTVarIO templates
           for_ (HM.lookup w t) $ \tpl -> do
             atomically $ modifyTVar templates (HM.insert who tpl)

         _ -> throwIO $ BadFormException @C nil

       entry $ bindMatch "define-template" $ nil_ $ \case
         [SymbolVal who, IsSimpleTemplate body ] -> do
          t <- lift $ asks fixmeEnvTemplates
          atomically $ modifyTVar t (HM.insert who (Simple (SimpleTemplate body)))

         _ -> throwIO $ BadFormException @C nil

       entry $ bindMatch "log:trace:on" $ nil_ $ const do
          lift $ setLogging @TRACE $ toStderr . logPrefix ""

       entry $ bindMatch "log:trace:off" $ nil_ $ const do
          lift $ setLoggingOff @TRACE

       entry $ bindMatch "log:debug:on" $ nil_ $ const do
          lift $ setLogging @DEBUG $ toStderr . logPrefix ""

       entry $ bindMatch "log:debug:off" $ nil_ $ const do
          lift $ setLoggingOff @DEBUG

       entry $ bindMatch "debug:peer:check" $ nil_ $ const do
        peer <- lift $ getClientAPI @PeerAPI @UNIX
        poked <- callRpcWaitMay @RpcPoke (TimeoutSec 1) peer ()
                   <&> fromMaybe "hbs2-peer not connected"
        liftIO $ putStrLn poked


  argz <- liftIO getArgs

  conf <- readConfig

  let args = zipWith (\i s -> bindValue (mkId ("$_" <> show i)) (mkStr @C s )) [1..] argz
               & HM.unions

  let finalDict = dict <> args -- :: Dict C (FixmeM m)

  atomically $ writeTVar tvd finalDict

  run finalDict (conf <> forms) >>= eatNil display

