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

notEmpty :: [a] -> Maybe [a]
notEmpty = \case
  [] -> Nothing
  x  -> Just x

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
          w <- fixmeWorkDir
          t <- lift $ asks fixmeEnvFileMask
          atomically (modifyTVar t (<> fmap (w </>) xs))

        _ -> throwIO $ BadFormException @C nil

       entry $ bindMatch "fixme-exclude" $ nil_ \case
        StringLikeList xs -> do
          w <- fixmeWorkDir
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

       entry $ bindMatch "fixme:scan:import" $ nil_ $ const $ lift do
          import_

       entry $ bindMatch "fixme:scan:list" $ nil_ $ const do
          fxs <- lift scanFiles
          for_ fxs $ \fme -> do
            liftIO $ print $ pretty fme

       -- TODO: some-uncommited-shit

       -- TODO: some-shit
       --  one

       -- TODO: some-shit
       --   two
       --   updated

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

       entry $ bindMatch "git:commits" $ const $ do
        co <- lift listCommits <&> fmap (mkStr @C . view _1)
        pure $ mkList co

       -- TODO: implement-fixme:refchan:export
       entry $ bindMatch "fixme:refchan:export" $ nil_ \case
        _ -> none

       entry $ bindMatch "git:import" $ nil_ $ const do
        error "not implemented yet"
        -- lift $ scanGitLocal mempty Nothing

       entry $ bindMatch "git:blobs" $  \_ -> do
        blobs <- lift listRelevantBlobs

        elems <- for blobs $ \(f,h) -> do
                    pure $ mkList @C [ mkStr f, mkSym ".", mkStr h ]

        pure $ mkList @C elems

       entry $ bindMatch "init" $ nil_ $ const $ do
        lift init

       entry $ bindMatch  "refchan:sync" $ nil_ $ const  do
        notice "running export to refchan"

        rchan <- lift $ asks fixmeEnvRefChan
                    >>= readTVarIO
                    >>= orThrowUser "refchan is not set"

        notice $ "1. read refchan" <+> pretty (AsBase58 rchan)

        -- fxs <- lift $ selectFixmeThin ()

--         for_ fxs $ \(FixmeThin x) -> void $ runMaybeT do
--           h <- HM.lookup "fixme-hash" x & toMPlus
--           notice $ pretty h

        notice "2. read issues from state"
        notice "3. discover new issues"
        notice "3. save merkle tree of new issues"
        notice "4. post refchan tx"
        pure ()

       brief "initializes a new refchan" $
         desc ( vcat [
                   "Refchan is an ACL-controlled CRDT channel useful for syncronizing"
                 , "fixme-new state amongst the different remote setups/peers/directories"
                 , "use it if you want to use fixme-new in a distributed fashion"
                 ]
              ) $
         args [] $
         returns "string" "refchan-key" $ do
         entry $ bindMatch "refchan:init" $ nil_ $ const $ do

           let rch0 = refChanHeadDefault @L4Proto
           sto <- lift getStorage
           peer <- lift $ getClientAPI @PeerAPI @UNIX
           rchanApi <- lift $ getClientAPI @RefChanAPI @UNIX

           confFile <- localConfig
           conf <- liftIO (readFile confFile)
                      <&> parseTop
                      <&> either (error.show) (fmap (fixContext @_ @C))

           let already = headMay [ x
                                 | ListVal [StringLike "refchan", SignPubKeyLike x] <- conf
                                 ]

           flip runContT pure $ callCC \done -> do

             when (isJust already) do
              warn $ red "refchan is already set" <+> pretty (fmap AsBase58 already)

             poked <- lift $ callRpcWaitMay @RpcPoke (TimeoutSec 1) peer ()
                        >>= orThrowUser "hbs2-peer not connected"
                        <&> parseTop
                        <&> fromRight mempty

             pkey <- [ fromStringMay @(PubKey 'Sign 'HBS2Basic) x
                     | ListVal [SymbolVal "peer-key:", StringLike x ] <- poked
                     ] & headMay . catMaybes & orThrowUser "hbs2-peer key not set"


             notice $ green "default peer" <+> pretty (AsBase58 pkey)


             signK' <- lift $ runKeymanClientRO $ listCredentials
                     <&> headMay

             signK <- ContT $ maybe1 signK' (throwIO $ userError "no default author key found in hbs2-keyman")

             notice $ green "default author" <+> pretty (AsBase58 signK)

             -- TODO: use-hbs2-git-api?
             (_, gkh', _) <- readProcess (shell [qc|git hbs2 key|])
                              <&> over _2 (  (fromStringMay @HashRef)  <=< (notEmpty . headDef "" . lines . LBS8.unpack) )
                              <&> \x ->  case view _1 x of
                                    ExitFailure _ -> set _2 Nothing x
                                    ExitSuccess   -> x

             notice $ green "group key" <+> maybe "none" pretty gkh'

             readers <- fromMaybe mempty <$> runMaybeT do
                          gh <- toMPlus gkh'
                          gk <- loadGroupKeyMaybe @'HBS2Basic sto gh
                                   >>= toMPlus
                          pure $ HM.keys (recipients gk)

             notice $ green "readers" <+> pretty (length readers)

             let rch1 = rch0 & set refChanHeadReaders (HS.fromList readers)
                             & set refChanHeadAuthors (HS.singleton signK)
                             & set refChanHeadPeers   (HM.singleton pkey 1)


             let unlucky =    HM.null (view refChanHeadPeers rch1)
                           || HS.null (view refChanHeadAuthors rch1)


             liftIO $ print $ pretty rch1

             if unlucky then do
                warn $ red $ "refchan definition is not complete;" <+>
                             "you may add missed keys, edit the"   <+>
                             "defition and add if manually or repeat init attempt"
                             <> line
             else do
                notice "refchan definition seems okay, adding new refchan"
                refchan <- lift $ keymanNewCredentials (Just "refchan") 0

                creds <- lift $ runKeymanClientRO $ loadCredentials refchan
                               >>= orThrowUser "can't load credentials"

                let box = makeSignedBox @'HBS2Basic (view peerSignPk creds) (view peerSignSk creds) rch1

                href <- writeAsMerkle sto  (serialise box)

                callService @RpcPollAdd peer (refchan, "refchan", 17)
                    >>= orThrowUser "can't subscribe to refchan"

                callService @RpcRefChanHeadPost rchanApi (HashRef href)
                    >>= orThrowUser "can't post refchan head"

                liftIO $ appendFile confFile $
                     show $ pretty ( mkList @C [ mkSym "refchan"
                                               , mkSym (show $ pretty (AsBase58 refchan)) ]
                                   )


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

  conf <- readConfig

  argz <- liftIO getArgs

  let args = zipWith (\i s -> bindValue (mkId ("$_" <> show i)) (mkStr @C s )) [1..] argz
               & HM.unions

  let finalDict = dict <> args -- :: Dict C (FixmeM m)

  atomically $ writeTVar tvd finalDict

  run finalDict (conf <> forms) >>= eatNil display

