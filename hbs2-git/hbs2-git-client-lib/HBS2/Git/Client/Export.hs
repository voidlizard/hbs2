module HBS2.Git.Client.Export (export) where


import HBS2.Git.Client.Prelude hiding (info)
import HBS2.Git.Client.App.Types
import HBS2.Git.Client.Config
import HBS2.Git.Client.RefLog
import HBS2.Git.Client.State
import HBS2.Git.Client.Progress

import HBS2.Git.Data.RefLog
import HBS2.Git.Data.Tx.Git
import HBS2.Git.Data.LWWBlock
import HBS2.Git.Data.GK

import HBS2.Git.Local.CLI

import HBS2.KeyMan.Keys.Direct

import HBS2.OrDie
import HBS2.Storage.Operations.ByteString
import HBS2.System.Dir

import Text.InterpolatedString.Perl6 (qc)
import Data.Text qualified as Text
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Builder as B
import Data.HashSet qualified as HashSet
import Data.HashMap.Strict qualified as HashMap
import Data.Maybe
import Data.List qualified as L
import Data.Ord (comparing)
import Data.Either

data ExportError = ExportUnsupportedOperation
                 | ExportBundleCreateError
                   deriving stock (Show,Typeable)

instance Exception ExportError

instance HasErrorStatus ExportError where
  getStatus = \case
    ExportUnsupportedOperation -> Failed
    ExportBundleCreateError -> Failed

instance ToFilePath (GitRef, GitHash) where
  toFilePath (g, r) = show (pretty g)

{-# ANN module "HLint: ignore Eta reduce" #-}
filterPat :: ToFilePath a =>  [FilePattern] -> [FilePattern] -> [a] -> [a]
filterPat inc excl refs = filter check refs
  where
    check r = i || not e
     where
       e = not $ L.null $ catMaybes [ match p (toFilePath r) | p <- excl ]
       i = not $ L.null $ catMaybes [ match p (toFilePath r) | p <- inc  ]

refsForExport :: (MonadReader GitEnv m, MonadIO m) => [(GitRef, Maybe GitHash)] -> m [(GitRef,GitHash)]

refsForExport forPushL = do
{- HLINT ignore "Functor law" -}

  conf <- asks _config
  path <- asks _gitPath

  let tags = headDef mempty [ "--tags" :: String | (ListVal [SymbolVal "export", SymbolVal "tags"] ) <- conf]

  let incl = [ Text.unpack p
             | (ListVal [SymbolVal "export", SymbolVal "include", LitStrVal p]) <- conf
             ]

  let excl = [ Text.unpack p
             | (ListVal [SymbolVal "export", SymbolVal "exclude", LitStrVal p]) <- conf
             ]

  let forPush = [ (k,v) | (k, Just v) <- forPushL ] & HashMap.fromList

  let deleted = [ k | (k, Nothing) <- forPushL ] & HashSet.fromList

  debug $ red "CONF" <> pretty path <> line <> indent 2 (vcat (fmap pretty conf))

  let cmd = [qc|git --git-dir={path} show-ref {tags} --heads --head|]

  debug $ red "CMD" <+> pretty cmd
  debug $ "FILTERS" <+> pretty (incl, excl)
  debug $ red "DELETED" <+> pretty (HashSet.toList deleted)
  debug $ red "FOR-PUSH" <+> pretty (HashMap.toList forPush)

  -- мы экспортируем всегда HEAD что бы правильно работал git clone
  -- поэтому мы экспортируем и текущий бранч тоже
  -- даже если он запрещён фильтрами

  currentBranch <- gitRunCommand [qc|git --git-dir={path} symbolic-ref HEAD|]
                     >>= orThrowUser "can't read HEAD 1"
                     <&> GitRef . BS8.strip . LBS8.toStrict

  currentVal <- gitRunCommand [qc|git --git-dir={path} rev-parse {pretty currentBranch}|]
                  >>= orThrowUser "can't read HEAD 2"
                  <&> (BS8.unpack . BS8.strip . LBS8.toStrict)
                  <&> fromStringMay @GitHash
                  >>= orThrowUser "invalid git hash for HEAD"

  gitRunCommand cmd
    >>= orThrowUser ("can't read git repo" <+> pretty path)
    <&> LBS8.lines
    <&> fmap LBS8.words
    <&> mapMaybe \case
         [val,name] -> (GitRef (LBS8.toStrict name),) <$> fromStringMay @GitHash (LBS8.unpack val)
         _          -> Nothing
    <&> filterPat incl excl
    <&> HashMap.fromList
    <&> HashMap.filterWithKey (\k _ -> not (HashSet.member k deleted))
    <&> mappend forPush
    <&> mappend (HashMap.singleton currentBranch currentVal)
    <&> HashMap.toList
    <&> L.sortBy orderRefs

  where
    orderRefs (GitRef "HEAD", _) _ = LT
    orderRefs _ (GitRef "HEAD", _) = GT
    orderRefs x y = comparing fst x y

loadNewGK0 :: (MonadIO m, MonadReader GitEnv m)
           => RefLogId
           -> Maybe HashRef
           -> m (Maybe (HashRef,Epoch))

loadNewGK0 r = \case
  Nothing -> storeNewGK0

  Just tx0 -> do
    href <- storeNewGK0
    withState do
      for_ href (insertNewGK0 r tx0 . fst)
      commitAll

    withState $ selectNewGK0 r

storeNewGK0 :: (MonadIO m, MonadReader GitEnv m) => m (Maybe (HashRef,Epoch))
storeNewGK0 = do
  sto <- asks _storage
  enc <- asks _gitExportEnc
  runMaybeT do
    gkf <- headMay [ f | ExportPrivate f <- [enc] ] & toMPlus
    gk <- loadGK0FromFile gkf >>= toMPlus
    epoch <- getEpoch
    writeAsMerkle sto (serialise gk) <&> HashRef <&> (,epoch)

export :: ( GitPerks m
          , MonadReader GitEnv m
          , GroupKeyOperations m
          , HasAPI PeerAPI UNIX m
          )
       => LWWRefKey 'HBS2Basic
       -> [(GitRef,Maybe GitHash)]
       -> m ()
export key refs  = do

  git <- asks _gitPath
  sto <- asks _storage
  new <- asks _gitExportType <&> (== ExportNew)
  reflog <- asks _refLogAPI
  ip <- asks _progress

  subscribeLWWRef key

  (lww, LWWBlockData{..}) <- waitOrInitLWWRef

  let puk0 = fromLwwRefKey key

  debug $ red $ pretty $ AsBase58 lwwRefLogPubKey

  (sk0,pk0) <- liftIO $ runKeymanClient do
                creds <- loadCredentials puk0
                             >>= orThrowUser ("can't load credentials" <+> pretty (AsBase58 puk0))
                pure ( view peerSignSk  creds, view peerSignPk creds )

  (puk,sk) <- derivedKey @'HBS2Basic @'Sign lwwRefSeed sk0

  subscribeRefLog puk

  myrefs <- refsForExport refs

  let myrefsKey = L.sortOn fst myrefs & serialise & hashObject @HbSync & HashRef

  flip runContT pure do
    callCC \exit -> do


      tx0 <- getLastAppliedTx

      rh <- runMaybeT ( toMPlus tx0 >>= readRepoHeadFromTx sto >>= toMPlus )

      let rh0 = snd <$> rh

      (name,brief,mf) <- lift getManifest

      gk0new0 <- loadNewGK0 puk tx0

      let gk0old = _repoHeadGK0 =<< rh0

      mbTxTime0 <- runMaybeT $ toMPlus tx0
                                >>= withState .selectTxForRefLog puk
                                >>= toMPlus

      -- смотрим, какое время ключа для данного рефлога, т.к. голова-то
      -- может быть одна, а вот рефлоги -- разные
      -- если мы успели --- то накатываем свой ключ.
      -- если нет -- придется повторить
      let gk0new = if (snd <$> gk0new0) > (snd <$> mbTxTime0) then
                     fst <$> gk0new0
                   else
                     gk0old

      let gk0 = gk0new <|> gk0old

      repohead <- makeRepoHeadSimple name brief mf gk0 myrefs

      let oldRefs = maybe mempty _repoHeadRefs rh0

      trace $ "TX0" <+> pretty  tx0

      bss <- maybe (pure mempty) txBundles tx0

      objs <- lift enumAllGitObjects
                >>= withState . filterM (notInTx tx0)

      when (null objs && not new && oldRefs == myrefs) do
        exit ()

      debug $ red "REFS-FOR-EXPORT:" <+> pretty myrefs

      done <- withState (selectBundleByKey puk myrefsKey)

      out <-
        if isJust done && not new then do
          pure []

        else do

          p <- ContT $ withGitPack

          for_ (zip [1..] objs) $ \(n,o) -> do
            onProgress ip (ExportWriteObject (Progress n Nothing))
            liftIO $ LBS8.hPutStrLn (getStdin p) (LBS8.pack $ show $ pretty o)

          code <- hFlush (getStdin p) >> hClose (getStdin p) >> getExitCode p

          let idx = serialise objs
          let size = B.word32BE (fromIntegral $ LBS.length idx)
          let hdr  = B.word32BE 1
          pack <- liftIO $ LBS.hGetContents (getStdout p)
          let out = B.toLazyByteString ( size <> hdr <> B.lazyByteString idx <> B.lazyByteString pack )
          pure [out]

      rank <- getEpoch <&> fromIntegral

      let rw = gk0new /= gk0old

      debug $ red "MAKE TX" <+> pretty rw <+> pretty gk0old <+> "->" <+> pretty gk0new

      tx <- lift $ makeTx sto rw rank puk (const $ pure (Just sk)) repohead bss out

      r <- lift $ race (pause @'Seconds 1) (callService @RpcRefLogPost reflog tx)
             >>= orThrowUser "hbs2-peer rpc timeout"

      when (isLeft r) $ exit ()

      void $ runMaybeT do
        (_,_,bh) <- unpackTx tx
        withState (insertBundleKey puk myrefsKey bh)

  where

    findSK pk = liftIO $ runKeymanClient $ runMaybeT do
        creds  <- lift (loadCredentials pk) >>= toMPlus
        pure (view peerSignSk creds)

    waitOrInitLWWRef  = do
      sto <- asks _storage
      new <- asks _gitExportType <&> (== ExportNew)

      flip fix 3 $ \next n -> do
               blk <- readLWWBlock sto key

               case blk of
                Just x -> pure x

                Nothing | new && n > 0 -> do
                  _ <- runExceptT (initLWWRef sto Nothing findSK key)
                        >>= either ( throwIO . userError . show ) pure

                  next (pred n)

                        | otherwise -> do
                  -- FIXME: detailed-error-description
                  orThrowUser "lwwref not available" Nothing


    notInTx Nothing _ = pure True
    notInTx (Just tx0) obj = not <$> isObjectInTx tx0 obj

    getLastAppliedTx = runMaybeT do
      (tx0,_) <- withState selectMaxAppliedTx
                    >>= toMPlus
      pure tx0

    txBundles tx0 = withDef =<< runMaybeT do

      new <- asks _gitExportType <&> (== ExportNew)
      sto <- asks _storage

      txbody  <- runExceptT (readTx sto tx0)
                   >>= orThrowUser ("missed blocks for tx" <+> pretty tx0)

      let bref = view _4 txbody

      readBundleRefs sto bref
        >>= orThrowUser ("missed blocks for tx" <+> pretty tx0)

      where
        withDef Nothing = pure mempty
        withDef (Just x) = pure x

enumAllGitObjects :: (GitPerks m, MonadReader GitEnv m) => m [GitHash]
enumAllGitObjects = do
  path <- asks _gitPath
  let rcmd = [qc|git --git-dir {path} cat-file --batch-check='%(objectname)'  --batch-all-objects|]
  (_, out, _) <- liftIO $ readProcess (shell rcmd)
  pure $ LBS8.lines out & mapMaybe (fromStringMay @GitHash . LBS8.unpack)


withGitPack ::  (GitPerks m, MonadReader GitEnv m) => (Process Handle Handle () -> m a) -> m a
withGitPack action = do
  fp <- asks _gitPath
  let cmd = "git"
  let args = ["--git-dir", fp, "pack-objects", "--stdout", "-q"]
  let config = setStdin createPipe $ setStdout createPipe $ setStderr closed $ proc cmd args
  p <- startProcess config
  action p


