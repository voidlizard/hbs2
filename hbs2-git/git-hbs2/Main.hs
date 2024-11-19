{-# Language UndecidableInstances #-}
module Main where

import HBS2.Git.Client.Prelude hiding (info, Input(..))
import HBS2.Git.Client.App
import HBS2.Git.Client.Export
import HBS2.Git.Client.Import
import HBS2.Git.Client.State
import HBS2.Git.Client.Manifest

import HBS2.Data.Types.SignedBox
import HBS2.Git.Data.RepoHead
import HBS2.Git.Data.RefLog
import HBS2.Git.Local.CLI qualified as Git
import HBS2.Git.Data.Tx.Git qualified as TX
import HBS2.Git.Data.Tx.Git (RepoHead(..))
import HBS2.Git.Data.Tx.Index
import HBS2.Git.Data.LWWBlock
import HBS2.Peer.Proto.RefChan.Types
import HBS2.Git.Data.GK

import HBS2.KeyMan.Keys.Direct
import HBS2.Storage.Operations.ByteString

import Data.Config.Suckless.Script

import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.HashSet qualified as HS
import Data.Maybe
import Data.List (nubBy)
import Data.List qualified as L
import Data.Function (on)
import Data.HashMap.Strict qualified as HM
import Data.Coerce
import Options.Applicative as O
import Data.ByteString.Lazy qualified as LBS
import Prettyprinter
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.ByteString (ByteString)
-- import Data.ByteString.Lazy (ByteString)
import Text.InterpolatedString.Perl6 (qc)

import Streaming.Prelude qualified as S

import System.Exit

{- HLINT ignore "Functor law" -}

globalOptions :: Parser [GitOption]
globalOptions = do

  t <- flag [] [GitTrace]
           (  long "trace" <> short 't' <> help "allow trace"
           )

  d <- flag [] [GitDebug]
           (  long "debug" <> short 'd' <> help "allow debug"
           )

  pure (t <> d)

commands :: GitPerks m => Parser (GitCLI m ())
commands =
  hsubparser (  command "export"   (info pExport   (progDesc "export repo to hbs2-git"))
             <> command "import"   (info pImport   (progDesc "import repo from reflog"))
             <> command "fsck"     (info pFsck     (progDesc "check objects from a last reflog transaction"))
             <> command "key"      (info pKey      (progDesc "key management"))
             <> command "manifest" (info pManifest (progDesc "manifest commands"))
             <> command "track"    (info pTrack    (progDesc "track tools"))
             <> command "tools"    (info pTools    (progDesc "misc tools"))
             <> command "run"      (info pRun      (progDesc "run new cli command; run help to figure it out"))
             )


pRefLogId :: ReadM RefLogId
pRefLogId = maybeReader (fromStringMay @RefLogId)

pRefChanId :: ReadM GitRefChanId
pRefChanId = maybeReader (fromStringMay @GitRefChanId)

pLwwKey :: ReadM (LWWRefKey 'HBS2Basic)
pLwwKey = maybeReader fromStringMay

pHashRef :: ReadM HashRef
pHashRef = maybeReader (fromStringMay @HashRef)

pInit :: GitPerks m => Parser (GitCLI m ())
pInit = do
  pure runDefault


pRun :: GitPerks m => Parser (GitCLI m ())
pRun = do
  args <- many (strArgument (metavar "SCRIPT"))
  pure $ runScriptArgs args

pExport :: GitPerks m => Parser (GitCLI m ())
pExport = do

  puk <- argument pLwwKey (metavar "REFLOG-KEY")

  et <- flag ExportInc ExportNew
           (  long "new" <> help "new is usable to export to a new empty reflog"
           )

  enc <- flag' ExportPublic (long "public" <> help "create unencrypted reflog")
          <|>
          ( ExportPrivate <$>
                strOption (long "encrypted" <> help "create encrypted reflog"
                                            <> metavar "GROUP-KEY-FILE")
          )

  pure do
    git <- Git.gitDir >>= orThrowUser "not a git dir"
    notice (green "git dir" <+> pretty git <+> pretty (AsBase58 puk))

    env <- ask

    withGitEnv ( env & set gitApplyHeads False & set gitExportType et & set gitExportEnc enc) do
      unless (et == ExportNew) do
        importRepoWait puk

      export puk mempty

pImport :: GitPerks m => Parser (GitCLI m ())
pImport = do
  puk <- argument pLwwKey (metavar "LWWREF")

  pure do
    git <- Git.gitDir >>= orThrowUser "not a git dir"
    importRepoWait puk

pFsck :: GitPerks m => Parser (GitCLI m ())
pFsck = do
  lww <- argument pLwwKey (metavar "LWWREF")
  pure do
    git <- Git.gitDir >>= orThrowUser "not a git dir"
    fsckRepo lww

pTools :: GitPerks m => Parser (GitCLI m ())
pTools = hsubparser (  command "dump-pack" (info pDumpPack (progDesc "dump hbs2 git pack"))
                    <> command "show-ref" (info pShowRef (progDesc "show current references"))
                    <> command "show-remotes" (info pShowLww (progDesc "show current remotes (hbs2 references)"))
                    )


data DumpOpt = DumpInfoOnly | DumpObjects | DumpPack

pDumpPack :: GitPerks m => Parser (GitCLI m ())
pDumpPack = do
  what <- dumpInfoOnly <|> dumpObjects <|> dumpPack
  pure do
    co <- liftIO LBS.getContents

    (idSize,idVer,sidx,pack) <- TX.unpackPackMay co
                                  & orThrowUser "can't unpack the bundle"

    case what of
      DumpInfoOnly -> do
        liftIO $ print $ pretty "version:"    <+> pretty idVer  <> line
                             <> "index size:" <+> pretty idSize <> line
                             <> "objects:"    <+> pretty (length sidx)
      DumpObjects -> do
        liftIO $ print $ vcat (fmap pretty sidx)

      DumpPack -> do
        liftIO $ LBS.putStr pack

  where
    dumpInfoOnly = flag DumpInfoOnly DumpInfoOnly
                      ( long "info-only" )

    dumpObjects = flag DumpObjects DumpObjects
                      ( long "objects" )

    dumpPack = flag DumpPack DumpPack
                      ( long "pack" )


pShowLww :: GitPerks m => Parser (GitCLI m ())
pShowLww = pure do
  items <- withState selectAllLww
  liftIO $ print $ vcat (fmap fmt items)
  where
    fmt (l,n,k) = fill 4 (pretty n) <+> fill 32 (pretty l) <+> fill 32 (pretty (AsBase58 k))

pShowRef :: GitPerks m => Parser (GitCLI m ())
pShowRef = do
  remote <- strArgument (metavar "REMOTE")
  pure do
    runScript [mkList @C [mkSym "remote:refs:show", mkSym remote]]

pManifest :: GitPerks m => Parser (GitCLI m ())
pManifest = hsubparser (   command "list" (info pManifestList (progDesc "list all manifest"))
                        <> command "show" (info pManifestShow (progDesc "show manifest"))
                        <> command "update" (info pManifestUpdate (progDesc "update manifest"))
                       )

pManifestList :: GitPerks m => Parser (GitCLI m ())
pManifestList = do
  what <- argument pLwwKey (metavar "LWWREF")
  pure do
    repoHeadRefs' <- withState $ selectRepoHeadsFor ASC what
    sto <- getStorage
    repoHeads <- for repoHeadRefs' $ \repoHeadRef -> runMaybeT $ do
      repoHead <- runExceptT (readFromMerkle sto (SimpleKey (coerce repoHeadRef)))
        >>= toMPlus
        <&> deserialiseOrFail @RepoHead
        >>= toMPlus
      pure (repoHeadRef, repoHead)
    let removeDuplicates = nubBy ((==) `on` (_repoManifest . snd))
    let filteredRepoHeads = removeDuplicates $ catMaybes repoHeads
    for_ filteredRepoHeads $ \(repoHeadRef, repoHead) -> do
      let mfLen = maybe 0 Text.length (_repoManifest repoHead)
      let mf = parens ("manifest length" <+> pretty mfLen)
      liftIO $ print $ pretty (_repoHeadTime repoHead)
                         <+> pretty repoHeadRef
                         <+> mf

pManifestShow :: GitPerks m => Parser (GitCLI m ())
pManifestShow = do
  what <- argument pHashRef (metavar "HASH")
  pure do

      sto <- getStorage
      rhead <- runExceptT (readFromMerkle sto (SimpleKey (coerce what)))
                 >>= orThrowUser "repo head not found"
                 <&> deserialiseOrFail @RepoHead
                 >>= orThrowUser "repo head format not supported"

      liftIO $ for_ (_repoManifest rhead) Text.putStrLn

data Input
  = FileInput FilePath
  | StdInput

manifestFileInput :: Parser Input
manifestFileInput = FileInput <$> strOption
  (  long "file"
  <> short 'f'
  <> metavar "FILENAME"
  <> help "Read manifest from file" )

manifestStdInput :: Parser Input
manifestStdInput = flag' StdInput
  (  long "stdin"
  <> help "Read manifest from stdin" )

pManifestUpdate :: (GitPerks m) => Parser (GitCLI m ())
pManifestUpdate = do
  what <- argument pLwwKey (metavar "LWWREF")
  manifestInput <- manifestFileInput <|> manifestStdInput
  et <-
    flag
      ExportInc
      ExportNew
      ( long "new" <> help "This flag is used for new repositories. It allows you to skip the step of downloading data from peers."
      )
  pure do
    manifest <- case manifestInput of
      FileInput f -> do
         t <- liftIO $ Text.readFile f
         addManifestBriefAndName $ Just t
      StdInput -> do
        t <- liftIO $ Text.getContents
        addManifestBriefAndName $ Just t
    env <- ask
    enc <- getRepoEnc
    let manifestUpdateEnv = Just $ ManifestUpdateEnv {_manifest = manifest}
    withGitEnv
      ( env
          & set gitApplyHeads False
          & set gitExportType et
          & set gitExportEnc enc
          & set gitManifestUpdateEnv manifestUpdateEnv
      )
      do
        unless (et == ExportNew) do
          importRepoWait what
        export what mempty
        importRepoWait what

getRepoEnc :: (GitPerks m) => GitCLI m ExportEncryption
getRepoEnc = do
  sto <- asks _storage
  mgkh <- runMaybeT do
    tx <- withState do
      selectMaxAppliedTx >>= lift . toMPlus <&> fst
    (_, rh) <-
      TX.readRepoHeadFromTx sto tx
        >>= toMPlus
    toMPlus $ _repoHeadGK0 rh
  case mgkh of
    Nothing -> pure ExportPublic
    Just gkh -> do
      gk <- runExceptT (readGK0 sto gkh) >>= orThrowUser "failed to read encryption key"
      pure $ ExportPrivateGK gk

pKey :: GitPerks m => Parser (GitCLI m ())
pKey = hsubparser (  command "show"   (info pKeyShow  (progDesc "show current key"))
                  <> command "update" (info pKeyUpdate (progDesc "update current key"))
                  )
        <|> pKeyShow

pKeyShow :: GitPerks m => Parser (GitCLI m ())
pKeyShow = do
  full <- flag False True (long "full" <> help "show full key info")
  pure do
    sto <- asks _storage
    void $ runMaybeT do

      tx  <- withState do
               selectMaxAppliedTx >>= lift  . toMPlus <&> fst

      (_,rh) <- TX.readRepoHeadFromTx sto tx
                  >>= toMPlus

      gkh <- toMPlus (_repoHeadGK0 rh)

      if not full then do
        liftIO $ print $ pretty gkh
      else do
        gk <- runExceptT (readGK0 sto gkh) >>= toMPlus
        liftIO $ print $ ";; group key" <+> pretty gkh <> line <> line <> pretty gk

pKeyUpdate :: GitPerks m => Parser (GitCLI m ())
pKeyUpdate = do
  rlog <- argument pRefLogId (metavar "REFLOG-KEY")
  fn <- strArgument (metavar "GROUP-KEY-FILE")
  pure do
    gk <- loadGK0FromFile  fn
            `orDie` "can not load group key or invalid format"

    sto <- asks _storage

    gh <- writeAsMerkle sto (serialise gk) <&> HashRef

    added <- withState $ runMaybeT do
                (tx,_) <- lift selectMaxAppliedTx >>= toMPlus
                lift do
                  insertNewGK0 rlog tx gh
                  commitAll
                  pure gh

    case added of
      Nothing -> liftIO $ putStrLn "not added" >> exitFailure
      Just x  -> liftIO $ print $ pretty x


pTrack :: GitPerks m => Parser (GitCLI m ())
pTrack = hsubparser (   command "send-repo-notify" (info pSendRepoNotify (progDesc "sends repository notification"))
                     <> command "show-repo-notify" (info pShowRepoNotify (progDesc "shows repository notification"))
                     <> command "gen-repo-index"   (info pGenRepoIndex (progDesc "generates repo index tx"))
                    )

pSendRepoNotify :: GitPerks m => Parser (GitCLI m ())
pSendRepoNotify = do
  dry <- flag False True (short 'n' <> long "dry" <> help "don't post anything")
  notifyChan <- argument pRefChanId (metavar "CHANNEL-KEY")
  pure do
    notice $ "test send-repo-notify" <+> pretty (AsBase58 notifyChan)
    -- откуда мы берём ссылку, которую постим? их много.

    lwws <- withState selectAllLww

    -- берём те, для которых у нас есть приватный ключ (наши)
    creds <- catMaybes <$> runKeymanClient do
                for lwws $ \(lwref,_,_) -> do
                  loadCredentials (coerce @_ @(PubKey 'Sign 'HBS2Basic) lwref)

    sto <- getStorage
    rchanAPI <- asks _refChanAPI

    hd <- getRefChanHead @L4Proto sto (RefChanHeadKey notifyChan)
             `orDie` "refchan head not found"

    let notifiers = view refChanHeadNotifiers hd & HS.toList

    -- откуда мы берём ключ, которым подписываем?
    -- ищем тоже в кеймане, берём тот, у которого выше weight
    foundKey <- runKeymanClient (
                  S.head_ do
                     for notifiers $ \n -> do
                      lift (loadCredentials n) >>= maybe none S.yield
                      ) `orDie` "signing key not found"

    for_ creds $ \c -> do
      let lww = LWWRefKey @'HBS2Basic (view peerSignPk c)
      let lwwSk = view peerSignSk c
      let tx = makeNotificationTx @'HBS2Basic (NotifyCredentials foundKey) lww lwwSk Nothing

      notice $ "about to publish lwwref index entry:"
        <+> pretty (AsBase58 $ view peerSignPk c)

      -- как мы постим ссылку
      unless dry do
        void $ callService @RpcRefChanNotify rchanAPI (notifyChan, tx)

    -- кто парсит ссылку и помещает в рефчан


pShowRepoNotify :: GitPerks m => Parser (GitCLI m ())
pShowRepoNotify = do
  href <- argument pHashRef (metavar "HASH")
  pure do
    sto <- asks _storage

    box <- getBlock sto (coerce href)
            `orDie` "tx not found"
           <&> deserialiseOrFail @(RefChanNotify L4Proto)
           >>= orThrowUser "malformed announce tx 1"
           >>= \case
                  Notify _ box -> pure box
                  _            -> throwIO (userError "malformed announce tx 2")

    ann <- runExceptT (unpackNotificationTx box)
            >>= either (error . show) pure

    liftIO $ print $ pretty ann


pGenRepoIndex :: GitPerks m => Parser (GitCLI m ())
pGenRepoIndex = do
  what <- argument pLwwKey (metavar "LWWREF")
  pure do
    hd <- withState $ selectRepoIndexEntryFor what
            >>= orThrowUser "no decent repo head data found"

    seq <- getEpoch
    let tx = GitIndexTx what seq (GitIndexRepoDefine hd)
    liftIO $ LBS.putStr (serialise tx)


script :: GitPerks m => Parser (GitCLI m ())
script = do
  rest <- many (strArgument (metavar "CLI") )
  pure do
    cli <- parseTop (unlines $ unwords <$> splitForms rest)
           & either  (error.show) pure
    void $ runScript cli

runScriptArgs :: GitPerks m => [String] -> GitCLI m ()
runScriptArgs cli = do
    cli <- parseTop (unlines $ unwords <$> splitForms cli)
           & either  (error.show) pure
    void $ runScript cli

runScript :: GitPerks m => [Syntax C] -> GitCLI m ()
runScript syn = void $ run theDict syn

quit :: MonadIO m => m ()
quit = liftIO exitSuccess

theDict :: forall m . ( GitPerks m
                      -- , HasTimeLimits UNIX (ServiceProto MyRPC UNIX) m
                      ) => Dict C (GitCLI m)
theDict = do
  makeDict @C do
    -- TODO: write-man-entries
    myHelpEntry
    myEntries

  where

    myHelpEntry = do
        entry $ bindMatch "help" $ nil_ $ \case
          HelpEntryBound what -> do
            helpEntry what
            quit

          [StringLike s] -> helpList False (Just s) >> quit

          _ -> helpList False Nothing >> quit


    myEntries = do
        entry $ bindMatch "lww:fsck" $ nil_ $ \case
          [StringLike puk] -> lift do
            lww <- orThrowUser "bad lwwref key" (fromStringMay puk)
            git <- Git.gitDir >>= orThrowUser "not a git dir"
            fsckRepo lww

        entry $ bindMatch "remote:hbs2:show" $ nil_ $ \case
          _ -> do
            -- TODO: move-to-HBS2.Local.CLI
            remotes <- Git.gitListHBS2Remotes
            let w = fmap (length.fst) remotes & maximumDef 8
            for_ remotes $ \(n,r) -> do
              liftIO $ print $ fill w (pretty n) <+> pretty (AsBase58 r)

        entry $ bindMatch "remote:refs:show" $ nil_ $ \args -> lift do

          sto <- getStorage

          remotez <- Git.gitListHBS2Remotes <&> HM.fromList
          let zetomer = HM.fromList [ (v,k) | (k,v) <-   HM.toList remotez ]

          lww <- case args of

                  [ StringLike x ] | x `HM.member` remotez  -> do
                    orThrowUser ( "remote" <+> pretty x <+> "not found" ) (HM.lookup x remotez)

                  [ SignPubKeyLike what ] | what `HM.member` zetomer -> do
                    pure what

                  _ -> throwIO $ BadFormException @C nil

          void $ runMaybeT do
            rh <- readActualRepoHeadFor (LWWRefKey lww)
                    >>= toMPlus

            liftIO $ print $ vcat (fmap formatRef (view repoHeadRefs rh))


main :: IO ()
main = do
  (o, action) <- customExecParser (prefs showHelpOnError) $
                      O.info (liftA2 (,) globalOptions commands <**> helper)
                        ( fullDesc
                           <> header "hbs2-git"
                           <> progDesc "hbs2-git"
                        )

  runGitCLI o action


