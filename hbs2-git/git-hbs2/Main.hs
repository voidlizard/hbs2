{-# Language UndecidableInstances #-}
module Main where

import HBS2.Git.Client.Prelude hiding (info)
import HBS2.Git.Client.App
import HBS2.Git.Client.Export
import HBS2.Git.Client.Import
import HBS2.Git.Client.State

import HBS2.Git.Data.RefLog
import HBS2.Git.Local.CLI qualified as Git
import HBS2.Git.Data.Tx.Git qualified as TX
import HBS2.Git.Data.Tx.Git (RepoHead(..))
import HBS2.Git.Data.LWWBlock
import HBS2.Git.Data.GK

import HBS2.Storage.Operations.ByteString

import Options.Applicative as O
import Data.ByteString.Lazy qualified as LBS

import System.Exit

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
  hsubparser (  command "export"  (info pExport  (progDesc "export repo to hbs2-git"))
             <> command "import" (info pImport  (progDesc "import repo from reflog"))
             <> command "key"    (info pKey     (progDesc "key management"))
             <> command "track"  (info pTrack   (progDesc "track tools"))
             <> command "tools"  (info pTools   (progDesc "misc tools"))
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
  pure do
    sto <- asks _storage
    void $ runMaybeT do

      tx  <- withState do
               selectMaxAppliedTx >>= lift  . toMPlus <&> fst

      rh <- TX.readRepoHeadFromTx sto tx >>= toMPlus

      liftIO $ print $ vcat (fmap formatRef (_repoHeadRefs rh))


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

      rh <- TX.readRepoHeadFromTx sto tx
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
pTrack = hsubparser (  command "send-repo-notify" (info pSendRepoNotify (progDesc "sends repository notification"))
                    )

pSendRepoNotify :: GitPerks m => Parser (GitCLI m ())
pSendRepoNotify = do
  notifyChan <- argument pRefChanId (metavar "CHANNEL-KEY")
  pure do
    notice "wip"
    pure ()

main :: IO ()
main = do
  (o, action) <- customExecParser (prefs showHelpOnError) $
                      O.info (liftA2 (,) globalOptions commands <**> helper)
                        ( fullDesc
                           <> header "hbs2-git"
                           <> progDesc "hbs2-git"
                        )

  runGitCLI o action


