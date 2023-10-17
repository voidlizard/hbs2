module HBS2Git.Tools where

import HBS2.Prelude.Plated
import HBS2.Base58
import HBS2.Net.Proto.Types
import HBS2.Net.Auth.Credentials
import HBS2.Data.Types.Refs (HashRef)
import HBS2.OrDie

import HBS2.System.Logger.Simple

import HBS2Git.Types
import HBS2Git.App

import HBS2.Git.Local.CLI
import HBS2.Git.Types
import HBS2Git.Import (importRefLogNew)
import HBS2Git.Config
import HBS2Git.State
import HBS2Git.PrettyStuff

import Data.HashMap.Strict qualified as HashMap
import Data.ByteString.Char8 qualified as BS8
import Control.Monad.Trans.Maybe
import Data.Text qualified as Text
import Data.Traversable
import Data.Maybe
import Prettyprinter.Render.Terminal
import Control.Monad.IO.Unlift
import Control.Monad.Catch (MonadCatch,MonadThrow,MonadMask)
import Data.Generics.Product (field)
import Lens.Micro.Platform
import System.FilePath
import System.Directory
import System.Process.Typed
import Text.InterpolatedString.Perl6 (qc)
import System.FilePattern.Directory
import System.IO.Temp
import System.IO (stdout,stderr)

import UnliftIO

data EncryptionOpts =
  EncryptionOpts
  { encryptKeyring :: FilePath
  , encryptKey     :: PubKey 'Encrypt HBS2Basic
  }
  deriving stock Generic

data NewRepoOpts =
  NewRepoOpts
  { newRepoKeyring      :: Maybe FilePath
  , newRepoEncryption   :: Maybe (PubKey 'Encrypt HBS2Basic, FilePath)
  }
  deriving stock (Generic)

data AsRemoteEntry = AsRemoteEntry
  { remoteName :: Text,
    remoteURL :: Text,
    remoteRefValue :: Maybe HashRef
  }

remoteNameColWidth :: Int
remoteNameColWidth = 16

remoteURLColWidth :: Int
remoteURLColWidth = 51

remoteRefValueColWidth :: Int
remoteRefValueColWidth = 44

instance Pretty AsRemoteEntry where
  pretty (AsRemoteEntry {..}) =
    fill remoteNameColWidth (pretty remoteName)
      <+> fill remoteURLColWidth (pretty remoteURL)
      <+> fill remoteRefValueColWidth (maybe "-" pretty remoteRefValue)

hbs2Prefix :: Text
hbs2Prefix = "hbs2://"

-- TODO: backlog-list-refs-all-option
--  сделать опцию --all которая выведет
--  все известные ref-ы из стейта.
--  Сейчас выводятся только локальные

runListRefs :: (MonadIO m, HasStorage (App m)) => App m ()
runListRefs = do
  refs <- gitGetRemotes <&> filter isHbs2
  remoteEntries <-
    forM
      refs
      ( \(name, url) -> do
          refVal <- getRefVal url
          pure $
            AsRemoteEntry
              { remoteName = name,
                remoteURL = url,
                remoteRefValue = refVal
              }
      )
  let header =
        fill remoteNameColWidth (green "Name")
          <+> fill remoteURLColWidth (green "URL")
          <+> fill remoteRefValueColWidth (green "Reference value")
  liftIO $ putDoc $ header <> line
  liftIO $ putDoc $ vcat $ pretty <$> remoteEntries
  where
    isHbs2 (_, b) = Text.isPrefixOf hbs2Prefix b

runToolsScan :: (MonadUnliftIO m,MonadCatch m,MonadMask m,HasStorage (App m)) => RepoRef -> App m ()
runToolsScan ref = do
  trace $ "runToolsScan" <+> pretty ref
  importRefLogNew True ref
  shutUp
  pure ()

runToolsGetRefs :: (MonadUnliftIO m,MonadCatch m,MonadMask m) => RepoRef -> App m ()
runToolsGetRefs ref = do
  db <- makeDbPath ref >>= dbEnv
  refs <- withDB db stateGetActualRefs
  let rh = RepoHead Nothing (HashMap.fromList refs)
  hPrint stdout $ pretty (AsGitRefsFile rh)
  shutUp

getRefVal :: (MonadIO m, HasStorage m) => Text -> m (Maybe HashRef)
getRefVal url =
  case Text.stripPrefix hbs2Prefix url of
    Nothing -> do
      liftIO $ print $ pretty "wrong URL format" <+> pretty url
      pure Nothing
    Just refStr -> case fromStringMay $ Text.unpack refStr of
      Nothing -> do
        liftIO $ print $ pretty "can't parse ref" <+> pretty refStr
        pure Nothing
      Just ref -> do
        mRefVal <- readRef ref
        case mRefVal of
          Nothing -> do
            liftIO $ print $ pretty "readRef error" <+> pretty ref
            pure Nothing
          Just v -> pure $ Just v



runInitRepo :: (MonadUnliftIO m, MonadThrow m, MonadCatch m) => NewRepoOpts -> m ()
runInitRepo = runInitInteractive

runInitInteractive :: (MonadUnliftIO m, MonadThrow m, MonadCatch m) => NewRepoOpts -> m ()
runInitInteractive opts = do

  hSetBuffering stdin  NoBuffering
  hSetBuffering stdout LineBuffering

  conf <- configPath ""
           `catch`
            (\NoWorkDirException -> do
                liftIO $ hPutDoc stderr $ red "init:"
                                           <+> "No git working directory."
                                           <+> yellow "Run" <+> "'git init'" <+> "first"
                                           <> line
                die "nope"
            )

  rpc <- (Just <$> detectRPC False)
           `catch`
            (\NoRPCException -> do
                liftIO $ hPutDoc stderr $ yellow "init:"
                                           <+> "No RPC found."
                                           <+> "Perhaps, hbs2-peer is down"
                                           <> line
                                           <> "Okay, you may add it later"
                                           <> line
                pure Nothing
            )

  let confFile = conf </> "config"

  liftIO $ createDirectoryIfMissing True conf

  confHere <- liftIO $ doesFileExist confFile

  when confHere do
    liftIO $ hPutDoc stdout $ yellow "Config"
                           <+> pretty confFile
                           <+> yellow "is already here."
                           <+> "Continue? [y/n]: "

    liftIO $ hFlush stdout

    y <- liftIO getChar

    unless (y `elem` "'yY ") do
      exitFailure

  liftIO $ hPutStrLn stdout ""

  puk <- case view (field @"newRepoKeyring") opts of
    Just kr -> liftIO do
      addKeyring confFile kr

    Nothing -> do
      tmp <- liftIO $ emptyTempFile "." "reflog.key"

      code <- runProcess (shell [qc|hbs2 keyring-new > {tmp}|])

      unless (code == ExitSuccess) do
        liftIO $ hPutDoc stderr $ red "init:" <+> "can't generate new keyring file" <> line
        die "nope"

      addKeyring confFile tmp


  encrypt <- if isJust (view (field @"newRepoEncryption") opts) then do
               pure True
             else do
               liftIO $ hPutDoc stdout $ yellow "Make reflog" <+> pretty (AsBase58 puk)
                                         <+> "encrypted?"
                                         <+> "[y/n]: "

               liftIO $ hFlush stdout

               y2 <- liftIO getChar

               liftIO $ hPutStrLn stdout ""

               pure $ y2 `elem` "'yY "

  when encrypt do
    let enc = view (field @"newRepoEncryption") opts

    case enc of
      Just (epuk, fp') -> do
        fp <- liftIO $ makeAbsolute fp'
        addDecrypt confFile fp
        addEncrypted confFile puk epuk

      Nothing -> do
        tmp <- liftIO $ emptyTempFile "." "cred.key"

        code <- runProcess (shell [qc|hbs2 keyring-new -n1 > {tmp}|])

        fp <- liftIO $ makeAbsolute tmp

        ke <- readPubKeyFrom fp
        addDecrypt confFile fp
        addEncrypted confFile puk ke

        pure ()

    pure ()

  liftIO $ hPutDoc stderr $ green "succeed!" <> line <> line
  liftIO $ readFile confFile >>= putStrLn

  where

    readPubKeyFrom fp = do
      bs <- liftIO $ BS8.readFile fp
      cred <- pure (parseCredentials @HBS2Basic (AsCredFile bs))
               `orDie` [qc|invalid credentials file {fp}|]

      pure (view krPk <$> headMay (view peerKeyring cred))
               `orDie` [qc|invalid credentials file {fp}|]

    addEncrypted fn puk enc = liftIO do

      appendFile fn $ show $
        line
        <> brackets (  "encrypted" <+> dquotes (pretty (AsBase58 puk))
                      <> line
                      <> parens ("ttl" <+> pretty 864000)
                      <> line
                      <> parens ("owner" <+> dquotes (pretty (AsBase58 enc)))
                      <> line
                    )
        <> line

      pure ()

    addDecrypt fn kf = liftIO do
      appendFile fn $ show
                    $ ";; this keyring is a SECRET for encryption/decryption"
                       <> line
                       <> ";; move it to a private/safe place"
                       <> line
                       <> "decrypt" <+> dquotes (pretty kf)
                       <> line

    addKeyring fn kr = liftIO do
      fp <- makeAbsolute kr

      bs <- BS8.readFile fp
      cred <- pure (parseCredentials @HBS2Basic (AsCredFile bs))
               `orDie` [qc|invalid credentials file {fp}|]

      let puk = view peerSignPk cred

      liftIO $ hPutDoc stdout $ yellow "Adding reflog" <+> pretty (AsBase58 puk) <> line
      appendFile fn  $ show $ ";; SECRET keyring for reflog" <+> pretty (AsBase58 puk) <> line
      appendFile fn  $ show $ ";; move it to a private/safe place" <> line
      appendFile fn  $ show line
      appendFile fn  $ show $ "keyring" <+> dquotes (pretty fp) <> line <> line

      pure puk


