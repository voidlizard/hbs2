module Main where

import HBS2.Prelude.Plated
import HBS2.Data.Types.Refs
import HBS2.Base58
import HBS2.OrDie
import HBS2.Git.Types

import HBS2.System.Logger.Simple

import HBS2Git.Types(traceTime)
import HBS2Git.App
import HBS2Git.State
import HBS2Git.Import
import HBS2.Git.Local.CLI

import HBS2Git.Export (runExport)

import GitRemoteTypes
import GitRemotePush

import Control.Concurrent.STM
import Control.Monad.Reader
import Data.Attoparsec.Text hiding (try)
import Data.Attoparsec.Text qualified as Atto
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Foldable
import Data.Functor
import Data.Function ((&))
import Data.HashMap.Strict qualified as HashMap
import Data.Maybe
import Data.Text qualified as Text
import Data.List qualified as List
import System.Environment
import System.Posix.Signals
import Text.InterpolatedString.Perl6 (qc)
import UnliftIO.IO as UIO
import Control.Monad.Catch
import Control.Monad.Trans.Resource


send :: MonadIO m => BS.ByteString -> m ()
send = liftIO . BS.hPutStr stdout

sendLn :: MonadIO m => BS.ByteString -> m ()
sendLn s = do
  trace $ "sendLn" <+> pretty (show s)
  liftIO $ BS.hPutStrLn stdout s

sendEol :: MonadIO m => m ()
sendEol = liftIO $ BS.hPutStrLn stdout "" >> hFlush stdout

receive :: MonadIO m => m BS.ByteString
receive = liftIO $ BS.hGetLine stdin

done :: MonadIO m => m Bool
done = UIO.hIsEOF stdin

parseRepoURL :: String -> Maybe HashRef
parseRepoURL url' = either (const Nothing) Just (parseOnly p url)
  where
    url = Text.pack url'
    p = do
      _ <- string "hbs2://"
      topic'  <- Atto.manyTill' anyChar endOfInput
      let topic = BS.unpack <$> fromBase58 (BS.pack topic')
      maybe (fail "invalid url") (pure . fromString) topic


capabilities :: BS.ByteString
capabilities = BS.unlines ["push","fetch"]


guessHead :: GitRef -> Integer
guessHead = \case
  "refs/heads/master" -> 0
  "refs/heads/main"   -> 0
  _                   -> 1

loop :: forall m . ( MonadIO m
                   , MonadCatch m
                   , MonadUnliftIO m
                   , MonadMask m
                   , HasProgress (RunWithConfig (GitRemoteApp m))
                   ) => [String] -> GitRemoteApp m ()
loop args = do


  -- setLogging @TRACE tracePrefix

  trace $ "args:" <+> pretty args

  let ref' = case args of
              [_, s] -> Text.stripPrefix "hbs2://" (Text.pack s) <&> fromString @RepoRef . Text.unpack
              _      -> Nothing

  ref <- pure ref' `orDie` ("invalid reference: "  <> show args)

  trace $ "ref:" <+> pretty ref

  dbPath <- makeDbPath ref

  trace $ "dbPath:" <+> pretty dbPath

  db <- dbEnv dbPath

  -- TODO: hbs2-peer-fetch-reference-and-wait

  checkRef <- readRef ref <&> isJust

  unless checkRef do
    warn $ "reference" <+> pretty ref <+> "missing"
    warn "trying to init reference --- may be it's ours"
    liftIO $ runApp NoLog (runExport Nothing ref)

  refs <- withDB db stateGetActualRefs

  let heads = [ h | h@GitHash{} <- universeBi refs ]

  missed <- try (mapM (gitReadObject Nothing) heads) <&> either (\(_::SomeException) -> True) (const False)

  let force = missed || List.null heads

  debug $ "THIS MIGHT BE CLONE!" <+> pretty force

  -- sync state first
  traceTime "TIMING: importRefLogNew" $ importRefLogNew force ref

  refsNew <- withDB db stateGetActualRefs
  let possibleHead = listToMaybe $ List.take 1 $ List.sortOn guessHead (fmap fst refsNew)

  let hd = refsNew & LBS.pack . show
                              . pretty
                              . AsGitRefsFile
                              . RepoHead possibleHead
                              . HashMap.fromList

  batch <- liftIO $ newTVarIO False

  fix \next -> do

    eof <- done

    when eof do
      exitFailure

    s <- receive

    let str = BS.unwords (BS.words s)
    let cmd = BS.words str

    isBatch <- liftIO $ readTVarIO batch

    case cmd of
      [] -> do
        liftIO $ atomically $ writeTVar batch False
        sendEol
        when isBatch next
        -- unless isBatch do

      ["capabilities"] -> do
          trace $ "send capabilities" <+> pretty (BS.unpack capabilities)
          send capabilities >> sendEol
          next

      ["list"] -> do
        importRefLogNew False ref
        for_ (LBS.lines hd) (sendLn . LBS.toStrict)
        sendEol
        next

      ["list","for-push"] -> do
        importRefLogNew False ref
        for_ (LBS.lines hd) (sendLn . LBS.toStrict)
        sendEol
        next

      ["fetch", sha1, x] -> do
        importRefLogNew False ref
        trace $ "fetch" <+> pretty (BS.unpack sha1) <+> pretty (BS.unpack x)
        liftIO $ atomically $ writeTVar batch True
        -- sendEol
        next

      ["push", rr] -> do
        let bra = BS.split ':' rr
        let pu = fmap (fromString' . BS.unpack) bra
        liftIO $ atomically $ writeTVar batch True
        -- debug $ "FUCKING PUSH" <> viaShow rr <+> pretty pu
        -- shutUp
        pushed <- push ref pu
        case pushed of
          Nothing  -> hPrint stderr "oopsie!" >> sendEol >> shutUp
          Just re -> sendLn [qc|ok {pretty re}|]
        next

      other -> die $ show other

  where
    fromString' "" = Nothing
    fromString' x  = Just $ fromString x

main :: IO ()
main = do

  hSetBuffering stdin  NoBuffering
  hSetBuffering stdout LineBuffering

  doTrace <- lookupEnv "HBS2TRACE" <&> isJust

  when doTrace do
    setLogging @DEBUG  debugPrefix
    setLogging @TRACE  tracePrefix

  setLogging @NOTICE noticePrefix
  setLogging @ERROR  errorPrefix
  setLogging @WARN   warnPrefix
  setLogging @INFO   infoPrefix

  args <- getArgs

  void $ installHandler sigPIPE Ignore Nothing

  env <- RemoteEnv <$> detectHBS2PeerCatAPI
                   <*> detectHBS2PeerSizeAPI
                   <*> detectHBS2PeerPutAPI
                   <*> detectHBS2PeerRefLogGetAPI
                   <*> liftIO (newTVarIO mempty)

  runRemoteM env do
    loop args

  shutUp

  hPutStrLn stdout ""
  hPutStrLn stderr ""

