module Main where

import HBS2.Prelude.Plated
import HBS2.Data.Types.Refs
import HBS2.Base58
import HBS2.OrDie
import HBS2.Git.Types

import HBS2.System.Logger.Simple

import HBS2Git.App
import HBS2Git.State
import HBS2Git.Import
import HBS2Git.Evolve
import HBS2.Git.Local.CLI

import HBS2Git.Export (runExport)

import HBS2Git.Config as Config
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
import Lens.Micro.Platform


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


getGlobalOptionFromURL :: HasGlobalOptions m => [String] -> m ()
getGlobalOptionFromURL args = do

  case args of
    [_, ss] -> do
      let (_, suff) = Text.breakOn "?" (Text.pack ss)
                      & over _2 (Text.dropWhile (== '?'))
                      & over _2 (Text.splitOn "&")
                      & over _2 (fmap (over _2 (Text.dropWhile (=='=')) . Text.break (== '=')))
                      & over _2 (filter (\(k,_) -> k /= ""))

      forM_ suff $ \(k,v) -> do
        addGlobalOption (Text.unpack k) (Text.unpack v)

    _ -> pure ()

loop :: forall m . ( MonadIO m
                   , MonadCatch m
                   , MonadUnliftIO m
                   , MonadMask m
                   , HasProgress m
                   , HasConf m
                   , HasStorage m
                   , HasRPC m
                   , HasRefCredentials m
                   , HasEncryptionKeys m
                   , HasGlobalOptions m
                   ) => [String] -> m ()
loop args = do

  trace $ "args:" <+> pretty args

  ref <- case args of
            [_, ss] -> do
              let (s, _) = Text.breakOn "?" (Text.pack ss)

              let r = Text.stripPrefix "hbs2://" s <&> fromString @RepoRef . Text.unpack

              pure r `orDie` [qc|bad reference {args}||]

            _  -> do
                die [qc|bad reference: {args}|]

  trace $ "ref:" <+> pretty ref

  dbPath <- makeDbPath ref

  trace $ "dbPath:" <+> pretty dbPath

  db <- dbEnv dbPath

  -- TODO: hbs2-peer-fetch-reference-and-wait

  checkRef <- readRef ref <&> isJust

  let getHeads upd = do
        when upd do importRefLogNew False ref
        refsNew <- withDB db stateGetActualRefs
        let possibleHead = listToMaybe $ List.take 1 $ List.sortOn guessHead (fmap fst refsNew)

        let hd = refsNew & LBS.pack . show
                                    . pretty
                                    . AsGitRefsFile
                                    . RepoHead possibleHead
                                    . HashMap.fromList
        pure hd


  hd <- getHeads True

  refs <- withDB db stateGetActualRefs

  let heads = [ h | h@GitHash{} <- universeBi refs ]

  missed <- try (mapM (gitReadObject Nothing) heads) <&> either (\(_::SomeException) -> True) (const False)

  let force = missed || List.null heads

  when force do
  -- sync state first
    traceTime "TIMING: importRefLogNew" $ importRefLogNew True ref

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
        for_ (LBS.lines hd) (sendLn . LBS.toStrict)
        sendEol
        next

      ["list","for-push"] -> do
        for_ (LBS.lines hd) (sendLn . LBS.toStrict)
        sendEol
        next

      ["fetch", sha1, x] -> do
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


    shutUp

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

  evolve

  (_, syn) <- Config.configInit

  runWithRPC $ \rpc -> do
    env <- RemoteEnv <$> liftIO (newTVarIO mempty)
                     <*> liftIO (newTVarIO mempty)
                     <*> liftIO (newTVarIO mempty)
                     <*> pure rpc

    runRemoteM env do
      runWithConfig syn $ do
        getGlobalOptionFromURL args
        loadCredentials mempty
        loadKeys
        loop args

  shutUp

  hPutStrLn stdout ""
  hPutStrLn stderr ""

