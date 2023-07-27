module Main where

import HBS2.Base58
import HBS2.Data.Detect
import HBS2.Data.Types
import HBS2.Defaults
import HBS2.Merkle
import HBS2.Net.Proto.Types
import HBS2.Net.Auth.AccessKey
import HBS2.Net.Auth.Credentials
import HBS2.Net.Proto.Definition()
import HBS2.Prelude.Plated
import HBS2.Storage.Simple
import HBS2.Storage.Simple.Extra
import HBS2.OrDie


import HBS2.System.Logger.Simple hiding (info)

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Trans.Maybe
import Crypto.Saltine.Core.Box qualified as Encrypt
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy (ByteString)
import Data.Function
import Data.Functor
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Monoid qualified as Monoid
import Options.Applicative
import System.Directory
import Data.Maybe
import Lens.Micro.Platform
-- import System.FilePath.Posix
import System.IO
import System.Exit

import Codec.Serialise

import Streaming.Prelude qualified as S
-- import Streaming qualified as S


tracePrefix :: SetLoggerEntry
tracePrefix  = logPrefix "[trace] "

debugPrefix :: SetLoggerEntry
debugPrefix  = logPrefix "[debug] "

errorPrefix :: SetLoggerEntry
errorPrefix  = logPrefix "[error] "

warnPrefix :: SetLoggerEntry
warnPrefix   = logPrefix "[warn] "

noticePrefix :: SetLoggerEntry
noticePrefix = logPrefix "[notice] "


newtype CommonOpts =
  CommonOpts
  { _coPref :: Maybe StoragePrefix
  }
 deriving stock (Data)

newtype OptInputFile = OptInputFile { unOptFile :: FilePath }
                       deriving newtype (Eq,Ord,IsString)
                       deriving stock (Data)

newtype CatHashesOnly = CatHashesOnly Bool
                        deriving newtype (Eq,Ord,Pretty)
                        deriving stock (Data,Generic)


newtype OptKeyringFile = OptKeyringFile { unOptKeyringFile :: FilePath }
                       deriving newtype (Eq,Ord,IsString)
                       deriving stock (Data)

newtype OptGroupkeyFile = OptGroupkeyFile { unOptGroupkeyFile :: FilePath }
                       deriving newtype (Eq,Ord,IsString)
                       deriving stock (Data)

newtype OptInit = OptInit { fromOptInit :: Bool }
                  deriving newtype (Eq,Ord,Pretty)
                  deriving stock (Data,Generic)

data StoreOpts =
  StoreOpts
  {  storeInit      :: Maybe OptInit
  ,  storeInputFile :: Maybe OptInputFile
  ,  storeGroupkeyFile :: Maybe OptGroupkeyFile
  ,  storeBase58Meta :: Maybe String
  }
  deriving stock (Data)

data CatOpts =
  CatOpts
  { catMerkleHash :: Maybe MerkleHash
  , catHashesOnly :: Maybe CatHashesOnly
  , catPathToKeyring :: Maybe OptKeyringFile
  , catRaw        :: Maybe Bool
  }
  deriving stock (Data)

newtype HashOpts =
 HashOpts
  { hashFp  :: FilePath
  }
  deriving stock (Data)

newtype NewRefOpts =
  NewRefOpts
  { newRefMerkle :: Bool
  }
  deriving stock (Data)


runHash :: HashOpts -> SimpleStorage HbSync -> IO ()
runHash opts _ = do
  withBinaryFile (hashFp opts) ReadMode $ \h -> do
    LBS.hGetContents h >>= print . pretty . hashObject @HbSync

runCat :: forall s . ForHBS2Basic s => CatOpts -> SimpleStorage HbSync -> IO ()

runCat opts ss | catRaw opts == Just True = do

  let mhash' = uniLastMay @MerkleHash opts <&> fromMerkleHash

  maybe1 mhash' exitFailure $ \h -> do
    obj <- getBlock ss h
    maybe exitFailure LBS.putStr obj
    exitSuccess

runCat opts ss = do

  let honly = or [ x | CatHashesOnly x <- universeBi opts  ]

  void $ runMaybeT $ do

    mhash <- MaybeT $ pure $ uniLastMay @MerkleHash opts <&> fromMerkleHash

    obj <- MaybeT $ getBlock ss mhash

    let q = tryDetect mhash obj

    liftIO $ do

      let stepInside hr =
            case hr of
              Left hx -> void $ hPrint stderr $ "missed block:" <+> pretty hx
              Right (hrr :: [HashRef]) -> do
                 forM_ hrr $ \(HashRef hx) -> do
                   if honly then do
                     print $ pretty hx
                   else do
                     mblk <- getBlock ss hx
                     case mblk of
                       Nothing  -> die $ show $ "missed block: " <+> pretty hx
                       Just blk -> LBS.putStr blk

      let walk h = walkMerkle h (getBlock ss) stepInside

      -- FIXME: switch-to-deep-scan
      -- TODO: to-the-library
      let walkAnn :: MTreeAnn [HashRef] -> IO ()
          walkAnn ann = do
            bprocess :: Hash HbSync -> ByteString -> IO ByteString <- case (_mtaCrypt ann) of
              NullEncryption -> pure (const pure)
              CryptAccessKeyNaClAsymm crypth -> do

                  keyringFile <- pure (uniLastMay @OptKeyringFile opts <&> unOptKeyringFile)
                      `orDie` "block encrypted. keyring required"
                  s <- BS.readFile keyringFile
                  ourKeys <- _peerKeyring
                      <$> pure (parseCredentials @s (AsCredFile s))
                      `orDie` "bad keyring file"

                  blkc <- getBlock ss crypth `orDie` (show $ "missed block: " <+> pretty crypth)
                  recipientKeys :: [(PubKey 'Encrypt s, EncryptedBox)]
                    <- pure (deserialiseMay blkc)
                      `orDie` "can not deserialise access key"

                  (ourkr, box)
                      <- pure (Monoid.getFirst
                                (foldMap (\kr@(KeyringEntry pk _ _)
                                           -> Monoid.First ((kr, )
                                          <$> Map.lookup pk (Map.fromList recipientKeys)))
                                         ourKeys))
                      `orDie` "no available recipient key"

                  kr <- pure (openEncryptedKey box ourkr)
                      `orDie` "can not open sealed secret key with our key"

                  pure $ \hx blk ->
                      pure ((fmap LBS.fromStrict . Encrypt.boxSealOpen (_krPk kr) (_krSk kr) . LBS.toStrict) blk)
                      `orDie` (show $ "can not decode block: " <+> pretty hx)

            walkMerkleTree (_mtaTree ann) (getBlock ss) $ \(hr :: Either (Hash HbSync) [HashRef]) -> do
              case hr of
                Left hx -> void $ hPrint stderr $ "missed block:" <+> pretty hx
                Right (hrr :: [HashRef]) -> do
                  forM_ hrr $ \(HashRef hx) -> do
                    if honly then do
                      print $ pretty hx
                    else do
                      blk <- getBlock ss hx `orDie` (show $ "missed block: " <+> pretty hx)
                      LBS.putStr =<< bprocess hx blk

      case q of
        Blob h -> getBlock ss h >>= maybe (die "blob not found") LBS.putStr
        Merkle t -> walkMerkleTree t (getBlock ss) stepInside

        MerkleAnn ann -> walkAnn ann

        -- FIXME: what-if-multiple-seq-ref-?
        SeqRef (SequentialRef _ (AnnotatedHashRef _ h)) -> do
          walk (fromHashRef h)

        AnnRef _ -> do
          let lnk = either (error . ("Deserialise AnnotatedHashRef: " <>) . show) id $
                  deserialiseOrFail @AnnotatedHashRef obj
          let mbHead =  headMay [ h
                                | HashRefMerkle (HashRefObject (HashRef h) _) <- universeBi lnk
                                ]
          maybe (error "empty ref") walk mbHead


runStore :: StoreOpts -> SimpleStorage HbSync -> IO ()

runStore opts _ | justInit = do
  putStrLn "initialized"

  where
    justInit = maybe False fromOptInit (uniLastMay @OptInit opts)

runStore opts ss = do

  let fname = uniLastMay @OptInputFile opts
  let meta58  = storeBase58Meta opts

  handle <- maybe (pure stdin) (flip openFile ReadMode . unOptFile) fname

  case uniLastMay @OptGroupkeyFile opts of
    Nothing -> do
        root' <- putAsMerkle ss handle

        root <- case meta58 of
                  Nothing -> pure root'
                  Just s -> do
                    let metad = fromBase58 (BS8.pack s) & fromMaybe "" & BS8.unpack & fromString
                    mtree <- ((either (const Nothing) Just . deserialiseOrFail =<<) <$> getBlock ss (fromMerkleHash root'))
                        `orDie` "merkle tree was not stored properly with `putAsMerkle`"
                    mannh <- maybe (die "can not store MerkleAnn") pure
                                  =<< (putBlock ss . serialise @(MTreeAnn [HashRef])) do
                                            MTreeAnn (ShortMetadata metad) NullEncryption  mtree
                    pure (MerkleHash mannh)

        print $ "merkle-root: " <+> pretty root

    Just gkfile -> do
        gk :: GroupKey HBS2Basic
            <- (parseGroupKey . AsGroupKeyFile <$> BS.readFile (unOptGroupkeyFile gkfile))
            `orDie` "bad groupkey file"

        accKeyh <- (putBlock ss . serialise . permitted . accessKey) gk
            `orDie` "can not store access key"

        let rawChunks :: S.Stream (S.Of ByteString) IO ()
            rawChunks = readChunked handle (fromIntegral defBlockSize) -- FIXME: to settings!

            encryptedChunks :: S.Stream (S.Of ByteString) IO ()
            encryptedChunks = rawChunks
                & S.mapM (fmap LBS.fromStrict . Encrypt.boxSeal (recipientPk gk) . LBS.toStrict)

        mhash <- putAsMerkle ss encryptedChunks
        mtree <- ((either (const Nothing) Just . deserialiseOrFail =<<) <$> getBlock ss (fromMerkleHash mhash))
            `orDie` "merkle tree was not stored properly with `putAsMerkle`"

        mannh <- maybe (die "can not store MerkleAnn") pure
              =<< (putBlock ss . serialise @(MTreeAnn [HashRef])) do
            MTreeAnn NoMetaData (CryptAccessKeyNaClAsymm accKeyh) mtree

        print $ "merkle-ann-root: " <+> pretty mannh

runNewGroupkey :: forall s . (s ~ HBS2Basic) => FilePath -> IO ()
runNewGroupkey pubkeysFile = do
  s <- BS.readFile pubkeysFile
  pubkeys <- pure (parsePubKeys @s s) `orDie` "bad pubkeys file"
  keypair <- newKeypair @s Nothing
  accesskey <- AccessKeyNaClAsymm @s <$> do
      List.sort pubkeys `forM` \pk -> (pk, ) <$> mkEncryptedKey keypair pk
  print $ pretty $ AsGroupKeyFile $ AsBase58 $ GroupKeyNaClAsymm (_krPk keypair) accesskey

runNewKey :: forall s . (s ~ HBS2Basic) => IO ()
runNewKey = do
  cred <- newCredentials @s
  print $ pretty $ AsCredFile $ AsBase58 cred

runListKeys :: forall s . (s ~ HBS2Basic) => FilePath -> IO ()
runListKeys fp = do
  s <- BS.readFile fp
  cred <- pure (parseCredentials @s (AsCredFile s)) `orDie` "bad keyring file"
  print $ pretty (ListKeyringKeys cred)


runKeyAdd :: forall s . (s ~ HBS2Basic) => FilePath -> IO ()
runKeyAdd fp = do
  hPrint stderr $ "adding a key into keyring" <+> pretty fp
  s <- BS.readFile fp
  cred <- pure (parseCredentials @s (AsCredFile s)) `orDie` "bad keyring file"
  credNew <- addKeyPair Nothing cred
  print $ pretty $ AsCredFile $ AsBase58 credNew

runKeyDel :: forall s . (s ~ HBS2Basic) => String -> FilePath -> IO ()
runKeyDel n fp = do
  hPrint stderr $ "removing key" <+> pretty n <+> "from keyring" <+> pretty fp
  s <- BS.readFile fp
  cred <- pure (parseCredentials @s (AsCredFile s)) `orDie` "bad keyring file"
  credNew <- delKeyPair (AsBase58 n) cred
  print $ pretty $ AsCredFile $ AsBase58 credNew


runShowPeerKey :: forall s . ( s ~ HBS2Basic) => Maybe FilePath -> IO ()
runShowPeerKey fp = do
  handle <- maybe (pure stdin) (`openFile` ReadMode) fp
  bs <- LBS.hGet handle 4096 <&> LBS.toStrict
  let cred' = parseCredentials @s (AsCredFile bs)

  maybe1 cred' exitFailure $ \cred -> do
    print $ pretty $ AsBase58 (view peerSignPk cred)

---

deserialiseMay :: Serialise a => ByteString -> Maybe a
deserialiseMay = either (const Nothing) Just . deserialiseOrFail

mdeserialiseMay :: Serialise a => Maybe ByteString -> Maybe a
mdeserialiseMay = (deserialiseMay =<<)

---
runEnc58 :: IO ()
runEnc58 = do
   s <- LBS.hGetContents stdin  <&> LBS.toStrict
   print $ pretty (AsBase58 s)

runRefLogGet :: forall s . IsRefPubKey s => RefLogKey s -> SimpleStorage HbSync -> IO ()
runRefLogGet s ss = do
  ref' <- getRef ss s
  maybe1 ref' exitFailure $ \ref -> do
    print $ pretty ref
    exitSuccess

withStore :: Data opts => opts -> ( SimpleStorage HbSync -> IO () ) -> IO ()
withStore opts f = do

  setLogging @DEBUG  debugPrefix
  setLogging @INFO   defLog
  setLogging @ERROR  errorPrefix
  setLogging @WARN   warnPrefix
  setLogging @NOTICE noticePrefix

  setLoggingOff @TRACE

  xdg <- getXdgDirectory XdgData defStorePath <&> fromString

  let pref = uniLastDef xdg opts :: StoragePrefix
  s <- simpleStorageInit (Just pref)

  w <- replicateM 4 $ async $ simpleStorageWorker s

  f s

  simpleStorageStop s

  _ <-  waitAnyCatch w

  setLoggingOff @DEBUG
  setLoggingOff @INFO
  setLoggingOff @ERROR
  setLoggingOff @WARN
  setLoggingOff @NOTICE
  setLoggingOff @TRACE

  pure ()

main :: IO ()
main = join . customExecParser (prefs showHelpOnError) $
  info (helper <*> parser)
  (  fullDesc
  <> header "hbsync block fetch"
  <> progDesc "fetches blocks from hbsync peers"
  )
  where
    parser ::  Parser (IO ())
    parser = hsubparser (  command "store"           (info pStore (progDesc "store block"))
                        <> command "cat"             (info pCat (progDesc "cat block"))
                        <> command "hash"            (info pHash (progDesc "calculates hash"))
                        <> command "fsck"            (info pFsck (progDesc "check storage constistency"))
                        <> command "deps"            ( info pDeps (progDesc "print dependencies"))
                        <> command "del"             ( info pDel (progDesc "del block"))
                        <> command "keyring-new"     (info pNewKey (progDesc "generates a new keyring"))
                        <> command "keyring-list"    (info pKeyList (progDesc "list public keys from keyring"))
                        <> command "keyring-key-add" (info pKeyAdd (progDesc "adds a new keypair into the keyring"))
                        <> command "keyring-key-del" (info pKeyDel (progDesc "removes a keypair from the keyring"))
                        <> command "show-peer-key"   (info pShowPeerKey (progDesc "show peer key from credential file"))
                        <> command "groupkey-new"    (info pNewGroupkey (progDesc "generates a new groupkey"))
                        <> command "reflog"          (info pReflog (progDesc "reflog commands"))
                        )

    common = do
      pref <- optional $ strOption ( short 'p' <> long "prefix" <> help "storage prefix" )
      pure $ CommonOpts pref

    pStore = do
      o <- common
      file <- optional $ strArgument ( metavar "FILE" )
      init' <- optional $ flag' True ( long "init" <> help "just init storage") <&> OptInit
      groupkeyFile <- optional $ strOption ( long "groupkey" <> help "path to groupkey file" )
      b58meta <- optional $ strOption ( long "short-meta-base58" <> help "pass escaped metadata string")
      pure $ withStore o (runStore ( StoreOpts init' file (OptGroupkeyFile <$> groupkeyFile) b58meta))

    pCat = do
      o <- common
      hash  <- optional $ strArgument ( metavar "HASH" )
      onlyh <- optional $ flag' True ( short 'H' <> long "hashes-only" <> help "list only block hashes" )
      keyringFile <- optional $ strOption ( long "keyring" <> help "path to keyring file" )
      raw <- optional $ flag' True ( short 'r' <> long "raw" <> help "dump raw block" )
      pure $ withStore o $ runCat
          $ CatOpts hash (CatHashesOnly <$> onlyh) (OptKeyringFile <$> keyringFile) raw

    pNewGroupkey = do
      pubkeysFile <- strArgument ( metavar "FILE" <> help "path to a file with a list of recipient public keys" )
      pure $ runNewGroupkey pubkeysFile

    pHash = do
      o <- common
      hash  <- strArgument ( metavar "HASH" )
      pure $ withStore o $ runHash $ HashOpts hash

    pNewKey = do
      pure runNewKey

    pShowPeerKey = do
      fp <- optional $ strArgument ( metavar "FILE" )
      pure $ runShowPeerKey fp

    pKeyList = do
      f <- strArgument ( metavar "KEYRING-FILE" )
      pure (runListKeys f)

    pKeyAdd = do
      f <- strArgument ( metavar "KEYRING-FILE" )
      pure (runKeyAdd f)


    pKeyDel = do
      s <- strArgument ( metavar "PUB-KEY-BASE58" )
      f <- strArgument ( metavar "KEYRING-FILE" )
      pure (runKeyDel s f)

    pReflog = hsubparser ( command "get" (info pRefLogGet (progDesc "get reflog root") )  )

    -- FIXME: only-for-hbs2-basic-encryption
    pRefLogGet = do
      o <- common
      reflogs <- strArgument ( metavar "REFLOG" )
      pure $ withStore o (runRefLogGet @HBS2Basic reflogs)

    pFsck = do
      o <- common
      pure $ withStore o $ \sto -> do
        rs <- simpleStorageFsck sto
        forM_ rs $ \(h,f) -> do
          print $ fill 24 (pretty f) <+> pretty h

    pDeps = do
      o <- common
      h <- strArgument ( metavar "HASH" )

      pure $ withStore o $ \sto -> do
        deepScan ScanDeep (const none) h (getBlock sto) $ \ha -> do
          print $ pretty ha

    -- TODO: reflog-del-command-- TODO: reflog-del-command
    pDel = do
      o <- common
      recurse <- optional (flag' True ( short 'r' <> long "recursive" <> help "try to delete all blocks recursively" )
                          ) <&> fromMaybe False

      dontAsk <- optional ( flag' True ( short 'y' <> long "yes" <> help "don't ask permission to delete block")
                          ) <&> fromMaybe False

      h <- strArgument ( metavar "HASH" )

      pure $ withStore o $ \sto -> do

        setLogging @TRACE tracePrefix
        hSetBuffering stdin NoBuffering

        q <- liftIO newTQueueIO

        if not recurse then
          liftIO $ atomically $ writeTQueue q h
        else do
          -- hPrint stderr $ "recurse" <+> pretty h
          deepScan ScanDeep (const none) h (getBlock sto) $ \ha -> do
            liftIO $ atomically $ writeTQueue q ha

        deps <- liftIO $ atomically $ flushTQueue q

        forM_ deps $ \d -> do
          doDelete <- if dontAsk then do
                        pure True
                      else do
                        hPutStr stderr $ show $ "Are you sure to delete block" <+> pretty d <+> "[y/n]: "
                        y <- getChar
                        hPutStrLn stderr ""
                        pure $ y `elem` ['y','Y']

          when doDelete do
            delBlock sto d
            hFlush stderr
            print $ "deleted" <+> pretty d
            hFlush stdout

