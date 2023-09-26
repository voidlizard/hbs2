module Main where

import HBS2.Base58
import HBS2.Data.Detect
import HBS2.Data.Types
import HBS2.Data.Types.EncryptedBox
import HBS2.Defaults
import HBS2.Merkle
import HBS2.Net.Proto.Types
import HBS2.Net.Auth.GroupKeyAsymm as Asymm
import HBS2.Net.Auth.GroupKeySymm qualified as Symm
import HBS2.Net.Auth.GroupKeySymm
-- (ToEncrypt(..))
import HBS2.Net.Auth.Credentials
import HBS2.Net.Proto.Definition()
import HBS2.Net.Proto.RefLog(RefLogKey(..))
import HBS2.Net.Proto.AnyRef(AnyRefKey(..))
import HBS2.Prelude.Plated
import HBS2.Storage.Operations.Class
import HBS2.Storage.Simple
import HBS2.Storage.Simple.Extra
import HBS2.Data.Bundle
import HBS2.OrDie


import HBS2.System.Logger.Simple hiding (info)

import Data.Config.Suckless
import Data.Config.Suckless.KeyValue

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Except
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
import Data.Either
import Data.Maybe
import Data.Text qualified as Text
import Lens.Micro.Platform
-- import System.FilePath.Posix
import System.IO
import System.Exit

import Codec.Serialise

import Streaming.Prelude qualified as S
-- import Streaming qualified as S


tracePrefix :: SetLoggerEntry
tracePrefix  = logPrefix "[trace] " . toStderr

debugPrefix :: SetLoggerEntry
debugPrefix  = logPrefix "[debug] " . toStderr

errorPrefix :: SetLoggerEntry
errorPrefix  = logPrefix "[error] " . toStderr

warnPrefix :: SetLoggerEntry
warnPrefix   = logPrefix "[warn] " . toStderr

noticePrefix :: SetLoggerEntry
noticePrefix = logPrefix "[notice] " . toStderr


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

newtype OptEncPubKey = OptEncPubKey { unOptEncPk :: PubKey 'Encrypt HBS2Basic  }
                       deriving newtype (Eq,Ord)
                       deriving stock (Data)

newtype OptInit = OptInit { fromOptInit :: Bool }
                  deriving newtype (Eq,Ord,Pretty)
                  deriving stock (Data,Generic)

data StoreOpts =
  StoreOpts
  {  storeInit         :: Maybe OptInit
  ,  storeInputFile    :: Maybe OptInputFile
  ,  storeGroupkeyFile :: Maybe OptGroupkeyFile
  ,  storeBase58Meta   :: Maybe String
  ,  storeEncPubKey    :: Maybe OptEncPubKey
  ,  storeKeyringFile  :: Maybe OptKeyringFile
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


data EncSchema  = EncSymm   (GroupKey 'Symm HBS2Basic)
                | EncAsymm  (GroupKey 'Asymm HBS2Basic)

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

              EncryptGroupNaClSymm{} -> do
                die "EncryptGroupNaClSymm is not supported yet"

              CryptAccessKeyNaClAsymm crypth -> do

                  keyringFile <- pure (uniLastMay @OptKeyringFile opts <&> unOptKeyringFile)
                      `orDie` "block encrypted. keyring required"
                  s <- BS.readFile keyringFile
                  ourKeys <- _peerKeyring
                      <$> pure (parseCredentials @s (AsCredFile s))
                      `orDie` "bad keyring file"

                  blkc <- getBlock ss crypth `orDie` (show $ "missed block: " <+> pretty crypth)
                  recipientKeys :: [(PubKey 'Encrypt s, EncryptedBox (KeyringEntry s))]
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

        MerkleAnn ann | honly -> do
          walkMerkleTree (_mtaTree ann) (getBlock ss) $ \case
            Left hx  -> err $ "missed block" <+> pretty hx
            Right hr -> print $ vcat (fmap pretty hr)

        MerkleAnn ann@(MTreeAnn {_mtaCrypt = EncryptGroupNaClSymm{}}) -> do

          krf <- pure (uniLastMay @OptKeyringFile opts) `orDie` "keyring file not set"
          s <- BS.readFile (unOptKeyringFile krf)
          cred <- pure (parseCredentials @HBS2Basic (AsCredFile s)) `orDie` "bad keyring file"
          let keyring = view peerKeyring cred

          elbs <- runExceptT $ readFromMerkle ss (ToDecryptBS keyring mhash)
          case elbs of
            Right lbs -> LBS.putStr lbs
            Left e    -> die (show e)

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

      gkSymm  <- Symm.parseGroupKey @HBS2Basic . AsGroupKeyFile <$> LBS.readFile (unOptGroupkeyFile gkfile)
      gkAsymm <- Asymm.parseGroupKey . AsGroupKeyFile <$> BS.readFile (unOptGroupkeyFile gkfile)

      let mbGk = EncSymm <$> gkSymm <|> EncAsymm <$> gkAsymm

      case mbGk of
        Nothing -> die "unknown or invalid group key"

        Just (EncSymm gk) -> do
          pk  <- unOptEncPk <$> pure (uniLastMay @OptEncPubKey opts) `orDie` "public key not specified"
          krf <- pure (uniLastMay @OptKeyringFile opts) `orDie` "keyring file not set"

          s <- BS.readFile (unOptKeyringFile krf)
          cred <- pure (parseCredentials @HBS2Basic (AsCredFile s)) `orDie` "bad keyring file"

          sk <- pure (headMay [ (view krPk k, view krSk k)
                              |  k <- view peerKeyring cred
                              ,  view krPk k == pk
                              ]) `orDie` "secret key not found"

          gks <- pure (Symm.lookupGroupKey (snd sk) pk gk) `orDie` ("can't find secret key for " <> show (pretty (AsBase58 (fst sk))))

          let  segments :: S.Stream (S.Of ByteString) IO ()
               segments = readChunked handle (fromIntegral defBlockSize)

          let source = ToEncryptSymmBS gks segments gk

          r <- runExceptT $ writeAsMerkle ss source

          case r of
            Left e  -> die (show e)
            Right h -> print (pretty h)

        Just (EncAsymm gk) -> do

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

runNewGroupKeyAsymm :: forall s . (s ~ HBS2Basic) => FilePath -> IO ()
runNewGroupKeyAsymm pubkeysFile = do
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

runAnyRefGet :: forall s t . IsRefPubKey s => AnyRefKey t s -> SimpleStorage HbSync -> IO ()
runAnyRefGet s ss = do
  ref' <- getRef ss s
  maybe1 ref' exitFailure $ \ref -> do
    print $ pretty ref
    exitSuccess

runAnyRefSet :: forall s t . IsRefPubKey s => AnyRefKey t s -> HashRef -> SimpleStorage HbSync -> IO ()
runAnyRefSet s hr ss = do
  updateRef ss s (fromHashRef hr)

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
                        <> command "deps"            (info pDeps (progDesc "print dependencies"))
                        <> command "del"             (info pDel (progDesc "del block"))
                        <> command "keyring-new"     (info pNewKey (progDesc "generates a new keyring"))
                        <> command "keyring-list"    (info pKeyList (progDesc "list public keys from keyring"))
                        <> command "keyring-key-add" (info pKeyAdd (progDesc "adds a new keypair into the keyring"))
                        <> command "keyring-key-del" (info pKeyDel (progDesc "removes a keypair from the keyring"))
                        <> command "show-peer-key"   (info pShowPeerKey (progDesc "show peer key from credential file"))
                        <> command "groupkey"        (info pGroupKey (progDesc "group key commands"))
                        <> command "reflog"          (info pReflog (progDesc "reflog commands"))
                        <> command "bundle"          (info pBundle (progDesc "bundle commands"))
                        <> command "anyref"          (info pAnyRef (progDesc "anyref commands"))
                        )

    common = do
      pref <- optional $ strOption ( short 'p' <> long "prefix" <> help "storage prefix" )
      pure $ CommonOpts pref

    pStore = do
      o <- common
      file <- optional $ strArgument ( metavar "FILE" )
      init' <- optional $ flag' True ( long "init" <> help "just init storage") <&> OptInit
      groupkeyFile <- optional $ strOption ( long "groupkey" <> short 'g' <> help "path to groupkey file" )
      b58meta <- optional $ strOption ( long "short-meta-base58" <> help "pass escaped metadata string")
      pk <- optional $ option epk ( long "public-key" <> short 'P' <> help "public key of group key")
      kr <- optional $ strOption ( long "keyring" <> short 'k' <> help "keyring file") <&> OptKeyringFile
      pure $ withStore o (runStore ( StoreOpts init' file (OptGroupkeyFile <$> groupkeyFile) b58meta pk kr))

    epk  :: ReadM OptEncPubKey
    epk = eitherReader $ \arg -> do
      let mpk = fromStringMay @(PubKey 'Encrypt HBS2Basic) arg
      maybe1 mpk (Left "invalid public key") (pure . OptEncPubKey)

    pCat = do
      o <- common
      hash  <- optional $ strArgument ( metavar "HASH" )
      onlyh <- optional $ flag' True ( short 'H' <> long "hashes-only" <> help "list only block hashes" )
      keyringFile <- optional $ strOption ( long "keyring" <> help "path to keyring file" )
      raw <- optional $ flag' True ( short 'r' <> long "raw" <> help "dump raw block" )
      pure $ withStore o $ runCat
          $ CatOpts hash (CatHashesOnly <$> onlyh) (OptKeyringFile <$> keyringFile) raw

    pGroupKey = hsubparser (  command "asymm" (info pGroupKeyAsymm (progDesc "asymmetric group keys") )
                           <> command "symm"  (info pGroupKeySymm  (progDesc "symmetric group keys") )
                           )


    pGroupKeyAsymm = hsubparser (  command "gen" (info pGroupKeyAsymmNew (progDesc "generate") )
                                )

    pGroupKeyAsymmNew = do
      pubkeysFile <- strArgument ( metavar "FILE" <> help "path to a file with a list of recipient public keys" )
      pure $ runNewGroupKeyAsymm pubkeysFile


    pGroupKeySymm = hsubparser (  command "gen" (info pGroupKeySymmGen (progDesc "generate") )
                               <> command "dump" (info pGroupKeySymmDump (progDesc "dump") )
                               )

    pGroupKeySymmGen = do
      fn <- optional $ strArgument ( metavar "FILE" <> help "group key definition file" )
      pure $ do
        syn <- maybe1 fn getContents readFile <&> parseTop <&> fromRight mempty

        let members = [ fromStringMay @(PubKey 'Encrypt HBS2Basic) (Text.unpack s)
                      | (ListVal (Key "member" [LitStrVal s]) ) <- syn
                      ] & catMaybes

        gk <- Symm.generateGroupKey @HBS2Basic members
        print $ pretty (AsGroupKeyFile gk)

    pGroupKeySymmDump = do
      fn <- optional $ strArgument ( metavar "FILE" <> help "group key file" )
      pure $ do
        gk <- ( maybe1 fn LBS.getContents LBS.readFile
                <&> Symm.parseGroupKey @HBS2Basic . AsGroupKeyFile ) `orDie` "Invalid group key file"

        print $ pretty gk

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


    pAnyRef = hsubparser (  command "get" (info pAnyRefGet (progDesc "get anyref value") )
                         <> command "set" (info pAnyRefSet (progDesc "set anyref value") )
                         )

    pAnyRefGet = do
      o <- common
      anyref <- strArgument ( metavar "ANYREF" )
      pure $ withStore o (runAnyRefGet @HBS2Basic anyref)

    pAnyRefSet = do
      o <- common
      anyref <- strArgument ( metavar "ANYREF" )
      val    <- strArgument ( metavar "HASHREF" )
      pure $ do
        hr <- pure (fromStringMay val) `orDie` "bad HASHREF"
        withStore o (runAnyRefSet @HBS2Basic anyref hr)

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

    pBundle = hsubparser ( command  "create" (info pBundleCreate  (progDesc "create bundle"))
                         <> command "list" (info pBundleList      (progDesc "list bundle"))
                         <> command "import" (info pBundleImport  (progDesc "import objects from bundle"))
                         <> command "create-ref" (info pBundleCreateRef (progDesc "create bundle ref block"))
                         )

    pBundleCreate = do
      o <- common
      fname <- optional $ strOption (long "file" <> short 'f' <> help "hash list file (plaintext)")
      pure $ withStore o $ \sto -> do
        handle <- maybe (pure stdin) (flip openFile ReadMode . unOptFile) fname

        ls <- hGetContents handle <&> lines
        let hashes = mapMaybe (fromStringMay @HashRef) ls

        when (length ls  /= length hashes) do
          die "Invalid hashref found"

        bundle <- createBundle sto hashes `orDie` "can't create bundle"

        print $ pretty bundle

    pBundleCreateRef = do
      o <- common
      kr <- strOption (long "keyring" <> short 'k' <> help "owner credentials")
      hash <- strArgument (metavar "HASHREF")

      pure $ withStore o $ \sto -> do
        sc <- BS.readFile kr
        creds <- pure (parseCredentials @(Encryption L4Proto) (AsCredFile sc)) `orDie` "bad keyring file"

        let sk = view peerSignSk creds
        let pk = view peerSignPk creds

        ref <- pure (fromStringMay hash) `orDie` "invalid HASHREF"

        let refval = makeBundleRefValue @L4Proto pk sk (BundleRefSimple ref)

        mh <- putBlock sto (serialise refval)

        maybe1 mh exitFailure $ \h -> do
          print $ pretty h
          exitSuccess

    pBundleImport = do
      o <- common
      mbHref <- strArgument (metavar "HASHREF")
      pure $ withStore o $ \sto -> do
        href <- pure (fromStringMay @HashRef mbHref) `orDie` "invalid hashref"
        r <- importBundle sto (void . putBlock sto . snd) href
        case r of
          Right{} -> pure ()
          Left e -> die (show e)

    pBundleList = do
      o <- common
      mbHref <- strArgument (metavar "HASHREF")
      doCheck  <- optional (flag' False ( long "check" <> help "check hashes" )) <&> fromMaybe False
      pure $ withStore o $ \sto -> do
        href <- pure (fromStringMay @HashRef mbHref) `orDie` "invalid hashref"
        r <- importBundle sto (outSection doCheck) href
        case r of
          Right{} -> pure ()
          Left e -> die (show e)

      where
        outSection :: Bool -> (Maybe HashRef, ByteString) -> IO ()

        outSection True  x@(Just h, bs) = do
          let hh = HashRef $ hashObject @HbSync bs

          unless (hh == h) do
            die $ "hash mismatch:" <> show (pretty h <+> pretty hh)

          printHash x

        outSection False x = printHash x

        outSection _ x@(Nothing, _) = printHash x

        printHash = void . print . pretty . fst



