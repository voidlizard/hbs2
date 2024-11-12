module Main where

import HBS2.Base58
import HBS2.Data.Detect
import HBS2.Data.Types
import HBS2.Data.Types.SignedBox
import HBS2.Data.KeyRing as KeyRing
import HBS2.Defaults
import HBS2.Merkle
import HBS2.Peer.Proto
import HBS2.Peer.CLI.Detect
import HBS2.Peer.RPC.Client.Unix
import HBS2.Peer.RPC.API.Storage
import HBS2.Peer.RPC.Client.StorageClient
import HBS2.Net.Auth.GroupKeyAsymm as Asymm
import HBS2.Net.Auth.GroupKeySymm qualified as Symm
import HBS2.Net.Auth.GroupKeySymm
import HBS2.Net.Auth.Credentials
import HBS2.Net.Auth.Credentials.Sigil
import HBS2.Prelude.Plated
import HBS2.Storage
import HBS2.Storage.Operations.Class
import HBS2.Storage.Operations.ByteString
import HBS2.Storage.Simple
import HBS2.Storage.Simple.Extra
import HBS2.Data.Bundle
import HBS2.OrDie
import HBS2.Version
import HBS2.Misc.PrettyStuff
import Paths_hbs2 qualified as Pkg

import HBS2.KeyMan.Keys.Direct

import HBS2.System.Logger.Simple.ANSI hiding (info)

import Data.Config.Suckless

import Codec.Serialise
import Control.Concurrent.STM qualified as STM
import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Resource
import Control.Monad.Trans.Cont
import Crypto.Saltine.Core.Box qualified as Encrypt
import Data.Aeson qualified as Aeson
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.ByteString qualified as BS
import Data.ByteArray.Hash (SipHash(..), SipKey(..))
import Data.ByteArray.Hash qualified as BA
import Data.HashMap.Strict qualified as HM
import Data.Either
import Data.List qualified as List
import Data.Maybe
import Data.Text qualified as Text
import Data.Text.IO qualified as TIO
import Lens.Micro.Platform
import Options.Applicative
import Streaming.Prelude qualified as S
import Streaming.ByteString qualified as SB
import System.Directory
import System.FilePath
import System.Exit qualified as Exit
import System.IO qualified as IO
import System.IO.Temp (emptySystemTempFile)

import Magic.Data
import Magic.Init       (magicLoadDefault,magicOpen)
import Magic.Operations (magicFile)

import UnliftIO

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


data MetadataMethod = MetaDataAuto FilePath
                      deriving stock (Eq,Generic,Show)

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

newtype OptEncPubKey = OptEncPubKey { unOptEncPk :: PubKey 'Encrypt 'HBS2Basic  }
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


data EncSchema  = EncSymm   (GroupKey 'Symm 'HBS2Basic)
                | EncAsymm  (GroupKey 'Asymm 'HBS2Basic)


hPrint :: (MonadIO m, Show a) => Handle -> a -> m ()
hPrint h s = liftIO $ IO.hPrint h s

hGetContents :: MonadIO m => Handle -> m String
hGetContents h = liftIO $ IO.hGetContents h

{- HLINT ignore "Use getChar" -}

hGetChar :: MonadIO m => Handle -> m Char
hGetChar = liftIO . IO.hGetChar

hPutStrLn :: MonadIO m => Handle -> String -> m ()
hPutStrLn h s = liftIO $ IO.hPutStrLn h s

hPutStr :: MonadIO m => Handle -> String -> m ()
hPutStr h s = liftIO $ IO.hPutStr h s

exitSuccess :: MonadIO m => m ()
exitSuccess = do
  liftIO Exit.exitSuccess

exitFailure :: MonadIO m => m ()
exitFailure = do
  liftIO Exit.exitFailure

die :: MonadIO m => String -> m a
die = liftIO . Exit.die

runHash :: Maybe HashOpts -> SimpleStorage HbSync -> IO ()
runHash Nothing _ = do
    LBS.getContents >>= print . pretty . hashObject @HbSync

runHash (Just opts) _ = do
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

      case q of
        Blob h -> getBlock ss h >>= maybe (die "blob not found") LBS.putStr

        Merkle t -> walkMerkleTree t (getBlock ss) stepInside

        MerkleAnn ann | honly -> do
          walkMerkleTree (_mtaTree ann) (getBlock ss) $ \case
            Left hx  -> err $ "missed block" <+> pretty hx
            Right hr -> print $ vcat (fmap pretty hr)

        MerkleAnn (MTreeAnn {_mtaCrypt = NullEncryption }) -> do
          bs <- runExceptT (readFromMerkle (AnyStorage ss) (SimpleKey mhash))
                   >>= orThrowPassIO -- User "can't read/decode tree"
          LBS.putStr bs

        MerkleAnn ann@(MTreeAnn {_mtaCrypt = EncryptGroupNaClSymm gkh _}) -> do
          keyring <- case uniLastMay @OptKeyringFile opts of
                       Just krf -> do
                        s <- BS.readFile (unOptKeyringFile krf)
                        cred <- pure (parseCredentials @'HBS2Basic (AsCredFile s)) `orDie` "bad keyring file"
                        pure $ view peerKeyring cred

                       Nothing -> fromMaybe mempty <$> runMaybeT do
                         rcpts <- runExceptT (readFromMerkle (AnyStorage ss) (SimpleKey gkh))
                                    >>= toMPlus
                                    <&> deserialiseOrFail @(GroupKey 'Symm s)
                                    >>= toMPlus
                                    <&> HM.keys . recipients

                         lift $ runKeymanClient do
                           loadKeyRingEntries rcpts <&> fmap snd

          let sto = AnyStorage ss
          elbs <- runExceptT $ readFromMerkle ss (ToDecryptBS mhash (liftIO . runKeymanClientRO . findMatchedGroupKeySecret sto))
          case elbs of
            Right lbs -> LBS.putStr lbs
            Left e    -> die (show e)

        MerkleAnn ann -> die "asymmetric group encryption is deprecated"

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

runStore opts ss = runResourceT do

  let fname = uniLastMay @OptInputFile opts
  let meta58  = storeBase58Meta opts

  inputFile <- case fname of
    Just (OptInputFile fn) -> pure fn

    Nothing -> do
      (_, fn) <- allocate (liftIO $ emptySystemTempFile "hbs2-store")
                          (liftIO . removeFile)

      SB.writeFile fn (SB.fromHandle stdin)
      pure fn

  maybe (pure stdin) (flip openFile ReadMode . unOptFile) fname

  case uniLastMay @OptGroupkeyFile opts of
    Nothing -> liftIO $ IO.withFile inputFile IO.ReadMode $ \ha -> do
        root' <- liftIO $ putAsMerkle ss ha

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

        hPrint stdout $ pretty root

    Just gkfile -> do

      gkSymm  <- liftIO $ Symm.parseGroupKey @'HBS2Basic . AsGroupKeyFile <$> LBS.readFile (unOptGroupkeyFile gkfile)

      let mbGk = EncSymm <$> gkSymm

      case mbGk of
        Nothing -> die "unknown or invalid group key"

        Just (EncSymm gk) -> do
          pk  <- unOptEncPk <$> pure (uniLastMay @OptEncPubKey opts) `orDie` "public key not specified"
          krf <- pure (uniLastMay @OptKeyringFile opts) `orDie` "keyring file not set"

          s <- liftIO $ BS.readFile (unOptKeyringFile krf)
          cred <- pure (parseCredentials @'HBS2Basic (AsCredFile s)) `orDie` "bad keyring file"

          sk <- pure (headMay [ (view krPk k, view krSk k)
                              |  k <- view peerKeyring cred
                              ,  view krPk k == pk
                              ]) `orDie` "secret key not found"

          gks <- pure (Symm.lookupGroupKey (snd sk) pk gk) `orDie` ("can't find secret key for " <> show (pretty (AsBase58 (fst sk))))

          void $ liftIO $ IO.withFile inputFile IO.ReadMode $ \fh -> do
            let reader =  readChunked fh (fromIntegral defBlockSize)
            qqq <- S.toList_ $ reader
                             & S.map (BA.sipHash (SipKey 2716310006254639645 507093936407764973) . LBS.toStrict)
                             & S.map \(SipHash w) -> w

            let (HbSyncHash nonce) = hashObject @HbSync (serialise qqq)

            IO.hSeek fh IO.AbsoluteSeek 0

            let segments = readChunked fh (fromIntegral defBlockSize)

            let source = ToEncryptSymmBS gks (Right gk) nonce segments  NoMetaData Nothing

            r <- runExceptT $ writeAsMerkle ss source

            case r of
              Left e  -> die (show e)
              Right h -> hPrint stdout (pretty h)

        Just (EncAsymm gk) -> liftIO $ IO.withFile inputFile IO.ReadMode $ \ha -> do

          accKeyh <- (putBlock ss . serialise . permitted . accessKey) gk
              `orDie` "can not store access key"

          let rawChunks = readChunked ha (fromIntegral defBlockSize) -- FIXME: to settings!

          let encryptedChunks = rawChunks
                  & S.mapM (fmap LBS.fromStrict . Encrypt.boxSeal (recipientPk gk) . LBS.toStrict)

          mhash <- putAsMerkle ss encryptedChunks
          mtree <- ((either (const Nothing) Just . deserialiseOrFail =<<) <$> getBlock ss (fromMerkleHash mhash))
              `orDie` "merkle tree was not stored properly with `putAsMerkle`"

          mannh <- maybe (die "can not store MerkleAnn") pure
                =<< (putBlock ss . serialise @(MTreeAnn [HashRef])) do
              MTreeAnn NoMetaData (CryptAccessKeyNaClAsymm accKeyh) mtree

          hPrint stdout $ "merkle-ann-root: " <+> pretty mannh

runNewGroupKeyAsymm :: forall s . (s ~ 'HBS2Basic) => FilePath -> IO ()
runNewGroupKeyAsymm pubkeysFile = do
  s <- BS.readFile pubkeysFile
  pubkeys <- pure (parsePubKeys @s s) `orDie` "bad pubkeys file"
  keypair <- newKeypair @s Nothing
  accesskey <- AccessKeyNaClAsymm @s <$> do
      List.sort pubkeys `forM` \pk -> (pk, ) <$> mkEncryptedKey keypair pk
  print $ pretty $ AsGroupKeyFile $ AsBase58 $ GroupKeyNaClAsymm (_krPk keypair) accesskey

runNewKey :: forall s . (s ~ 'HBS2Basic) => Int -> IO ()
runNewKey n = do
  cred0 <- newCredentials @s
  cred <- foldM (\cred _ -> addKeyPair Nothing cred) cred0 [1..n]
  print $ pretty $ AsCredFile $ AsBase58 cred

runListKeys :: forall s . (s ~ 'HBS2Basic) => FilePath -> IO ()
runListKeys fp = do
  s <- BS.readFile fp
  cred <- pure (parseCredentials @s (AsCredFile s)) `orDie` "bad keyring file"
  print $ pretty (ListKeyringKeys cred)


runKeyAdd :: forall s . (s ~ 'HBS2Basic) => FilePath -> IO ()
runKeyAdd fp = do
  hPrint stderr $ "adding a key into keyring" <+> pretty fp
  s <- BS.readFile fp
  cred <- pure (parseCredentials @s (AsCredFile s)) `orDie` "bad keyring file"
  credNew <- addKeyPair Nothing cred
  print $ pretty $ AsCredFile $ AsBase58 credNew

runKeyDel :: forall s . (s ~ 'HBS2Basic) => String -> FilePath -> IO ()
runKeyDel n fp = do
  hPrint stderr $ "removing key" <+> pretty n <+> "from keyring" <+> pretty fp
  s <- BS.readFile fp
  cred <- pure (parseCredentials @s (AsCredFile s)) `orDie` "bad keyring file"
  credNew <- delKeyPair (AsBase58 n) cred
  print $ pretty $ AsCredFile $ AsBase58 credNew


runShowPeerKey :: forall s . ( s ~ 'HBS2Basic) => Maybe FilePath -> IO ()
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
  <> header "hbs2"
  <> progDesc "hbs2 tools"
  )
  where
    parser ::  Parser (IO ())
    parser = hsubparser (  command "store"           (info pStore (progDesc "store block"))
                        <> command "cat"             (info pCat (progDesc "cat block"))
                        <> command "metadata"        (info pMetadata (progDesc "tree metadata manipulation"))
                        <> command "hash"            (info pHash (progDesc "calculates hash"))
                        <> command "fsck"            (info pFsck (progDesc "check storage constistency"))
                        <> command "deps"            (info pDeps (progDesc "print dependencies"))
                        <> command "del"             (info pDel (progDesc "del block"))
                        <> command "keyring"         (info pKeyRing (progDesc "keyring commands"))
                        <> command "keyring-new"     iNewKey
                        <> command "keyring-list"    iKeyList
                        <> command "keyring-key-add" iKeyAdd
                        <> command "keyring-key-del" iKeyDel
                        <> command "sigil"           (info pSigil (progDesc "sigil functions"))
                        <> command "show-peer-key"   (info pShowPeerKey (progDesc "show peer key from credential file"))
                        <> command "groupkey"        (info pGroupKey (progDesc "group key commands"))
                        <> command "reflog"          (info pReflog (progDesc "reflog commands"))
                        <> command "bundle"          (info pBundle (progDesc "bundle commands"))
                        <> command "anyref"          (info pAnyRef (progDesc "anyref commands"))
                        <> command "version"         (info pVersion (progDesc "show program version"))
                        )
    common = do
      pref <- optional $ strOption ( short 'p' <> long "prefix" <> help "storage prefix" )
      pure $ CommonOpts pref

    pVersion = pure do
        LBS.putStr $ Aeson.encode $(inlineBuildVersion Pkg.version)

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
      let mpk = fromStringMay @(PubKey 'Encrypt 'HBS2Basic) arg
      maybe1 mpk (Left "invalid public key") (pure . OptEncPubKey)

    pCat = do
      o <- common
      hash  <- optional $ strArgument ( metavar "HASH" )
      onlyh <- optional $ flag' True ( short 'H' <> long "hashes-only" <> help "list only block hashes" )
      keyringFile <- optional $ strOption ( long "keyring" <> help "path to keyring file" )
      raw <- optional $ flag' True ( short 'r' <> long "raw" <> help "dump raw block" )
      pure $ withStore o $ runCat
          $ CatOpts hash (CatHashesOnly <$> onlyh) (OptKeyringFile <$> keyringFile) raw

    pMetadata = hsubparser ( command "dump" (info pMetadataDump (progDesc "dump metadata"))
                           <> command "create" (info pMetadataCreate (progDesc "create tree with metadata"))
                           )

    pMetadataDump = do
      o <- common
      h <- argument (maybeReader (fromStringMay @HashRef)) (metavar "HASH") <&> fromHashRef
      pure $ flip runContT pure do
        sto <- ContT (withStore o)

        void $ runMaybeT do
          bs <- getBlock sto h >>= toMPlus
          case tryDetect h bs of
            MerkleAnn (MTreeAnn { _mtaMeta = ShortMetadata s } ) -> do
              liftIO $ TIO.putStr s

            MerkleAnn (MTreeAnn { _mtaMeta = AnnHashRef mh } ) -> do

              bs <- getBlock sto mh
                       `orDie` "cant' read metadata"

              liftIO $ LBS.putStr bs

            _ -> exitFailure

    pMetadataCreate = do
      o <- common
      how <- MetaDataAuto <$> strOption ( long "auto" <> metavar "FILENAME" <> help "automatic metadata from file name")
      dry <- flag False True (long "dry" <> short 'n' <> help "don't write to storage")
      hOnly <- flag False True (long "hash" <> short 'H' <> help "merely print hash")

      pure $ flip runContT pure do

        sto <- ContT $ withStore o

        void $ runMaybeT do

          case how of
            MetaDataAuto fn -> do

              meta <- liftIO do
                        magic <- magicOpen [MagicMimeType,MagicMime,MagicMimeEncoding]
                        magicLoadDefault magic
                        mime <- magicFile magic fn

                        pure [ "file-name:" <+> dquotes (pretty $ takeFileName fn)
                             , "mime-type:" <+> dquotes (pretty mime)
                             ]

              let s = LBS8.pack $ show $ vcat meta

              unless hOnly do
                liftIO $ LBS8.putStrLn s
                liftIO $ LBS8.putStrLn ""

              guard (not dry)

              mth <- putBlock sto s >>= toMPlus

              bs <- liftIO $ LBS.readFile fn

              root <- writeAsMerkle sto bs

              mt <- getBlock sto root `orDie` "can't read merkle tree just written"
                      <&> deserialiseOrFail @(MTree [HashRef])
                      >>= orThrowUser "corrupted merkle tree -- should never happen"

              delBlock sto root

              let mtann = MTreeAnn (AnnHashRef mth) NullEncryption mt

              hnew <- putBlock sto (serialise mtann)
                        `orDie` "can't write merkle tree"

              liftIO $ print $ pretty hnew

    pGroupKey = pGroupKeySymm

    pGroupKeySymm = hsubparser (  command "gen" (info pGroupKeySymmGen (progDesc "generate") )
                               <> command "from-sigils" (info pGroupKeyFromSigils (progDesc "generate from sigils") )
                               <> command "from-keys" (info pGroupKeyFromKeys (progDesc "generate from list of encryption pubkeys") )
                               <> command "dump" (info pGroupKeySymmDump (progDesc "dump") )
                               <> command "update" (info pGroupKeySymmUpdate (progDesc "update") )
                               )

    pGroupKeyFromSigils = do
      fns <- many $ strArgument ( metavar "SIGIL-FILES" <> help "sigil file list" )
      pure $ do
        members <- for fns $ \fn -> do

          sigil <- (BS.readFile fn <&> parseSerialisableFromBase58 @(Sigil 'HBS2Basic))
                      `orDie` "parse sigil failed"
          (_,sd) <- pure (unboxSignedBox0 @(SigilData 'HBS2Basic) (sigilData sigil))
                    `orDie` ("signature check failed "  <> fn)

          pure (sigilDataEncKey sd)

        gk <- Symm.generateGroupKey @'HBS2Basic Nothing members
        print $ pretty (AsGroupKeyFile gk)

    pGroupKeyFromKeys = do
      pure $ do
        input <- getContents <&> words
        members <- for input $ \s -> do
                     fromStringMay @(PubKey 'Encrypt 'HBS2Basic) s
                       & maybe (die "invalid public key") pure

        gk <- Symm.generateGroupKey @'HBS2Basic Nothing members
        print $ pretty (AsGroupKeyFile gk)


    pGroupKeySymmGen = do
      fn <- optional $ strArgument ( metavar "FILE" <> help "group key definition file" )
      pure $ do
        syn <- maybe1 fn getContents readFile <&> parseTop <&> fromRight mempty

        let members = [ fromStringMay @(PubKey 'Encrypt 'HBS2Basic) (Text.unpack s)
                      | (ListVal (Key "member" [LitStrVal s]) ) <- syn
                      ] & catMaybes

        gk <- Symm.generateGroupKey @'HBS2Basic Nothing members
        print $ pretty (AsGroupKeyFile gk)

    pGroupKeySymmDump = do
      fn <- optional $ strArgument ( metavar "FILE" <> help "group key file" )
      pure $ do
        gk <- ( maybe1 fn LBS.getContents LBS.readFile
                <&> Symm.parseGroupKey @'HBS2Basic . AsGroupKeyFile ) `orDie` "Invalid group key file"

        print $ pretty gk

    -- SEE: group-key-update-note

    pGroupKeySymmUpdate = do
      keyringFile <- strOption ( long "keyring" <> short 'k' <> help "path to keyring file" )
      dsl         <- strOption ( long "dsl" <>  short 'D' <> help "dsl file" )
      fn          <- strArgument ( metavar "FILE" <> help "group key file" )

      pure do

        sc <- BS.readFile keyringFile
        creds <- pure (parseCredentials @(Encryption L4Proto) (AsCredFile sc)) `orDie` "bad keyring file"

        gk <- ( LBS.readFile fn
                <&> Symm.parseGroupKey @'HBS2Basic . AsGroupKeyFile ) `orDie` "Invalid group key file"

        let keys = [ (view krPk x, view krSk x) | x <- view peerKeyring creds ]

        let gksec' = [ Symm.lookupGroupKey sk pk gk | (pk,sk) <- keys ] & catMaybes & headMay

        gsec <- pure gksec' `orDie` "Group key not found"

        syn <- readFile dsl <&> parseTop <&> fromRight mempty

        -- FIXME: fix-code-dup-members
        let members = [ fromStringMay @(PubKey 'Encrypt 'HBS2Basic) (Text.unpack s)
                      | (ListVal (Key "member" [LitStrVal s]) ) <- syn
                      ] & catMaybes

        debug $ vcat (fmap (pretty.AsBase58) members)

        gkNew <- Symm.generateGroupKey @'HBS2Basic (Just gsec) members
        print $ pretty (AsGroupKeyFile gkNew)

    pHash = do
      o <- common
      what  <- optional $ HashOpts <$> strArgument ( metavar "FILE" )
      pure $ withStore o $ runHash what

    iNewKey = info pNewKey (progDesc "generates a new keyring")

    pNewKey = do
      n <- optional $ option auto ( short 'n' <> long "number")
      pure $ runNewKey (fromMaybe 0 n)

    pShowPeerKey = do
      fp <- optional $ strArgument ( metavar "FILE" )
      pure $ runShowPeerKey fp

    iKeyList = info pKeyList (progDesc "list public keys from keyring")

    pKeyList = do
      f <- strArgument ( metavar "KEYRING-FILE" )
      pure (runListKeys f)

    iKeyAdd = info pKeyAdd (progDesc "adds a new keypair into the keyring")

    pKeyAdd = do
      f <- strArgument ( metavar "KEYRING-FILE" )
      pure (runKeyAdd f)

    iKeyDel = info pKeyDel (progDesc "removes a keypair from the keyring")

    pKeyDel = do
      s <- strArgument ( metavar "PUB-KEY-BASE58" )
      f <- strArgument ( metavar "KEYRING-FILE" )
      pure (runKeyDel s f)


    iKeyDisclose = info pKeyDisclose (progDesc "disclose private key")

    pKeyDisclose = do
      pks <- argument pPubKey ( metavar "PUB-KEY-ID" )

      pure $ flip runContT pure $ callCC \_ -> do

        soname <- lift detectRPC `orDie` "peer rpc not found"

        y <- lift do
          hSetBuffering stdin NoBuffering
          hPutDoc stderr $  yellow "Note: you are about to disclose private signing key"
                              <+> pretty (AsBase58 pks) <> line
                  <> "Probably, you wish to enable unsolicited notifications for some channel" <> line
                  <> "Anyway, make sure you know what you doing before proceeding" <> line
                  <> yellow "Proceed?" <+> "[y/n]: "
          hFlush stderr
          hGetChar stdin


        void $ ContT $ whenTrue ()
                         ( y `elem` "yY")
                         (hPutStrLn stderr "" >> hPutDoc stderr "wise. see you!")

        mcreds <- lift do
          hPutDoc stderr $ line
                      <> yellow "Note:" <+> "the key will be safe until you publish its hash"
                      <+> "somewhere" <> line
                      <> "so if you have changed your mind --- you may delete it with hbs2 del"
                      <> line <> line

          runKeymanClient $ loadCredentials pks

        creds <- ContT $ maybe1 mcreds exitFailure

        -- NOTE: only-sign-key-disclosed-yet
        let creds1 = set peerKeyring mempty creds

        rpc <- ContT $ withRPC2 @StorageAPI soname

        let sto = AnyStorage (StorageClient rpc)

        h <- putBlock sto (serialise creds1)

        liftIO $ print $ pretty h

    -- TODO: all-keyring-management-to-keyman
    pKeyRing = hsubparser (    command "find" (info pKeyRingFind (progDesc "find keyring"))
                           <>  command "new"  iNewKey
                           <>  command "list" iKeyList
                           <>  command "add"  iKeyAdd
                           <>  command "del"  iKeyDel
                           <>  command "disclose" iKeyDisclose
                          )

    pKeyRingFind = do
      spk <- option pPubKey ( long "sign-key" <> short 's' <> help "sign-key" )
      masks <- many (strArgument (metavar "PATHS"))
      pure do
        krf <- KeyRing.findKeyRing masks spk
        print $ vcat (fmap pretty krf)

    pReflog = hsubparser ( command "get" (info pRefLogGet (progDesc "get reflog root") )  )

    -- FIXME: only-for-hbs2-basic-encryption
    pRefLogGet = do
      o <- common
      reflogs <- strArgument ( metavar "REFLOG" )
      pure $ withStore o (runRefLogGet @'HBS2Basic reflogs)


    pAnyRef = hsubparser (  command "get" (info pAnyRefGet (progDesc "get anyref value") )
                         <> command "set" (info pAnyRefSet (progDesc "set anyref value") )
                         )

    pAnyRefGet = do
      o <- common
      anyref <- strArgument ( metavar "ANYREF" )
      pure $ withStore o (runAnyRefGet @'HBS2Basic anyref)

    pAnyRefSet = do
      o <- common
      anyref <- strArgument ( metavar "ANYREF" )
      val    <- strArgument ( metavar "HASHREF" )
      pure $ do
        hr <- pure (fromStringMay val) `orDie` "bad HASHREF"
        withStore o (runAnyRefSet @'HBS2Basic anyref hr)

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

    -- TODO: reflog-del-command
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

        deps <- liftIO $ atomically $ STM.flushTQueue q

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

        let refval = makeBundleRefValue @'HBS2Basic pk sk (BundleRefSimple ref)

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


    pSigil = hsubparser (  command  "create" (info pCreateSigil (progDesc "create sigil"))
                        <> command "check" (info pCheckSigil (progDesc "check sigil"))
                        )

    pCheckSigil = do
      _ <- common
      fn <- optional $ strArgument ( metavar "SIGIL-FILE" )
      pure $ do
        handle <- maybe1 fn (pure stdin) (flip openFile ReadMode)
        sigil <- (BS.hGetContents handle <&> parseSerialisableFromBase58 @(Sigil 'HBS2Basic))
                    `orDie` "parse sigil failed"
        (_,sd) <- pure (unboxSignedBox0 @(SigilData 'HBS2Basic) (sigilData sigil))
                  `orDie` "signature check failed"
        print $ parens ("sigil" <> line <> indent 2 (vcat $ [pretty sigil, pretty sd]))

    pCreateSigil = do
      _ <- common
      krf <- strOption   (long "keyring" <> short 'k' <> help "keyring")
      txt <- optional $ strOption ( long "description" <> short 'm' <> help "short sigil information")
      href <- optional $ option phref ( long "metadata-ref" <> help "reference to metadata" )
      pk <- argument ppk (metavar "PUBKEY")
      pure $ do
        sc <- BS.readFile krf
        creds <- pure (parseCredentials @'HBS2Basic (AsCredFile sc)) `orDie` "bad keyring file"
        sigil <- pure (makeSigilFromCredentials @'HBS2Basic creds pk txt href)
                  `orDie` "public key not found in credentials file"
        print $ pretty (AsBase58 sigil)

    ppk = maybeReader fromStringMay
    phref = maybeReader fromStringMay


    pPubKey = maybeReader (fromStringMay @(PubKey 'Sign 'HBS2Basic))
