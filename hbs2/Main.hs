module Main where

import HBS2.Base58
import HBS2.Data.Detect
import HBS2.Data.Types
import HBS2.Defaults
import HBS2.Merkle
import HBS2.Net.Auth.AccessKey
import HBS2.Net.Auth.Credentials
import HBS2.Net.Messaging.UDP (UDP)
import HBS2.Net.Proto.ACB
import HBS2.Net.Proto.Definition()
import HBS2.Net.Proto.Types
import HBS2.OrDie
import HBS2.Prelude
import HBS2.Prelude.Plated
import HBS2.Refs.Linear
import HBS2.Storage
import HBS2.Storage.Simple
import HBS2.Storage.Simple.Extra


import Control.Arrow ((&&&))
import Control.Concurrent.Async
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State.Strict
import Crypto.Saltine.Core.Box qualified as Encrypt
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString qualified as BS
import Data.ByteString.Lazy (ByteString)
import Data.Either
import Data.Function
import Data.Functor
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Monoid qualified as Monoid
import Data.Set qualified as Set
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Options.Applicative
import Prettyprinter
import System.Directory
import Data.Maybe
import Lens.Micro.Platform
-- import System.FilePath.Posix
import System.IO
import System.Exit

import Codec.Serialise

import Streaming.Prelude qualified as S
-- import Streaming qualified as S


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
  }
  deriving stock (Data)

data CatOpts =
  CatOpts
  { catMerkleHash :: Maybe MerkleHash
  , catHashesOnly :: Maybe CatHashesOnly
  , catPathToKeyring :: Maybe OptKeyringFile
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
runHash opts ss = do
  withBinaryFile (hashFp opts) ReadMode $ \h -> do
    LBS.hGetContents h >>= print . pretty . hashObject @HbSync


runCat :: Data opts => opts -> SimpleStorage HbSync -> IO ()
runCat opts ss = do

  let honly = or [ x | CatHashesOnly x <- universeBi opts  ]

  void $ runMaybeT $ do

    mhash <- MaybeT $ pure $ uniLastMay @MerkleHash opts <&> fromMerkleHash

    obj <- MaybeT $ getBlock ss mhash

    let q = tryDetect mhash obj

    liftIO $ do

      let walk h = walkMerkle h (getBlock ss) $ \(hr :: Either (Hash HbSync) [HashRef]) -> do
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

      let walkAnn :: MTreeAnn [HashRef] -> IO ()
          walkAnn ann = do
            bprocess :: Hash HbSync -> ByteString -> IO ByteString <- case (_mtaCrypt ann) of
              NullEncryption -> pure (const pure)
              CryptAccessKeyNaClAsymm crypth -> do

                  keyringFile <- pure (uniLastMay @OptKeyringFile opts <&> unOptKeyringFile)
                      `orDie` "block encrypted. keyring required"
                  s <- BS.readFile keyringFile
                  ourKeys <- _peerKeyring
                      <$> pure (parseCredentials @MerkleEncryptionType (AsCredFile s))
                      `orDie` "bad keyring file"

                  blkc <- getBlock ss crypth `orDie` (show $ "missed block: " <+> pretty crypth)
                  recipientKeys :: [(PubKey 'Encrypt MerkleEncryptionType, EncryptedBox)]
                    <- pure ((either (const Nothing) Just . deserialiseOrFail) blkc)
                      `orDie` "can not deserialise access key"

                  (ourkr, box)
                      <- pure (Monoid.getFirst
                                (foldMap (\kr@(KeyringEntry pk sk _)
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
        Merkle h -> walk h
        MerkleAnn ann -> walkAnn ann
        AnnRef h -> do
          let lnk = deserialise @AnnotatedHashRef obj
          let mbHead =  headMay [ h
                                | HashRefMerkle (HashRefObject (HashRef h) _) <- universeBi lnk
                                ]
          maybe (error "empty ref") walk mbHead


runStore ::(Data opts) => opts -> SimpleStorage HbSync -> IO ()

runStore opts ss | justInit = do
  putStrLn "initialized"

  where
    justInit = maybe False fromOptInit (uniLastMay @OptInit opts)

runStore opts ss = do

  let fname = uniLastMay @OptInputFile opts

  handle <- maybe (pure stdin) (flip openFile ReadMode . unOptFile) fname

  case (uniLastMay @OptGroupkeyFile opts) of
    Nothing -> do
        root <- putAsMerkle ss handle
        print $ "merkle-root: " <+> pretty root
    Just gkfile -> do
        gk :: GroupKey MerkleEncryptionType 'NaClAsymm
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
        mtree <- (((either (const Nothing) Just . deserialiseOrFail) =<<) 
                  <$> getBlock ss (fromMerkleHash mhash))
            `orDie` "merkle tree was not stored properly with `putAsMerkle`"

        mannh <- maybe (die "can not store MerkleAnn") pure
              =<< (putBlock ss . serialise @(MTreeAnn [HashRef])) do
            MTreeAnn NoMetaData (CryptAccessKeyNaClAsymm accKeyh) mtree

        print $ "merkle-ann-root: " <+> pretty mannh

runNewGroupkey :: FilePath -> IO ()
runNewGroupkey pubkeysFile = do
  s <- BS.readFile pubkeysFile
  pubkeys <- pure (parsePubKeys s) `orDie` "bad pubkeys file"
  keypair <- newKeypair @MerkleEncryptionType Nothing
  accesskey <- AccessKeyNaClAsymm <$> do
      List.sort pubkeys `forM` \pk -> (pk, ) <$> mkEncryptedKey keypair pk
  print $ pretty $ AsGroupKeyFile $ AsBase58 $ GroupKeyNaClAsymm (_krPk keypair) accesskey

runNewRef :: Data opts => opts -> MerkleHash -> SimpleStorage HbSync -> IO ()
runNewRef opts mhash ss = do
  uuid <- UUID.nextRandom <&> (hashObject @HbSync . UUID.toASCIIBytes)
  let href = HashRef (fromMerkleHash mhash)
  let mref = HashRefMerkle (HashRefObject href Nothing)
  let ref = AnnotatedHashRef Nothing mref
  res <- simpleWriteLinkRaw ss uuid (serialise ref)
  print (pretty res)

runNewKey :: IO ()
runNewKey = do
  cred <- newCredentials @UDP
  print $ pretty $ AsCredFile $ AsBase58 cred

runListKeys :: FilePath -> IO ()
runListKeys fp = do
  s <- BS.readFile fp
  cred <- pure (parseCredentials @UDP (AsCredFile s)) `orDie` "bad keyring file"
  print $ pretty (ListKeyringKeys cred)


runKeyAdd :: FilePath -> IO ()
runKeyAdd fp = do
  hPrint stderr $ "adding a key into keyring" <+> pretty fp
  s <- BS.readFile fp
  cred <- pure (parseCredentials @UDP (AsCredFile s)) `orDie` "bad keyring file"
  credNew <- addKeyPair Nothing cred
  print $ pretty $ AsCredFile $ AsBase58 credNew

runKeyDel :: String -> FilePath -> IO ()
runKeyDel n fp = do
  hPrint stderr $ "removing key" <+> pretty n <+> "from keyring" <+> pretty fp
  s <- BS.readFile fp
  cred <- pure (parseCredentials @UDP (AsCredFile s)) `orDie` "bad keyring file"
  credNew <- delKeyPair (AsBase58 n) cred
  print $ pretty $ AsCredFile $ AsBase58 credNew


runShowPeerKey :: Maybe FilePath -> IO ()
runShowPeerKey fp = do
  handle <- maybe (pure stdin) (`openFile` ReadMode) fp
  bs <- LBS.hGet handle 4096 <&> LBS.toStrict
  let cred' = parseCredentials @UDP (AsCredFile bs)

  maybe1 cred' exitFailure $ \cred -> do
    print $ pretty $ AsBase58 (view peerSignPk cred)

runGenACB :: Maybe FilePath -> Maybe FilePath -> IO ()
runGenACB inFile outFile = do
  inf <- maybe (pure stdin) (`openFile` ReadMode) inFile
  s <- hGetContents inf
  acb <- pure (fromStringMay s :: Maybe (ACBSimple UDP)) `orDie` "invalid ACB syntax"
  let bin = serialise acb
  out <- maybe (pure stdout) (`openFile` WriteMode) outFile
  LBS.hPutStr out bin
  hClose out
  hClose inf


runDumpACB :: Maybe FilePath -> IO ()
runDumpACB inFile = do
  inf <- maybe (pure stdin) (`openFile` ReadMode) inFile
  acb <- LBS.hGetContents inf <&> deserialise @(ACBSimple UDP)
  print $ pretty (AsSyntax (DefineACB "a1" acb))

---

withStore :: Data opts => opts -> ( SimpleStorage HbSync -> IO () ) -> IO ()
withStore opts f = do
  xdg <- getXdgDirectory XdgData defStorePath <&> fromString

  let pref = uniLastDef xdg opts :: StoragePrefix
  s <- simpleStorageInit (Just pref)

  w <- replicateM 4 $ async $ simpleStorageWorker s

  f s

  simpleStorageStop s

  _ <-  waitAnyCatch w

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
                        <> command "new-ref"         (info pNewRef (progDesc "creates reference"))
                        <> command "cat"             (info pCat (progDesc "cat block"))
                        <> command "hash"            (info pHash (progDesc "calculates hash"))
                        <> command "keyring-new"     (info pNewKey (progDesc "generates a new keyring"))
                        <> command "keyring-list"    (info pKeyList (progDesc "list public keys from keyring"))
                        <> command "keyring-key-add" (info pKeyAdd (progDesc "adds a new keypair into the keyring"))
                        <> command "keyring-key-del" (info pKeyDel (progDesc "removes a keypair from the keyring"))
                        <> command "show-peer-key"   (info pShowPeerKey (progDesc "show peer key from credential file"))
                        <> command "groupkey-new"    (info pNewGroupkey (progDesc "generates a new groupkey"))
                        <> command "acb-gen"         (info pACBGen  (progDesc "generates binary ACB from text config"))
                        <> command "acb-dump"        (info pACBDump (progDesc "dumps binary ACB to text config"))
                        )

    common = do
      pref <- optional $ strOption ( short 'p' <> long "prefix" <> help "storage prefix" )
      pure $ CommonOpts pref

    pNewRef = do
      o <- common
      merkle <- flag' True ( long "merkle-tree" <> help "it's a merkle-tree reference" )
      hash <- strArgument ( metavar "HASH" )
      pure $ withStore o (runNewRef (NewRefOpts merkle) hash)

    pStore = do
      o <- common
      file <- optional $ strArgument ( metavar "FILE" )
      init <- optional $ flag' True ( long "init" <> help "just init storage") <&> OptInit
      groupkeyFile <- optional $ strOption ( long "groupkey" <> help "path to groupkey file" )
      pure $ withStore o (runStore ( StoreOpts init file (OptGroupkeyFile <$> groupkeyFile) ))

    pCat = do
      o <- common
      hash  <- optional $ strArgument ( metavar "HASH" )
      onlyh <- optional $ flag' True ( short 'H' <> long "hashes-only" <> help "list only block hashes" )
      keyringFile <- optional $ strOption ( long "keyring" <> help "path to keyring file" )
      pure $ withStore o $ runCat
          $ CatOpts hash (CatHashesOnly <$> onlyh) (OptKeyringFile <$> keyringFile)

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

    pACBGen = do
      f <- optional $ strArgument ( metavar "ACB-FILE-INPUT" )
      o <- optional $ strArgument ( metavar "ACB-FILE-OUTPUT" )
      pure (runGenACB f o)

    pACBDump = do
      f <- optional $ strArgument ( metavar "ACB-FILE-INPUT" )
      pure (runDumpACB f)
