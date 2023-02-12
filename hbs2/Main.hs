module Main where

import HBS2.Base58
import HBS2.Data.Detect
import HBS2.Data.Types
import HBS2.Defaults
import HBS2.Merkle
import HBS2.Net.Auth.AccessKey
import HBS2.Net.Auth.Credentials
import HBS2.Net.Messaging.UDP (UDP)
import HBS2.Net.Proto.Definition()
import HBS2.Net.Proto.Types
import HBS2.Prelude.Plated
import HBS2.Storage.Simple
import HBS2.Storage.Simple.Extra
import HBS2.OrDie


import Data.ByteString.Lazy (ByteString)
import Control.Concurrent.Async
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State.Strict
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString qualified as BS
import Data.Either
import Data.Function
import Data.Functor
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


newtype OptInit = OptInit { fromOptInit :: Bool }
                  deriving newtype (Eq,Ord,Pretty)
                  deriving stock (Data,Generic)

data OptEncryption e = OptEncryption
  { encryptFromSecKey :: Maybe (PrivKey 'Encrypt e)
  , encryptToSecKey :: PrivKey 'Encrypt e
  , encryptToPubKey :: PubKey 'Encrypt e
  , encryptToGroup :: [PubKey 'Encrypt e]
  }
  deriving stock (Data)

data StoreOpts e =
  StoreOpts
  {  storeInit      :: Maybe OptInit
  ,  storeInputFile :: Maybe OptInputFile
  ,  storeEncryption :: Maybe (OptEncryption e)
  }
  deriving stock (Data)

data CatOpts =
  CatOpts
  { catMerkleHash :: Maybe MerkleHash
  , catHashesOnly :: Maybe CatHashesOnly
  , catPathToKeyring :: Maybe FilePath
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

      let walkAnn :: Ann -> MTree [HashRef] -> IO ()
          walkAnn ann t = do
            bprocess :: Int -> ByteString -> IO ByteString <- case ann of
              NullAnn -> pure (\_ -> pure)
              GroupKeyCrypt crypth -> do
                  mblk <- getBlock ss crypth
                  case mblk of
                    Nothing  -> die $ show $ "missed block: " <+> pretty crypth
                    Just blk -> do
                        -- FIXME apply crypto scheme from `crypth` to stream of blk's
                        -- extract AccessKeyV1 from blk
                        -- find in it any pubkey known to us
                        -- decrypt corresponding EncryptedBox
                        -- get actual (PrivKey 'Encrypt e) to use for merkle decryption
                        pure $ \blnum blk -> do
                            -- convert blnum to Crypto.Saltine.Core.Box.Nonce
                            -- decrypt blk with this nonce and priv key
                            undefined

            flip evalStateT firstNumOfBlock $
             walkMerkleTree t (lift . getBlock ss) $ \(hr :: Either (Hash HbSync) [HashRef]) -> do
              case hr of
                Left hx -> lift $ void $ hPrint stderr $ "missed block:" <+> pretty hx
                Right (hrr :: [HashRef]) -> do
                  forM_ hrr $ \(HashRef hx) -> do
                    if honly then do
                      lift $ print $ pretty hx
                    else do
                      mblk <- lift $ getBlock ss hx
                      case mblk of
                        Nothing  -> lift $ die $ show $ "missed block: " <+> pretty hx
                        Just blk -> do
                            blnum <- get
                            modify (+1)
                            lift $ LBS.putStr =<< bprocess blnum blk

      case q of
        Blob h -> getBlock ss h >>= maybe (die "blob not found") LBS.putStr
        Merkle h -> walk h
        MerkleAnn (MTreeAnn ann hs) -> walkAnn ann hs
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

firstNumOfBlock :: Int
firstNumOfBlock = 1

runStore opts ss = do

  let fname = uniLastMay @OptInputFile opts

  handle <- maybe (pure stdin) (flip openFile ReadMode . unOptFile) fname

  case (uniLastMay @(OptEncryption MerkleEncryptionType) opts) of
    Nothing -> do
        root <- putAsMerkle ss handle
        print $ "merkle-root: " <+> pretty root
    Just encOpts -> do

        -- FIXME generate AccessKeyV1, store it
        accKeyh <- maybe (die "can not store access key") pure
              =<< (putBlock ss . serialise) do
            AccessKeyV1 (undefined :: [(PubKey 'Encrypt e, EncryptedBox)])

        let rawChunks :: S.Stream (S.Of ByteString) IO ()
            rawChunks = readChunked handle (fromIntegral defBlockSize) -- FIXME: to settings!

            encryptedChunks :: S.Stream (S.Of ByteString) IO ()
            -- FIXME get keys to enrypt
            encryptedChunks = rawChunks
                & S.zip (S.enumFrom firstNumOfBlock)
                & S.map (\(blnum, blk) -> do
                      -- FIXME convert blnum to Crypto.Saltine.Core.Box.Nonce
                      -- encrypt blk with this nonce and priv key
                      undefined

        mhash <- putAsMerkle ss encryptedChunks

        mannh <- maybe (die "can not store MerkleAnn") pure
              =<< (putBlock ss . serialise) do
            MerkleAnn (MTreeAnn (GroupKeyCrypt accKeyh) (undefined mhash))

        print $ "merkle-ann-root: " <+> pretty mannh

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
    parser = hsubparser (  command "store"            (info pStore (progDesc "store block"))
                        <> command "new-ref"          (info pNewRef (progDesc "creates reference"))
                        <> command "cat"              (info pCat (progDesc "cat block"))
                        <> command "hash"             (info pHash (progDesc "calculates hash"))
                        <> command "keyring-new"      (info pNewKey (progDesc "generates a new keyring"))
                        <> command "keyring-list"     (info pKeyList (progDesc "list public keys from keyring"))
                        <> command "keyring-key-add"  (info pKeyAdd (progDesc "adds a new keypair into the keyring"))
                        <> command "keyring-key-del"  (info pKeyDel (progDesc "removes a keypair from the keyring"))
                        <> command "show-peer-key"    (info pShowPeerKey (progDesc "show peer key from credential file"))
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
      -- FIXME option to encrypt
      encOps :: Maybe (OptEncryption MerkleEncryptionType) <- optional $ undefined
      pure $ withStore o (runStore ( StoreOpts init file encOps ))

    pCat = do
      o <- common
      hash  <- optional $ strArgument ( metavar "HASH" )
      onlyh <- optional $ flag' True ( short 'H' <> long "hashes-only" <> help "list only block hashes" )
      keyringFile <- optional $ strOption ( long "keyring" <> help "path to keyring file" )
      pure $ withStore o $ runCat $ CatOpts hash (CatHashesOnly <$> onlyh) keyringFile

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
      s <- strArgument ( metavar "PUB-KEY-BAS58" )
      f <- strArgument ( metavar "KEYRING-FILE" )
      pure (runKeyDel s f)

