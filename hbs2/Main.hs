module Main where

import HBS2.Storage
import HBS2.Storage.Simple
import HBS2.Storage.Simple.Extra
import HBS2.Prelude
import HBS2.Prelude.Plated
import HBS2.Merkle
import HBS2.Data.Types
import HBS2.Data.Detect
import HBS2.Defaults


import Control.Concurrent.Async
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.ByteString.Lazy qualified as LBS
import Data.Either
import Data.Function
import Data.Functor
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Options.Applicative
import Prettyprinter
import System.Directory
import Data.Maybe
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

data StoreOpts =
  StoreOpts
  {  storeInit      :: Maybe OptInit
  ,  storeInputFile :: Maybe OptInputFile
  }
  deriving stock (Data)

data CatOpts =
  CatOpts
  { catMerkleHash :: Maybe MerkleHash
  , catHashesOnly :: Maybe CatHashesOnly
  }
  deriving stock (Data)


newtype NewRefOpts =
  NewRefOpts
  { newRefMerkle :: Bool
  }
  deriving stock (Data)

runCat :: Data opts => opts -> SimpleStorage HbSync -> IO ()
runCat opts ss = do

  let honly = or [ x | CatHashesOnly x <- universeBi opts  ]

  void $ runMaybeT $ do

    mhash <- MaybeT $ pure $ uniLastMay @MerkleHash opts <&> fromMerkleHash

    obj <- MaybeT $ getBlock ss mhash

    let q = tryDetect mhash obj

    liftIO $ do

      let walk h = walkMerkle h (getBlock ss) $ \(hr :: [HashRef]) -> do
           forM_ hr $ \(HashRef h) -> do
             if honly then do
               print $ pretty h
             else do
               mblk <- getBlock ss h
               case mblk of
                 Nothing  -> die $ show $ "missed block: " <+> pretty h
                 Just blk -> LBS.putStr blk


      case q of
        Blob h -> getBlock ss h >>= maybe (die "blob not found") LBS.putStr
        Merkle h -> walk h
        AnnRef h -> do
          let lnk = deserialise @AnnotatedHashRef obj
          let mbHead =  headMay [ h
                                | HashRefMerkle (HashRefObject (HashRef h) _) <- universeBi lnk
                                ]
          maybe (error "empty ref") walk mbHead


runStore :: Data opts => opts -> SimpleStorage HbSync -> IO ()

runStore opts ss | justInit = do
  putStrLn "initialized"

  where
    justInit = maybe False fromOptInit (uniLastMay @OptInit opts)


runStore opts ss = do

  let fname = uniLastMay @OptInputFile opts

  handle <- maybe (pure stdin) (flip openFile ReadMode . unOptFile) fname

  root <- putAsMerkle ss handle

  print $ "merkle-root: " <+> pretty root

runNewRef :: Data opts => opts -> MerkleHash -> SimpleStorage HbSync -> IO ()
runNewRef opts mhash ss = do
  uuid <- UUID.nextRandom <&> (hashObject @HbSync . UUID.toASCIIBytes)
  let href = HashRef (fromMerkleHash mhash)
  let mref = HashRefMerkle (HashRefObject href Nothing)
  let ref = AnnotatedHashRef Nothing mref
  res <- simpleWriteLinkRaw ss uuid (serialise ref)
  print (pretty res)

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
    parser = hsubparser (  command "store"   (info pStore (progDesc "store block"))
                        <> command "new-ref" (info pNewRef (progDesc "creates reference"))
                        <> command "cat"     (info pCat (progDesc "cat block"))
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
      pure $ withStore o (runStore ( StoreOpts init file ))

    pCat = do
      o <- common
      hash  <- optional $ strArgument ( metavar "HASH" )
      onlyh <- optional $ flag' True ( short 'H' <> long "hashes-only" <> help "list only block hashes" )
      pure $ withStore o $ runCat $ CatOpts hash (CatHashesOnly <$> onlyh)

