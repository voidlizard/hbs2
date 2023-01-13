module Main where

import Control.Concurrent.Async
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString qualified as B
import Data.Function
import Data.Functor
import Data.Maybe
import Options.Applicative
import Prettyprinter
import System.Directory
-- import System.FilePath.Posix
import System.IO

import Streaming.Prelude qualified as S
-- import Streaming qualified as S

import HBS2.Storage
import HBS2.Storage.Simple
import HBS2.Prelude
import HBS2.Prelude.Plated
import HBS2.Merkle
import HBS2.Data.Types
import HBS2.Defaults


newtype OptInputFile = OptInputFile { unOptFile :: FilePath }
                       deriving newtype (Eq,Ord,IsString)
                       deriving stock (Data)

newtype MerkleHash = MerkleHash { fromMerkleHash :: Hash HbSync }
                     deriving newtype (Eq,Ord,IsString,Pretty)
                     deriving stock (Data,Generic)


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


readChunked :: MonadIO m => Handle -> Int -> S.Stream (S.Of ByteString) m ()
readChunked handle size = fuu
  where
  fuu = fix \next -> do
    chunk <- liftIO do
      B.hGet handle size
    unless (B.null chunk) do
      S.yield chunk
      next


runCat :: Data opts => opts -> SimpleStorage HbSync -> IO ()
runCat opts ss = do

  let honly = or [ x | CatHashesOnly x <- universeBi opts  ]

  void $ runMaybeT $ do

    mhash <- MaybeT $ pure $ uniLastMay @MerkleHash opts <&> fromMerkleHash

    liftIO $ walkMerkle mhash (getBlock ss) $ \(hr :: [HashRef]) -> do
               forM_ hr $ \(HashRef h) -> do
                 if honly then do
                   print $ pretty h
                 else do
                   mblk <- getBlock ss h
                   case mblk of
                     Nothing  -> error $ show $ "missed block: " <+> pretty h
                     Just blk -> LBS.putStr blk


runStore :: Data opts => opts -> SimpleStorage HbSync -> IO ()

runStore opts ss | justInit = do
  putStrLn "initialized"

  where
    justInit = maybe False fromOptInit (uniLastMay @OptInit opts)


runStore opts ss = do

  let fname = uniLastMay @OptInputFile opts

  handle <- maybe (pure stdin) (flip openFile ReadMode . unOptFile) fname

  hashes <- readChunked handle (fromIntegral defBlockSize) -- FIXME: to settings!
                & S.mapM (\blk -> enqueueBlock ss (LBS.fromStrict blk) >> pure blk)
                & S.map (HashRef . hashObject)
                & S.toList_

  let pt = toPTree (MaxSize 8192) (MaxNum 8192) hashes -- FIXME: settings

  root <- makeMerkle 0 pt $ \(h,_,bs) -> void $ putBlock ss bs

  print $ "merkle-root: " <+> pretty root

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
    parser = hsubparser (  command "store"  (info pStore (progDesc "store block"))
                        <> command "cat"    (info pCat (progDesc "cat block"))
                        )

    common = do
      pure ()

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

