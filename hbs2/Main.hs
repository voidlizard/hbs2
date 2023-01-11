module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent.Async
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as LBS
import Data.Function
import Data.Functor
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
import HBS2.Hash
import HBS2.Defaults

newtype HashRef = HashRef (Hash HbSync)
                  deriving newtype (Eq,Ord,IsString,Pretty)
                  deriving stock (Data)

newtype OptInputFile = OptInputFile { unOptFile :: FilePath }
                       deriving newtype (Eq,Ord,IsString)
                       deriving stock (Data)

newtype Opts =
  Opts {
    optInputFile :: Maybe OptInputFile
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

runStore :: Opts -> SimpleStorage HbSync -> IO ()
runStore opts ss = do

  let fname = uniLastMay @OptInputFile opts

  handle <- maybe (pure stdin) (flip openFile ReadMode . unOptFile) fname

  hashes <- readChunked handle (fromIntegral defBlockSize) -- FIXME: to settings!
                & S.mapM (\blk -> putBlock ss (LBS.fromStrict blk) >> pure blk)
                & S.map hashObject
                & S.map HashRef
                & S.toList_

  let pt = toPTree (MaxSize 2048) (MaxNum 2048) hashes

  -- mapM_ (print . pretty) hashes

  pure ()


withStore :: Data opts => opts -> ( SimpleStorage HbSync -> IO () ) -> IO ()
withStore opts f = do
  xdg <- getXdgDirectory XdgData defStorePath <&> fromString

  let pref = uniLastDef xdg opts :: StoragePrefix
  s <- simpleStorageInit (Just pref)

  storage <- async $ simpleStorageWorker s

  f s

  simpleStorageStop s

  _ <-  waitAnyCatch [storage]

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
                        )

    common = do
      pure ()

    pStore = do
      o <- common
      file <- optional $ strArgument ( metavar "FILE" )
      pure $ withStore o (runStore ( Opts file ))


