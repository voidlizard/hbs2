{-# Language ViewPatterns #-}
{-# Language PatternSynonyms #-}
module Main where

import HBS2.Prelude.Plated
import HBS2.OrDie
import HBS2.Storage.Compact

import HBS2.Git.Local
import HBS2.Git.Local.CLI

import HBS2.System.Logger.Simple.ANSI as Exported
import HBS2.System.Dir
import HBS2.Misc.PrettyStuff as Exported

import Data.Config.Suckless.Script

-- import Codec.Compression.GZip as GZ1
-- import Codec.Compression.Zlib.Internal qualified as GZ

import Codec.Compression.BZip as BZ1
import Codec.Compression.BZip.Internal qualified as BZ
-- import Codec.Compression.Zlib.Internal qualified as GZ

import Data.Maybe
import Data.List qualified as L
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString qualified as BS
import Data.ByteString.Lazy (ByteString)
import Text.InterpolatedString.Perl6 (qc)
import Data.HashSet qualified as HS
import Data.HashMap.Strict qualified as HM
import Data.Word

import Streaming.Prelude qualified as S
import System.Exit qualified as Q
import System.Environment qualified as E
import System.Process.Typed
import Control.Monad.Trans.Cont
import System.IO (hPrint,hGetLine)

import Data.List (sortOn)
import Data.Ord (Down(..))

import UnliftIO

{- HLINT ignore "Functor law" -}

type HBS2GitPerks m = (MonadUnliftIO  m)

quit :: MonadUnliftIO m => m ()
quit = liftIO Q.exitSuccess

data GitException =
    CompressionError String
  | InvalidObjectFormat GitObjectType (Maybe GitHash)
  | InvalidGitPack ByteString
  deriving stock (Eq,Show,Typeable,Generic)

instance Exception GitException

data GitTreeEntry =
  GitTreeEntry
  { gitEntryAttribs :: Word16
  , gitEntryType    :: GitObjectType
  , gitEntryHash    :: GitHash
  , gitEntrySize    :: Maybe Word32
  , gitEntryName    :: FilePath
  }

pattern GitTreeEntryView :: GitTreeEntry -> [ByteString]
pattern GitTreeEntryView e <- (isGitLsTreeEntry -> Just e)

isGitLsTreeEntry :: [ByteString] -> Maybe GitTreeEntry
isGitLsTreeEntry = \case
  [sa,st,sh,ss,sn] -> do
    GitTreeEntry <$> readMay @Word16 (LBS8.unpack sa)
                 <*> fromStringMay (LBS8.unpack st)
                 <*> fromStringMay (LBS8.unpack sh)
                 <*> pure (readMay (LBS8.unpack ss))
                 <*> pure (LBS8.unpack sn)

  _ -> Nothing

gitReadTree :: (Pretty what, MonadIO m) => what -> m [GitTreeEntry]
gitReadTree what =
  gitRunCommand [qc|git ls-tree -t -l -r {pretty what}|]
     >>= orThrowPassIO
     <&> fmap LBS8.words . LBS8.lines
     <&> mapMaybe \case
          GitTreeEntryView v -> do
            Just v
          _ -> Nothing
     <&> \s -> HM.elems (HM.fromList [ (gitEntryHash e, e) | e <- s])

gitReadObjectThrow :: (Pretty h, MonadIO m) => GitObjectType -> h -> m ByteString
gitReadObjectThrow t h = do
  gitRunCommand [qc|git cat-file {pretty t} {pretty h}|]
    >>= orThrowPassIO

gitRevParse :: MonadIO m => GitRef -> m (Maybe GitHash)
gitRevParse ref = do
  gitRunCommand [qc|git rev-parse {pretty ref}|]
    >>= orThrowPassIO
    <&> LBS8.words
    <&> maybe Nothing (fromStringMay . LBS8.unpack) . headMay

withGitCat ::  (MonadIO m) => (Process Handle Handle () -> m a) -> m a
withGitCat action = do
  let cmd = "git"
  let args = ["cat-file", "--batch"]
  let config = setStdin createPipe $ setStdout createPipe $ setStderr closed $ proc cmd args
  p <- startProcess config
  action p

newtype Short x = Short x

instance Pretty (Short GitObjectType) where
  pretty = \case
    (Short Tree)   -> "T"
    (Short Blob)   -> "B"
    (Short Commit) -> "C"


sortGitTreeEntries :: [GitTreeEntry] -> [GitTreeEntry]
sortGitTreeEntries = sortOn (\entry -> (gitEntryType entry, gitEntrySize entry))

data UState =
  UHead ByteString

pattern PEntryView :: GitObjectType -> Word32 -> GitHash -> [ByteString]
pattern PEntryView t s h <- ( unpackPEntry -> Just (t,s,h) )

unpackPEntry :: [ByteString] -> Maybe (GitObjectType, Word32, GitHash)
unpackPEntry = \case
  ["C", s, h] -> (Commit,,) <$> readMay (LBS8.unpack s) <*> fromStringMay (LBS8.unpack h)
  ["B", s, h] -> (Blob,,)   <$> readMay (LBS8.unpack s) <*> fromStringMay (LBS8.unpack h)
  ["T", s, h] -> (Tree,,)   <$> readMay (LBS8.unpack s) <*> fromStringMay (LBS8.unpack h)
  _ -> Nothing

theDict :: forall m . ( HBS2GitPerks m
                      -- , HasTimeLimits UNIX (ServiceProto MyRPC UNIX) m
                      ) => Dict C m
theDict = do
  makeDict @C do
    -- TODO: write-man-entries
    myHelpEntry
  where

    myHelpEntry = do
        entry $ bindMatch "--help" $ nil_ $ \case
          HelpEntryBound what -> do
            helpEntry what
            quit

          _ -> helpList False Nothing >> quit

        entry $ bindMatch "git:tree:ls" $ nil_ $ const do
          r <- gitReadTree "HEAD"
          for_ r $ \GitTreeEntry{..} -> do
            liftIO $ print $ pretty gitEntryHash <+> pretty gitEntryType <+> pretty gitEntrySize <+> pretty gitEntryName

        entry $ bindMatch "test:git:tree:pack:dump" $ nil_ $ \case
          [ StringLike fn ] -> do

            content <- BZ.decompress defaultDecompressParams <$> liftIO (LBS8.readFile fn)

            flip fix (UHead content) $ \next -> \case
              UHead "" -> none
              UHead bs -> do

                let (hd,rest) = LBS8.span (/='\n') bs

                case LBS8.words hd of
                  PEntryView t s h -> do
                    liftIO $ print $ pretty h <+> pretty t <+> pretty s
                    next $ UHead (LBS8.drop (1 + fromIntegral s) rest)

                  _ -> throwIO (InvalidGitPack hd)

          _ -> throwIO (BadFormException @C nil)

        entry $ bindMatch "test:git:tree:pack:write" $ nil_ $ \syn -> do

          let co = headDef "HEAD" $ [ GitRef (LBS8.toStrict $ LBS8.pack what) | StringLike what  <- syn ]

          hhead <- gitRevParse co >>= orThrowUser ("can't parse" <+> pretty co)

          co <- gitReadObjectThrow Commit hhead
                 <&> LBS8.lines
                 <&> takeWhile ( not . LBS8.null )
                 <&> LBS8.unpack . LBS8.unlines
                 <&> parseTop
                 >>= orThrowUser "invalid commit format"

          let parents =  [ fromStringMay @GitHash hash
                         | ListVal [ StringLike "parent", StringLike hash ] <- co
                         ] & catMaybes

          skip' <- S.toList_ $ for parents $ \p -> do
                    gitReadTree p <&> fmap gitEntryHash >>= S.each

          let skip = HS.fromList skip'

          r <- gitReadTree hhead
                 <&> L.filter (\GitTreeEntry{..} -> not (HS.member gitEntryHash skip))
                 <&> sortGitTreeEntries

          flip runContT pure do
            ph <- ContT withGitCat
            let ssin  = getStdin ph
            let ssout = getStdout ph

            inq <- newTQueueIO

            atomically do
              writeTQueue inq (Commit, hhead)
              for_ r $ \GitTreeEntry{..} -> do
                writeTQueue inq (gitEntryType, gitEntryHash)

            let
                go ::  Handle -> BZ.CompressStream IO -> IO ()
                go outh (BZ.CompressInputRequired next) = do

                   inO <- atomically $ tryReadTQueue inq

                   case inO of
                    Nothing -> go outh =<< next mempty

                    Just (t,ha) -> do

                      liftIO $ hPrint ssin $ pretty ha
                      liftIO $ hFlush ssin

                      s <- liftIO $ hGetLine ssout

                      case words s of
                        [_,_,s] -> do
                          n <- readMay @Int s & orThrowUser "fuck!"
                          co <- liftIO $ LBS.hGet ssout n
                          void $ liftIO $ hGetLine ssout
                          let header = [qc|{pretty (Short t)} {s} {pretty ha}|]
                          go outh =<< next (LBS.toStrict (LBS8.intercalate "\n" [header, co]))

                        e -> error (show e)

                go outh (BZ.CompressOutputAvailable outchunk next) = do
                   BS.hPut outh outchunk
                   go outh =<< next
                go _ BZ.CompressStreamEnd = return ()

            let params = defaultCompressParams
            let compressStream = BZ.compressIO params

            liftIO $ go stdout compressStream

            none

-- debugPrefix :: LoggerEntry -> LoggerEntry
debugPrefix = toStderr . logPrefix "[debug] "

setupLogger :: MonadIO m => m ()
setupLogger = do
  setLogging @DEBUG  $ toStderr . logPrefix "[debug] "
  setLogging @ERROR  $ toStderr . logPrefix "[error] "
  setLogging @WARN   $ toStderr . logPrefix "[warn] "
  setLogging @NOTICE $ toStdout . logPrefix ""
  pure ()

flushLoggers :: MonadIO m => m ()
flushLoggers = do
  silence

silence :: MonadIO m => m ()
silence = do
  setLoggingOff @DEBUG
  setLoggingOff @ERROR
  setLoggingOff @WARN
  setLoggingOff @NOTICE



main :: IO ()
main = flip runContT pure do

  setupLogger

  ContT $ bracket none $ const do
    silence

  argz <- liftIO $ E.getArgs
  cli <- parseTop (unlines $ unwords <$> splitForms argz)
           & either  (error.show) pure

  let dict = theDict

  void $ lift $ run dict cli


