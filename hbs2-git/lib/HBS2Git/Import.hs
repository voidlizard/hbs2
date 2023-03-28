{-# Language TemplateHaskell #-}
module HBS2Git.Import where

import HBS2.Prelude.Plated
import HBS2.Data.Types.Refs
import HBS2.OrDie
import HBS2.System.Logger.Simple
import HBS2.Merkle
import HBS2.Hash
import HBS2.Net.Proto.RefLog
import Text.InterpolatedString.Perl6 (qc)
import HBS2.Data.Detect hiding (Blob)

import Data.Config.Suckless

import HBS2.Git.Local

import HBS2Git.App
import HBS2Git.State

import Control.Monad.Trans.Maybe
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue qualified as Q
import Control.Monad.Reader
import Data.Foldable (for_)
import Data.Maybe
import Data.Text qualified as Text
import Data.ByteString.Lazy qualified as LBS
import Lens.Micro.Platform
-- import System.Exit
import Codec.Serialise
import Control.Monad.Catch

data RunImportOpts =
  RunImportOpts
  { _runImportDry    :: Maybe Bool
  , _runImportRefVal :: Maybe HashRef
  }

makeLenses 'RunImportOpts

isRunImportDry :: RunImportOpts -> Bool
isRunImportDry o = view runImportDry o == Just True


walkHashes q h = walkMerkle h (readBlock . HashRef) $ \(hr :: Either (Hash HbSync) [HashRef]) -> do
  case hr of
    Left hx -> die $ show $ pretty "missed block:" <+> pretty hx
    Right (hrr :: [HashRef]) -> do
       forM_ hrr $ liftIO . atomically . Q.writeTQueue q

importRefLog :: (MonadIO m, HasCatAPI m) => DBEnv -> RepoRef -> m ()
importRefLog db ref = do

  logRoot <- readRef ref `orDie` [qc|can't read ref {pretty ref}|]

  trace $ pretty logRoot

  logQ <- liftIO newTQueueIO
  walkHashes logQ (fromHashRef logRoot)

  entries <- liftIO $ atomically $ flushTQueue logQ

  forM_ entries $ \e -> do

    missed <- readBlock e <&> isNothing

    when missed do
      debug $ "MISSED BLOCK" <+> pretty e

    runMaybeT $ do
      bs <- MaybeT $ readBlock e
      refupd <- MaybeT $ pure $ deserialiseOrFail @(RefLogUpdate Schema) bs & either (const Nothing) Just
      e <- MaybeT $ pure $ deserialiseOrFail (LBS.fromStrict $ view refLogUpdData refupd) & either (const Nothing) Just
      let (SequentialRef n (AnnotatedHashRef _ h)) = e
      withDB db $ stateUpdateRefLog n h

  new <- withDB db stateGetHead <&> isNothing

  when new do
    pure ()

importObjects :: (MonadIO m, MonadCatch m, HasCatAPI m) => DBEnv -> HashRef -> m ()
importObjects db root = do

  q <- liftIO newTQueueIO

  walkHashes q (fromHashRef root)

  entries <- liftIO $ atomically $ Q.flushTQueue q

  hd <- pure (headMay entries) `orDie` "no head block found"

  -- TODO: what-if-metadata-is-really-big?
  hdData <- readBlock hd `orDie` "empty head block"

  let hdBlk = tryDetect (fromHashRef hd) hdData

  let meta = headDef "" [ Text.unpack s | ShortMetadata s <- universeBi hdBlk ]

  syn <- liftIO $ parseTop meta & either (const $ die "invalid head block meta") pure

  let app sy = headDef False
               [ True
               | ListVal @C (Key "application:" [SymbolVal "hbs2-git"]) <- sy
               ]

  let hdd = headDef False
            [ True
            | ListVal @C (Key "type:" [SymbolVal "head"]) <- syn
            ]

  unless ( app syn  && hdd ) do
    liftIO $ die "invalid head block meta"

  let rest = drop 1 entries


  withDB db $ transactional $ do

    trace "ABOUT TO UPDATE HEAD"

    statePutHead hd
    statePutImported root hd

    mon <- newProgressMonitor "importing objects" (length rest)

    for_ rest $ \r -> do

      updateProgress mon 1

      gh <- stateGetGitHash r <&> isJust

      unless gh do

        blk <- lift $ readBlock r `orDie` "empty data block"

        let what = tryDetect (fromHashRef r) blk

        let short = headDef "" [ s | ShortMetadata s <- universeBi what ]

        let fields = Text.lines short & fmap Text.words

        let fromTxt =  fromString . Text.unpack
        let fromRec t = Just . (t,) . fromTxt

        hm <- forM fields $ \case
                ["type:", "blob", x]   -> pure $ fromRec Blob x
                ["type:", "commit", x] -> pure $ fromRec Commit x
                ["type:", "tree", x]   -> pure $ fromRec Tree x
                _                      -> pure Nothing

        case catMaybes hm of
          [(t,sha1)] -> do
            trace $ "statePutHash" <+> pretty t <+> pretty sha1

            -- FIXME: return-dry?
            statePutHash t sha1 r

          _          -> err $ "skipping bad object" <+> pretty r

  pure ()


