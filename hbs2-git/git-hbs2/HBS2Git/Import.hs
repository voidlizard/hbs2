module HBS2Git.Import where


import HBS2.Prelude.Plated
import HBS2.Data.Types.Refs
import HBS2.OrDie
import HBS2.System.Logger.Simple
import HBS2.Merkle
import HBS2.Hash
import HBS2.Data.Detect hiding (Blob)

import Data.Config.Suckless

import HBS2.Git.Local

import HBS2Git.App
import HBS2Git.State

import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue qualified as Q
import Control.Monad.Reader
import Data.Foldable (for_)
import Data.Maybe
import Data.Text qualified as Text
import Lens.Micro.Platform
import System.Exit


runImport :: MonadIO m => HashRef -> Maybe HashRef-> App m ()
runImport ref rfv = do

  -- FIXME: readRefValue
  root <- pure rfv `orDie` "ref value not set"      -- readRefValue ref

  q <- liftIO newTQueueIO

  let walk h = walkMerkle h (readBlock . HashRef) $ \(hr :: Either (Hash HbSync) [HashRef]) -> do
        case hr of
          Left hx -> liftIO $ die $ show $ pretty "missed block:" <+> pretty hx
          Right (hrr :: [HashRef]) -> do
             forM_ hrr $ liftIO . atomically . Q.writeTQueue q

  walk (fromHashRef root)

  entries <- liftIO $ atomically $ Q.flushTQueue q

  hd <- pure (headMay entries) `orDie` "no head block found"

  -- TODO: what-if-metadata-is-really-big?
  hdData <- readBlock hd `orDie` "empty head block"

  let hdBlk = tryDetect (fromHashRef hd) hdData

  case hdBlk of
    MerkleAnn{} -> pure ()
    _ -> liftIO $ die "invalid head block format"

  let meta = headDef "" [ Text.unpack s | ShortMetadata s <- universeBi hdBlk ]

  syn <- liftIO $ parseTop meta & either (const $ die "invalid head block meta") pure

  let app syn = headDef False
               [ True
               | ListVal @C (Key "application:" [SymbolVal "hbs2-git"]) <- syn
               ]

  let hdd = headDef False
            [ True
            | ListVal @C (Key "type:" [SymbolVal "head"]) <- syn
            ]


  unless ( app syn  && hdd ) do
    liftIO $ die "invalid head block meta"

  headBlk <- readObject hd `orDie` "empty head block data"

  -- shutUp

  -- liftIO $ LBS.putStr headBlk

  dbPath <- makeDbPath ref
  db <- dbEnv dbPath

  withDB db stateInit

  let rest = drop 1 entries

  ae <- ask

  withDB db do

    for_ rest $ \r -> do

      gh <- stateGetGitHash r <&> isJust

      unless gh do

        blk <- withApp ae $ readBlock r `orDie` "empty data block"

        let what = tryDetect (fromHashRef r) blk

        let short = headDef "" [ s | ShortMetadata s <- universeBi what ]

        let fields = Text.lines short & fmap Text.words

        hm <- forM fields $ \case
                ["type:", "blob", x]   -> pure $ Just (Blob, fromString (Text.unpack x))
                ["type:", "commit", x] -> pure $ Just (Commit, fromString (Text.unpack x))
                ["type:", "tree", x]   -> pure $ Just (Tree, fromString (Text.unpack x))
                _                      -> pure Nothing

        -- trace $ pretty hm

        case catMaybes hm of
          [(t,sha1)] -> notice $ pretty t <+> pretty sha1
          _          -> err $ "bad object" <+> pretty r

