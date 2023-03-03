{-# Language TemplateHaskell #-}
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


data RunImportOpts =
  RunImportOpts
  { _runImportDry    :: Maybe Bool
  , _runImportRefVal :: Maybe HashRef
  }

makeLenses 'RunImportOpts

isRunImportDry :: RunImportOpts -> Bool
isRunImportDry o = view runImportDry o == Just True

runImport :: MonadIO m
           => RunImportOpts
           -> HashRef
           -> App m ()

runImport o ref = do

  -- FIXME: readRefValue
  let rfv = view runImportRefVal o
  root <- pure rfv `orDie` "ref value not set"

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

  -- shutUp

  -- liftIO $ LBS.putStr headBlk

  dbPath <- makeDbPath ref
  db <- dbEnv dbPath

  withDB db stateInit

  let rest = drop 1 entries

  ae <- ask

  withDB db $ transactional $ do

    for_ rest $ \r -> do

      gh <- stateGetGitHash r <&> isJust

      unless gh do

        blk <- withApp ae $ readBlock r `orDie` "empty data block"

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

        -- TODO: backlog-sha1-might-be-verified-as-well
        --   Можно проверять соответствие sha1 хэша объекта,
        --   что бы защищаться от мусорных/битых коммитов.
        --   Но это затратно. Это так же можно делать при clone/fetch,
        --   там мы все равно читаем объект.

        case catMaybes hm of
          [(t,sha1)] -> do
            info $ pretty t <+> pretty sha1

            unless (isRunImportDry o) do
              statePutHash t sha1 r

          _          -> err $ "skipping bad object" <+> pretty r


