{-# Language RecordWildCards #-}
module NCQ3 where

import HBS2.Prelude.Plated
import HBS2.OrDie
import HBS2.Hash
import HBS2.Data.Types.Refs
import HBS2.Misc.PrettyStuff
import HBS2.Clock
import HBS2.Merkle
import HBS2.Polling

import HBS2.Storage
import HBS2.Storage.Simple
import HBS2.Storage.Operations.ByteString
import HBS2.Storage.NCQ3
import HBS2.Storage.NCQ3.Internal.Files

import HBS2.System.Logger.Simple.ANSI

import HBS2.Data.Log.Structured.SD
import HBS2.Data.Log.Structured.NCQ

import HBS2.CLI.Run.Internal.Merkle

import Data.Config.Suckless.Syntax
import Data.Config.Suckless.Script as SC
import Data.Config.Suckless.System

import NCQTestCommon

import Data.ByteString qualified as BS
import Data.Ord
import Data.Set qualified as Set
import System.Random.MWC as MWC
import UnliftIO


ncq3Tests :: forall m . MonadUnliftIO m => MakeDictM C m ()
ncq3Tests = do
  entry $ bindMatch "test:ncq3:start-stop" $ nil_ $ \e ->do
      let (opts,args) = splitOpts [] e
      let num = headDef 1000 [ fromIntegral n | LitIntVal n <- args ]
      g <- liftIO MWC.createSystemRandom
      runTest $ \TestEnv{..} -> do
        ncqWithStorage3 testEnvDir $ \sto -> do
           notice "start/stop ncq3 storage / write 1000 blocks"
           replicateM_ num do
             n <- liftIO $ uniformRM (1024, 256*1024) g
             bs <- liftIO $ genRandomBS g n
             ncqPutBS sto (Just B) Nothing bs

  entry $ bindMatch "test:ncq3:write-reopen" $ nil_ $ \e ->do
      let (opts,args) = splitOpts [] e
      let num = headDef 1000 [ fromIntegral n | LitIntVal n <- args ]
      g <- liftIO MWC.createSystemRandom
      runTest $ \TestEnv{..} -> do

        pending <- ncqWithStorage3 testEnvDir $ \sto -> do
           notice $ "write" <+> pretty num <+> "blocks"
           replicateM_ num do
             n <- liftIO $ uniformRM (1024, 256*1024) g
             bs <- liftIO $ genRandomBS g n
             ncqPutBS sto (Just B) Nothing bs

           fa <- readTVarIO (ncqState sto) <&> ncqStateFacts

           pure $ [ (ncqGetFileName sto (toFileName k),s) | P (PData k s) <- Set.toList fa ]
                           & maximumByMay (comparing snd)

        for_ pending $ \(dataFile,_) -> do
           n <- liftIO $ uniformRM (1, 16*1024) g
           bss <- liftIO $ genRandomBS g n
           notice $ "CORRUPTING PENDING FILE" <+> pretty n <+> pretty dataFile
           liftIO $ BS.appendFile dataFile bss

        notice $ "reopen"
        ncqWithStorage3 testEnvDir $ \sto -> do
          pause @'Seconds 2
          notice $ "done"

