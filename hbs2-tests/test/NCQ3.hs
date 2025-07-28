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

import HBS2.System.Logger.Simple.ANSI

import HBS2.Data.Log.Structured.SD
import HBS2.Storage.NCQ
import HBS2.Storage.NCQ2 as N2
import HBS2.Data.Log.Structured.NCQ

import HBS2.CLI.Run.Internal.Merkle

import Data.Config.Suckless.Syntax
import Data.Config.Suckless.Script as SC
import Data.Config.Suckless.System

import NCQTestCommon

import UnliftIO


ncq3Tests :: forall m . MonadUnliftIO m => MakeDictM C m ()
ncq3Tests = do
  entry $ bindMatch "test:ncq3:start-stop" $ nil_ $ \e ->do
      runTest $ \TestEnv{..} -> do
        ncqWithStorage3 testEnvDir $ \sto -> do
           notice "start/stop ncq3 storage"

