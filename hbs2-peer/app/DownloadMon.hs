{-# Language TemplateHaskell #-}
module DownloadMon where

import HBS2.Prelude.Plated

import HBS2.Actors.Peer
import PeerTypes

import Data.Functor
import Data.HashMap.Strict qualified as HashMap

import UnliftIO

import Control.Monad
import Lens.Micro.Platform


downloadMonLoop :: ( MonadIO m
                   , HasStorage m
                   )
                => DownloadMonEnv
                -> m ()

downloadMonLoop env = do

  debug "I'm a download monitor"

  -- FIXME: timeout-hardcodes
  let refs = readTVarIO (view downloads env) <&> HashMap.keys <&> fmap (,10)

  polling (Polling 2.5 2) refs $ \ref -> do
    debug $ "DownloadMon. check" <+> pretty ref
    done <- checkDownloaded ref
    when done do
      mbAction <- atomically $ do
        a <- readTVar (view downloads env) <&> HashMap.lookup ref
        modifyTVar (view downloads env) (HashMap.delete ref)
        pure a

      forM_ mbAction $ \action -> liftIO $ async $ liftIO action


