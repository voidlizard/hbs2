{-# Language AllowAmbiguousTypes #-}
module DownloadQ where

import HBS2.Prelude
import HBS2.Clock
import HBS2.Events
import HBS2.Data.Types.Refs
import HBS2.Net.PeerLocator
import HBS2.Peer.Brains
import HBS2.Storage
import HBS2.Storage.Operations.Missed
import HBS2.System.Logger.Simple

import PeerTypes
import PeerConfig

import Data.Foldable
import Control.Monad
import Lens.Micro.Platform

downloadQueue :: forall e m . ( MyPeer e
                              , DownloadFromPeerStuff e m
                              , HasPeerLocator e (BlockDownloadM e m)
                              , HasPeerLocator e m
                              , EventListener e (DownloadReq e) m
                              , HasStorage m
                              ) => PeerConfig
                                -> SomeBrains e
                                -> DownloadEnv e -> m ()

downloadQueue _ brains denv = do
  debug "DownloadQ started"

  down <- listDownloads @e brains
  sto <- getStorage

  withDownload denv do
    forM_ down $ \(HashRef h,_)  -> do
      missed <- findMissedBlocks sto (HashRef h)
      for_ missed $ \h -> do
        debug $ "DownloadQ:" <+> pretty h
        addDownload mzero (fromHashRef h)

  -- FIXME: timeout-hardcodes
  let refs = listDownloads @e brains <&> fmap (set _2 10)

  polling (Polling 5 10) refs $ \ref -> do
    missed <- findMissedBlocks sto ref

    debug $ "DownloadQ. check" <+> pretty ref <+> pretty (length missed)

    when (null missed) do
      delDownload @e brains ref


