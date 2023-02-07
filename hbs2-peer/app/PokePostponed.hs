module PokePostponed where

import HBS2.Prelude.Plated
import HBS2.Clock
import HBS2.Actors.Peer
import HBS2.Net.Proto.Peer
import HBS2.Events

import HBS2.System.Logger.Simple

import PeerTypes

import Data.Foldable (for_)
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad.Reader
import Data.Cache qualified as Cache
import Lens.Micro.Platform
import Numeric ( showGFloat )
import Prettyprinter

pokePostponed :: forall e m . ( MonadIO m
                              , EventListener e (PeerHandshake e) m
                              , MyPeer e
                              )
              => PeerEnv e
              -> BlockDownloadM e m ()

pokePostponed penv = do

  env <- ask

  waitQ <- liftIO $ newTBQueueIO 1

  busy <- liftIO $ newTVarIO False

  cache <- asks (view blockPostponed)

  lift $ subscribe @e AnyKnownPeerEventKey $ \(KnownPeerEvent{}) -> do
    cant <- liftIO $ readTVarIO busy
    unless cant $ do
      debug "AnyKnownPeerEventKey"
      mt <- liftIO $ atomically $ isEmptyTBQueue waitQ
      when mt do
        liftIO $ atomically $ writeTBQueue waitQ ()

  forever do
    -- FIXME: to defaults!
    r <- liftIO $ race ( pause @'Seconds 60 ) ( atomically $ readTBQueue waitQ )

    void $ liftIO $ atomically $ flushTBQueue waitQ

    liftIO $ atomically $ writeTVar busy True

    void $ liftIO $ async $ do
      pause @'Seconds 30
      atomically $ writeTVar busy False

    let allBack = either (const False) (const True) r

    blks <- liftIO $ Cache.toList cache

    w <- calcWaitTime

    debug $ "tossPostponed" <+> pretty (showGFloat (Just 2) w "")
                            <+> pretty (length blks)

    for_ blks $ \case
      (k,_,Nothing) | not allBack -> pure ()
                    | otherwise   -> pushBack cache k
      (k,_,Just{})  -> pushBack cache k

  where
    pushBack cache k = do
        w <- calcWaitTime
        liftIO $ Cache.delete cache k
        st <- getBlockState k
        t0 <- liftIO $ getTime MonotonicCoarse
        setBlockState k ( set bsStart t0
                        . set bsState Initial
                        . set bsWipTo w
                         $ st
                        )
        debug $ "returning block to downloads" <+> pretty k
        addDownload k
