{-# Language AllowAmbiguousTypes #-}
{-# Language UndecidableInstances #-}
{-# Language TemplateHaskell #-}
module Brains where

import HBS2.Prelude.Plated
import HBS2.Clock
import HBS2.Net.Proto
import HBS2.Hash

import HBS2.System.Logger.Simple

import Data.Maybe
import Control.Monad
import Control.Concurrent.STM
import Data.HashMap.Strict
import Lens.Micro.Platform
import Data.HashMap.Strict qualified as HashMap
import Data.List qualified as List

class HasBrains e a where

  onKnownPeers :: MonadIO m => a -> [Peer e] -> m ()

  onBlockDownloadAttempt :: MonadIO m => a -> Peer e -> Hash HbSync -> m ()

  onBlockDownloaded :: MonadIO m
                    => a
                    -> Peer e
                    -> Hash HbSync
                    -> m ()

  onBlockPostponed :: MonadIO m
                   => a
                   -> Hash HbSync
                   -> m ()

  claimBlockCameFrom :: MonadIO m
                     => a
                     -> Hash HbSync
                     -> Hash HbSync
                     -> m ()

  shouldPostponeBlock :: MonadIO m
                     => a
                     -> Hash HbSync
                     -> m Bool


  shouldDownloadBlock :: MonadIO m
                      => a
                      -> Peer e
                      -> Hash HbSync
                      -> m Bool

type NoBrains = ()

instance Pretty (Peer e) => HasBrains e NoBrains  where

  onKnownPeers _ ps = pure ()

  onBlockDownloadAttempt _ p h = do
    pure ()

  onBlockDownloaded _ p h = do
    pure ()

  onBlockPostponed _ h = do
    pure ()

  claimBlockCameFrom _ _ _ = do pure ()

  shouldPostponeBlock _ _ = pure False

  shouldDownloadBlock _ _ _ = pure True

data SomeBrains e = forall a . HasBrains e a => SomeBrains a

instance HasBrains e (SomeBrains e) where
  onKnownPeers (SomeBrains a) = onKnownPeers a
  onBlockDownloadAttempt (SomeBrains a) = onBlockDownloadAttempt a
  onBlockDownloaded (SomeBrains a) = onBlockDownloaded a
  onBlockPostponed (SomeBrains a) = onBlockPostponed @e a
  claimBlockCameFrom (SomeBrains a) = claimBlockCameFrom @e a
  shouldPostponeBlock (SomeBrains a) = shouldPostponeBlock @e a
  shouldDownloadBlock (SomeBrains a) = shouldDownloadBlock @e a

data BasicBrains e =
  BasicBrains
  { _brainsPeers        :: TVar [Peer e]
  , _brainsPostponeDown :: TVar (HashMap (Peer e, Hash HbSync) Int )
  }

makeLenses 'BasicBrains


cleanupPostponed :: MonadIO m => BasicBrains e -> Hash HbSync -> m ()
cleanupPostponed b h =  do
    let po = view brainsPostponeDown b
    let flt (_,h1) _ = h1 /= h
    liftIO $ atomically $ modifyTVar po $ HashMap.filterWithKey flt

instance Hashable (Peer e) => HasBrains e (BasicBrains e) where

  onKnownPeers br ps = do
    trace "BRAINS: onKnownPeers"
    let tv = view brainsPeers br
    liftIO $ atomically $ writeTVar tv ps

  onBlockDownloadAttempt b peer h = do
    trace "BRAINS: onBlockDownloadAttempt"
    let doAlter = HashMap.alter (maybe (Just 0) (Just . succ)) (peer,h)
    liftIO $ atomically $ modifyTVar (view brainsPostponeDown b) doAlter

  onBlockDownloaded b p h = do
    trace "BRAINS: onBlockDownloaded"
    cleanupPostponed b h

  onBlockPostponed b h  = do
    trace "BRAINS: onBlockPostponed"
    cleanupPostponed b h

  claimBlockCameFrom _ _ _ = do
    trace "BRAINS: claimBlockCameFrom"

  shouldPostponeBlock b h = do
    peers <- liftIO $ readTVarIO (view brainsPeers b)
    downs <- liftIO $ readTVarIO (view brainsPostponeDown b)

    r <- forM peers $ \p -> do
           let v = HashMap.lookup (p,h) downs & fromMaybe 0 & (<2)
           pure [v]

    let postpone = not (List.null r || or (mconcat r) )

    pure postpone

  shouldDownloadBlock b p h = do
    downs <- liftIO $ readTVarIO (view brainsPostponeDown b)
    pure $ HashMap.lookup (p,h) downs & fromMaybe 0 & (<2)

newBasicBrains :: forall e m . (Hashable (Peer e), MonadIO m) => m (BasicBrains e)
newBasicBrains = liftIO do
  BasicBrains <$> newTVarIO mempty
              <*> newTVarIO mempty

runBasicBrains :: MonadIO m => BasicBrains e -> m ()
runBasicBrains brains = forever do
  pause @'Seconds 20
  debug "BRAINS!"
  pure()


