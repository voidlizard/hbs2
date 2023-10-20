{-# Language AllowAmbiguousTypes #-}
{-# Language UndecidableInstances #-}
module HBS2.Peer.Brains where


import HBS2.Prelude.Plated
import HBS2.Net.Proto
import HBS2.Hash

import Data.Word
import HBS2.Data.Types.Refs (HashRef(..))

-- TODO: rename
class HasBrains e a where

  listPolledRefs :: MonadIO m => a -> Maybe String -> m [(PubKey 'Sign (Encryption e), String, Int)]
  listPolledRefs _ _  = pure mempty

  isPolledRef :: MonadIO m => a -> PubKey 'Sign (Encryption e) -> m Bool
  isPolledRef _ _ = pure False

  delPolledRef :: MonadIO m => a -> PubKey 'Sign (Encryption e) -> m ()
  delPolledRef _ _ = pure ()

  addPolledRef :: MonadIO m
               => a
               -> PubKey 'Sign (Encryption e)
               -> String
               -> Int
               -> m ()

  addPolledRef _ _ _ _ = pure ()

  onClientTCPConnected :: MonadIO m => a -> PeerAddr e -> Word64 -> m ()
  onClientTCPConnected _ _ = const none

  getClientTCP :: MonadIO m => a -> m [(PeerAddr e,Word64)]
  getClientTCP = const $ pure mempty

  setActiveTCPSessions :: MonadIO m => a -> [(PeerAddr e, Word64)] -> m ()
  setActiveTCPSessions _ _ = none

  listTCPPexCandidates :: MonadIO m => a -> m [PeerAddr e]
  listTCPPexCandidates _ = pure mempty

  listDownloads :: MonadIO m => a -> m [(HashRef, Integer)]
  listDownloads _ = pure mempty

  delDownload :: MonadIO m => a -> HashRef -> m ()
  delDownload _ _ = pure ()

  onKnownPeers :: MonadIO m => a -> [Peer e] -> m ()
  onKnownPeers _ _ = none

  onBlockSize :: ( MonadIO m
                 , IsPeerAddr e m
                 )
              => a
              -> Peer e
              -> Hash HbSync
              -> Integer
              -> m ()
  onBlockSize _ _ _ _ = none

  onBlockDownloadAttempt :: ( MonadIO m
                            , IsPeerAddr e m
                            )
                         => a
                         -> Peer e
                         -> Hash HbSync
                         -> m ()

  onBlockDownloadAttempt _ _ _ = none

  onBlockDownloaded :: MonadIO m
                    => a
                    -> Peer e
                    -> Hash HbSync
                    -> m ()

  onBlockDownloaded _ _ _ = none

  onBlockPostponed :: MonadIO m
                   => a
                   -> Hash HbSync
                   -> m ()

  onBlockPostponed _ _ = none

  claimBlockCameFrom :: MonadIO m
                     => a
                     -> Maybe (Hash HbSync)
                     -> Hash HbSync
                     -> m ()

  claimBlockCameFrom _ _ _ = none

  shouldPostponeBlock :: MonadIO m
                     => a
                     -> Hash HbSync
                     -> m Bool
  shouldPostponeBlock _ _ = pure False


  shouldDownloadBlock :: MonadIO m
                      => a
                      -> Peer e
                      -> Hash HbSync
                      -> m Bool
  shouldDownloadBlock _ _ _ = pure False

  advisePeersForBlock :: (MonadIO m, FromStringMaybe (PeerAddr e))
                       => a
                       -> Hash HbSync
                       -> m [PeerAddr e]
  advisePeersForBlock  _ _ = pure  mempty

  blockSize :: forall m . MonadIO m
            => a
            -> Peer e
            -> Hash HbSync
            -> m (Maybe Integer)

  blockSize _ _ _ = pure Nothing

  isReflogProcessed :: (MonadIO m)
                    => a
                    -> Hash HbSync
                    -> m Bool

  isReflogProcessed _ _ = pure False

  setReflogProcessed :: (MonadIO m)
                     => a
                     -> Hash HbSync
                     -> m ()

  setReflogProcessed _ _ = pure ()


type NoBrains = ()

instance Pretty (Peer e) => HasBrains e NoBrains  where

data SomeBrains e = forall a . HasBrains e a => SomeBrains a

instance HasBrains e (SomeBrains e) where
  listPolledRefs (SomeBrains a) = listPolledRefs @e a
  isPolledRef (SomeBrains a) = isPolledRef @e a
  delPolledRef (SomeBrains a) = delPolledRef @e a
  addPolledRef (SomeBrains a) = addPolledRef @e a
  onClientTCPConnected (SomeBrains a) = onClientTCPConnected @e a
  getClientTCP (SomeBrains a) = getClientTCP @e a
  setActiveTCPSessions (SomeBrains a) = setActiveTCPSessions @e a
  listTCPPexCandidates (SomeBrains a) = listTCPPexCandidates @e a

  listDownloads (SomeBrains a) = listDownloads @e a
  delDownload (SomeBrains a) = delDownload @e a

  onKnownPeers (SomeBrains a) = onKnownPeers a
  onBlockSize (SomeBrains a) = onBlockSize a
  onBlockDownloadAttempt (SomeBrains a) = onBlockDownloadAttempt a
  onBlockDownloaded (SomeBrains a) = onBlockDownloaded a
  onBlockPostponed (SomeBrains a) = onBlockPostponed @e a
  claimBlockCameFrom (SomeBrains a) = claimBlockCameFrom @e a
  shouldPostponeBlock (SomeBrains a) = shouldPostponeBlock @e a
  shouldDownloadBlock (SomeBrains a) = shouldDownloadBlock @e a
  advisePeersForBlock (SomeBrains a) = advisePeersForBlock @e a
  blockSize (SomeBrains a) = blockSize @e a
  isReflogProcessed (SomeBrains a) = isReflogProcessed @e a
  setReflogProcessed (SomeBrains a) = setReflogProcessed @e a


