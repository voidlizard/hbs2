{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language TemplateHaskell #-}
{-# Language UndecidableInstances #-}
{-# Language MultiWayIf #-}
module PeerTypes where

import HBS2.Actors.Peer
import HBS2.Clock
import HBS2.Defaults
import HBS2.Events
import HBS2.Hash
import HBS2.Net.Proto
import HBS2.Net.Proto.Peer
import HBS2.Net.Proto.BlockInfo
import HBS2.Net.Proto.Definition
import HBS2.Net.Proto.Sessions
import HBS2.Prelude.Plated
import HBS2.Storage
import HBS2.Net.PeerLocator
import HBS2.System.Logger.Simple

-- import PeerInfo
import Brains

import Data.Foldable (for_)
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad.Reader
import Data.ByteString.Lazy (ByteString)
import Data.Cache (Cache)
import Data.Cache qualified as Cache
import Data.HashSet (HashSet)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Maybe
import Lens.Micro.Platform
import Data.Hashable
import Type.Reflection
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)


data PeerInfo e =
  PeerInfo
  { _peerBurst          :: TVar Int
  , _peerBurstMax       :: TVar (Maybe Int)
  , _peerBurstSet       :: TVar IntSet
  , _peerErrors         :: TVar Int
  , _peerErrorsLast     :: TVar Int
  , _peerErrorsPerSec   :: TVar Int
  , _peerLastWatched    :: TVar TimeSpec
  , _peerDownloaded     :: TVar Int
  , _peerDownloadedLast :: TVar Int
  , _peerPingFailed     :: TVar Int
  , _peerDownloadedBlk  :: TVar Int
  , _peerDownloadFail   :: TVar Int
  , _peerDownloadMiss   :: TVar Int
  , _peerRTTBuffer      :: TVar [Integer] -- ^ Contains a list of the last few round-trip time (RTT) values, measured in nanoseconds.
                                          -- Acts like a circular buffer.
  , _peerHttpApiAddress :: TVar (Either Int (Maybe String))
  }
  deriving stock (Generic,Typeable)

makeLenses 'PeerInfo

newPeerInfo :: MonadIO m => m (PeerInfo e)
newPeerInfo = liftIO do
  PeerInfo <$> newTVarIO defBurst
           <*> newTVarIO Nothing
           <*> newTVarIO mempty
           <*> newTVarIO 0
           <*> newTVarIO 0
           <*> newTVarIO 0
           <*> newTVarIO 0
           <*> newTVarIO 0
           <*> newTVarIO 0
           <*> newTVarIO 0
           <*> newTVarIO 0
           <*> newTVarIO 0
           <*> newTVarIO 0
           <*> newTVarIO []
           <*> newTVarIO (Left 0)

type instance SessionData e (PeerInfo e) = PeerInfo e

newtype instance SessionKey e  (PeerInfo e) =
  PeerInfoKey (Peer e)

deriving newtype instance Hashable (SessionKey L4Proto (PeerInfo L4Proto))
deriving stock instance Eq (SessionKey L4Proto (PeerInfo L4Proto))

-- FIXME: this?
instance Expires (SessionKey L4Proto (PeerInfo L4Proto)) where
  expiresIn = const (Just defCookieTimeoutSec)



type MyPeer e = ( Eq (Peer e)
                , Hashable (Peer e)
                , Pretty (Peer e)
                , HasPeer e
                , Block ByteString ~ ByteString
                )

data DownloadReq e

data DownloadAsap e

data instance EventKey e (DownloadReq e) =
  DownloadReqKey
  deriving (Generic,Typeable,Eq)

instance Typeable (DownloadReq e) => Hashable (EventKey e (DownloadReq e)) where
  hashWithSalt salt _ = hashWithSalt salt (someTypeRep p)
    where
      p = Proxy @DownloadReq

newtype instance Event e (DownloadReq e) =
  DownloadReqData (Hash HbSync)
  deriving (Typeable)

instance EventType ( Event e (DownloadReq e) ) where
  isPersistent = True

instance Expires (EventKey e (DownloadReq e)) where
  expiresIn = const Nothing



type DownloadFromPeerStuff e m = ( MyPeer e
                                 , MonadIO m
                                 , Request e (BlockInfo e) m
                                 , Request e (BlockChunks e) m
                                 , MonadReader (PeerEnv e ) m
                                 , PeerMessaging e
                                 , HasProtocol e (BlockInfo e)
                                 , EventListener e (BlockInfo e) m
                                 , EventListener e (BlockChunks e) m
                                 , Sessions e (BlockChunks e) m
                                 , Sessions e (PeerInfo e) m
                                 , Block ByteString ~ ByteString
                                 , HasStorage m
                                 )


calcBursts :: forall a . Integral a => a -> [a] -> [(a,a)]
calcBursts bu pieces = go seed
  where
    seed = fmap (,1) pieces

    go ( (n1,s1) : (n2,s2) : xs )
      | (s1 + s2) <= bu = go ((n1, s1+s2) : xs)
      | otherwise  = (n1,s1) : go ( (n2,s2) : xs)

    go [x] = [x]
    go [] = []

data BlockDownload =
  BlockDownload
  { _sBlockHash      :: Hash HbSync
  , _sBlockSize      :: Size
  , _sBlockChunkSize :: ChunkSize
  , _sBlockChunks    :: TQueue (ChunkNum, ByteString)
  , _sBlockChunks2   :: TVar (IntMap ByteString)
  }
  deriving stock (Typeable)

makeLenses 'BlockDownload

newBlockDownload :: MonadIO m => Hash HbSync -> m BlockDownload
newBlockDownload h = liftIO do
  BlockDownload h 0 0 <$> newTQueueIO <*> newTVarIO mempty


type instance SessionData e (BlockChunks e) = BlockDownload

newtype instance SessionKey e (BlockChunks e) =
  DownloadSessionKey (Peer e, Cookie e)
  deriving stock (Generic,Typeable)

deriving newtype instance Hashable (SessionKey L4Proto (BlockChunks L4Proto))
deriving stock instance Eq (SessionKey L4Proto (BlockChunks L4Proto))

data BlockState =
  BlockState
  { _bsStart         :: TimeSpec
  , _bsReqSizeTimes  :: TVar Int
  , _bsLastSeen      :: TVar TimeSpec
  , _bsHasSize       :: TVar Bool
  }

makeLenses 'BlockState



data DownloadEnv e =
  DownloadEnv
  { _blockInQ       :: TVar   (HashMap (Hash HbSync) ())
  , _blockPostponed :: TVar   (HashMap (Hash HbSync) () )
  , _blockPostponedTo :: Cache  (Hash HbSync) ()
  , _blockDelayTo   :: TQueue (Hash HbSync)
  , _blockProposed  :: Cache (Hash HbSync, Peer e) ()
  , _downloadBrains :: SomeBrains e
  }

makeLenses 'DownloadEnv


newDownloadEnv :: (MonadIO m, MyPeer e, HasBrains e brains) => brains -> m (DownloadEnv e)
newDownloadEnv brains = liftIO do
  DownloadEnv <$> newTVarIO mempty
              <*> newTVarIO mempty
              <*> Cache.newCache (Just defBlockBanTime)
              <*> newTQueueIO
              <*> Cache.newCache (Just (toTimeSpec (2 :: Timeout 'Seconds)))
              <*> pure (SomeBrains brains)

newtype BlockDownloadM e m a =
  BlockDownloadM { fromBlockDownloadM :: ReaderT (DownloadEnv e) m a }
  deriving newtype ( Functor
                   , Applicative
                   , Monad
                   , MonadIO
                   , MonadReader (DownloadEnv e)
                   , MonadTrans
                   )

withDownload :: (MyPeer e, HasPeerLocator e m, MonadIO m) => DownloadEnv e -> BlockDownloadM e m a -> m a
withDownload e m = runReaderT ( fromBlockDownloadM m ) e


isBlockHereCached :: forall e m . ( MyPeer e
                                  , MonadIO m
                                  , HasStorage m
                                  )
            => Hash HbSync -> BlockDownloadM e m Bool

isBlockHereCached h = do
  sto <- lift getStorage
  liftIO $ hasBlock sto h <&> isJust


type DownloadConstr e m = ( MyPeer e
                          , MonadIO m
                          , HasPeerLocator e (BlockDownloadM e m)
                          , HasStorage m -- (BlockDownloadM e m)
                          , Block ByteString ~ ByteString
                          )

addDownload :: forall e m . ( DownloadConstr e m
                            )
            => Maybe (Hash HbSync)
            -> Hash HbSync
            -> BlockDownloadM e m ()

addDownload mbh h = do

  tinq <- asks (view blockInQ)
  brains <- asks (view downloadBrains)
  here <- isBlockHereCached h

  if here then do
    removeFromWip h
  else do
    maybe1 mbh none $ \hp -> claimBlockCameFrom @e brains hp h
    liftIO $ atomically $ modifyTVar tinq $ HashMap.insert h ()

postponedNum :: forall e  m . (MyPeer e, MonadIO m) => BlockDownloadM e m Int
postponedNum = do
  po <- asks (view blockPostponed)
  liftIO $ readTVarIO po <&> HashMap.size

isPostponed :: forall e  m . (MyPeer e, MonadIO m) => Hash HbSync -> BlockDownloadM e m Bool
isPostponed h = do
  po <- asks (view blockPostponed) >>= liftIO . readTVarIO
  pure $ HashMap.member  h po


delayLittleBit :: forall e  m . (MyPeer e, MonadIO m) => Hash HbSync -> BlockDownloadM e m ()
delayLittleBit h = do
  q <- asks (view blockDelayTo)
  liftIO $ atomically $ writeTQueue q h

postponeBlock :: forall e  m . (MyPeer e, MonadIO m) => Hash HbSync -> BlockDownloadM e m ()
postponeBlock h = do

  brains <- asks (view downloadBrains)
  po <- asks (view blockPostponed)
  tto <- asks (view blockPostponedTo)
  tinq <- asks (view blockInQ)


  liftIO $ do
    postponed <- atomically $ do
      already <- readTVar po <&> HashMap.member h
      unless already do
        modifyTVar tinq $ HashMap.delete h
        modifyTVar po (HashMap.insert h ())
      pure $ not already

    when postponed do
      Cache.insert tto h ()
      onBlockPostponed @e brains h

unpostponeBlock :: forall e m . (DownloadConstr e m) => Hash HbSync -> BlockDownloadM e m ()
unpostponeBlock h = do

  po <- asks (view blockPostponed)
  tto <- asks (view blockPostponedTo)

  liftIO $ do
      atomically $ modifyTVar po (HashMap.delete h)
      Cache.delete tto h

  trace $ "unpostponeBlock" <+> pretty h
  addDownload @e mzero h

removeFromWip :: (MyPeer e, MonadIO m) => Hash HbSync -> BlockDownloadM e m ()
removeFromWip h = do
  tinq <- asks (view blockInQ)
  liftIO $ atomically $ do
    modifyTVar' tinq (HashMap.delete h)

failedDownload :: forall e m . ( MyPeer e
                               , MonadIO m
                               , HasPeer e
                               , HasPeerLocator e (BlockDownloadM e m)
                               , HasStorage m
                               )
               => Peer e
               -> Hash HbSync
               -> BlockDownloadM e m ()

failedDownload p h = do
  trace $ "failedDownload" <+> pretty p <+> pretty h
  addDownload mzero h
  -- FIXME: brains-download-fail


forKnownPeers :: forall e  m . ( MonadIO m
                               , HasPeerLocator e m
                               , Sessions e (KnownPeer e) m
                               , HasPeer e
                               )
               =>  ( Peer e -> PeerData e -> m () ) -> m ()
forKnownPeers m = do
  pl <- getPeerLocator @e
  pips <- knownPeers @e pl
  for_ pips $ \p -> do
    pd' <- find (KnownPeerKey p) id
    maybe1 pd' (pure ()) (m p)

getKnownPeers :: forall e  m . ( MonadIO m
                               , HasPeerLocator e m
                               , Sessions e (KnownPeer e) m
                               , HasPeer e
                               )
               =>  m [Peer e]

getKnownPeers  = do
  pl <- getPeerLocator @e
  pips <- knownPeers @e pl
  r <- forM pips $ \p -> do
    pd' <- find (KnownPeerKey p) id
    maybe1 pd' (pure mempty) (const $ pure [p])
  pure $ mconcat r


