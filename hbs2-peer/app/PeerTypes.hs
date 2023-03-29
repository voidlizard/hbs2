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
import HBS2.Net.Messaging.UDP (UDP)
import HBS2.Net.Proto
import HBS2.Net.Proto.Peer
import HBS2.Net.Proto.BlockInfo
import HBS2.Net.Proto.Definition
import HBS2.Net.Proto.Sessions
import HBS2.Prelude.Plated
import HBS2.Storage
import HBS2.Net.PeerLocator
import HBS2.System.Logger.Simple

import PeerInfo
import Brains

import Data.Foldable (for_)
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad.Reader
import Data.ByteString.Lazy (ByteString)
import Data.Cache (Cache)
import Data.Cache qualified as Cache
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Maybe
import Lens.Micro.Platform
import Data.Hashable
import Type.Reflection
import Numeric (showGFloat)


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
  }
  deriving stock (Typeable)

makeLenses 'BlockDownload

newBlockDownload :: MonadIO m => Hash HbSync -> m BlockDownload
newBlockDownload h = do
  BlockDownload h 0 0 <$> liftIO newTQueueIO


type instance SessionData e (BlockChunks e) = BlockDownload

newtype instance SessionKey e (BlockChunks e) =
  DownloadSessionKey (Peer e, Cookie e)
  deriving stock (Generic,Typeable)

deriving newtype instance Hashable (SessionKey UDP (BlockChunks UDP))
deriving stock instance Eq (SessionKey UDP (BlockChunks UDP))

data BlockState =
  BlockState
  { _bsStart         :: TimeSpec
  , _bsReqSizeTimes  :: TVar Int
  , _bsLastSeen      :: TVar TimeSpec
  , _bsHasSize       :: TVar Bool
  }

makeLenses 'BlockState


data PeerTask e = DoDownload

data PeerThread e =
  PeerThread
  { _peerThreadAsync   :: Async ()
  , _peerThreadMailbox :: TQueue (PeerTask e)
  }

makeLenses 'PeerThread

data DownloadEnv e =
  DownloadEnv
  { _downloadQ      :: TQueue (Hash HbSync)
  , _peerBusy       :: TVar   (HashMap (Peer e) ())
  , _blockPeers     :: TVar   (HashMap (Hash HbSync) (HashMap (Peer e) Integer) )
  , _blockWip       :: Cache  (Hash HbSync) ()
  , _blockState     :: TVar   (HashMap (Hash HbSync) BlockState)
  , _blockInQ       :: TVar   (HashMap (Hash HbSync) ())
  , _peerThreads    :: TVar   (HashMap (Peer e) (PeerThread e))
  , _blockStored    :: Cache  (Hash HbSync) ()
  , _blockPostponed :: TVar   (HashMap (Hash HbSync) () )
  , _blockPostponedTo :: Cache  (Hash HbSync) ()
  , _downloadBrains :: SomeBrains e
  }

makeLenses 'DownloadEnv


newDownloadEnv :: (MonadIO m, MyPeer e, HasBrains e brains) => brains -> m (DownloadEnv e)
newDownloadEnv brains = liftIO do
  DownloadEnv <$> newTQueueIO
              <*> newTVarIO mempty
              <*> newTVarIO mempty
              <*> Cache.newCache (Just defBlockWipTimeout)
              <*> newTVarIO mempty
              <*> newTVarIO mempty
              <*> newTVarIO mempty
              <*> Cache.newCache (Just defBlockWipTimeout)
              <*> newTVarIO mempty
              <*> Cache.newCache (Just defBlockBanTime)
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

setBlockState :: MonadIO m => Hash HbSync -> BlockState -> BlockDownloadM e m ()
setBlockState h s = do
  sh <- asks (view blockState)
  liftIO $ atomically $ modifyTVar' sh (HashMap.insert h s)

setBlockHasSize :: MonadIO m => Hash HbSync -> BlockDownloadM e m ()
setBlockHasSize h = do
  blk <- fetchBlockState h
  liftIO $ atomically $ writeTVar (view bsHasSize blk) True

fetchBlockState :: MonadIO m => Hash HbSync -> BlockDownloadM e m BlockState
fetchBlockState h = do
  sh <- asks (view blockState)
  liftIO do
    now     <- getTimeCoarse
    tvlast  <- newTVarIO now
    tvreq   <- newTVarIO 0
    tvsz    <- newTVarIO False
    let defState = BlockState now tvreq tvlast tvsz
    atomically $ stateTVar sh $ \hm -> case HashMap.lookup h hm of
                                         Nothing -> (defState, HashMap.insert h defState hm)
                                         Just x  -> (x, hm)


delBlockState :: MonadIO m => Hash HbSync -> BlockDownloadM e m ()
delBlockState h = do
  sh <- asks (view blockState)
  liftIO $ atomically $ modifyTVar' sh (HashMap.delete h)

incBlockSizeReqCount :: MonadIO m => Hash HbSync -> BlockDownloadM e m ()
incBlockSizeReqCount h = do
  blk  <- fetchBlockState h
  now  <- liftIO getTimeCoarse
  seen <- liftIO $ readTVarIO (view bsLastSeen blk)
  let elapsed = realToFrac (toNanoSecs (now - seen))  / 1e9
  noSize <- liftIO $ readTVarIO (view bsHasSize blk) <&> not

  when (elapsed > 1.0 && noSize) do
    liftIO $ atomically $ do
      writeTVar (view bsLastSeen blk) now
      modifyTVar (view bsReqSizeTimes blk) succ

isBlockHereCached :: forall e m . ( MyPeer e
                                  , MonadIO m
                                  , HasStorage m
                                  )
            => Hash HbSync -> BlockDownloadM e m Bool

isBlockHereCached h = do
  szcache <- asks (view blockStored)
  sto <- lift getStorage

  cached <- liftIO $ Cache.lookup szcache h

  case cached of
    Just{} -> pure True
    Nothing -> liftIO do
     blk <- hasBlock sto h <&> isJust
     when blk $ Cache.insert szcache h ()
     pure blk

checkForDownload :: forall e m . ( MyPeer e
                                 , MonadIO m
                                 , HasPeerLocator e (BlockDownloadM e m)
                                 , HasStorage m -- (BlockDownloadM e m)
                                 )
                 => ByteString -> BlockDownloadM e m ()

checkForDownload lbs = do
  pure ()

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

  postponed <- isPostponed h

  unless postponed do

    maybe1 mbh none $ \hp -> claimBlockCameFrom @e brains hp h

    postpone <- shouldPostponeBlock @e brains h

    when postpone do
      -- trace $ "addDownload postpone" <+> pretty postpone <+> pretty h
      postponeBlock h

    doAdd <- do liftIO $ atomically $ stateTVar tinq
                                    \hm -> case HashMap.lookup h hm of
                                             Nothing -> (True,  HashMap.insert h () hm)
                                             Just{}  -> (False, HashMap.insert h () hm)

    notHere <- isBlockHereCached h <&> not

    when (doAdd && notHere && not postpone) do

      trace $ "addDownload" <+> pretty h

      q <- asks (view downloadQ)
      wip <- asks (view blockWip)

      liftIO do
        atomically $ do
          modifyTVar tinq $ HashMap.insert h ()
          writeTQueue q h

        Cache.insert wip h ()


postponedNum :: forall e  m . (MyPeer e, MonadIO m) => BlockDownloadM e m Int
postponedNum = do
  po <- asks (view blockPostponed)
  liftIO $ readTVarIO po <&> HashMap.size

isPostponed :: forall e  m . (MyPeer e, MonadIO m) => Hash HbSync -> BlockDownloadM e m Bool
isPostponed h = do
  po <- asks (view blockPostponed) >>= liftIO . readTVarIO
  pure $ HashMap.member  h po

postponeBlock :: forall e  m . (MyPeer e, MonadIO m) => Hash HbSync -> BlockDownloadM e m ()
postponeBlock h = do

  brains <- asks (view downloadBrains)
  po <- asks (view blockPostponed)
  tto <- asks (view blockPostponedTo)

  liftIO $ do
    already <- atomically $ readTVar po <&> HashMap.member h
    unless already do
      atomically $ modifyTVar po (HashMap.insert h ())
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
  wip <- asks (view blockWip)
  st  <- asks (view blockState)
  sz  <- asks (view blockPeers)
  tinq <- asks (view blockInQ)

  liftIO $ Cache.delete wip h

  liftIO $ atomically $ do
    modifyTVar' st   (HashMap.delete h)
    modifyTVar' sz   (HashMap.delete h)
    modifyTVar' tinq (HashMap.delete h)

hasPeerThread :: (MyPeer e, MonadIO m) => Peer e -> BlockDownloadM e m Bool
hasPeerThread p = do
  threads <- asks (view peerThreads)
  liftIO $ readTVarIO threads <&> HashMap.member p


delPeerThread :: (MyPeer e, MonadIO m) => Peer e -> BlockDownloadM e m ()
delPeerThread p = do
  debug $ "delPeerThread"  <+> pretty p
  threads <- asks (view peerThreads)
  pt <- liftIO $ atomically $ stateTVar threads (\x -> let t = HashMap.lookup p x
                                                       in  (t, HashMap.delete p x))

  maybe1 pt (pure ()) $ liftIO . cancel . view peerThreadAsync

newPeerThread :: ( MyPeer e
                 , MonadIO m
                 , Sessions e (PeerInfo e) m
                 -- , Sessions e (PeerInfo e) (BlockDownloadM e m)
                 )
              => Peer e
              -> Async ()
              -> BlockDownloadM e m ()

newPeerThread p m = do

  npi <- newPeerInfo
  void $ lift $ fetch True npi (PeerInfoKey p) id

  q <- liftIO  newTQueueIO
  let pt = PeerThread m q
  threads <- asks (view peerThreads)
  liftIO $ atomically $ modifyTVar threads $ HashMap.insert p pt


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


updateBlockPeerSize :: forall e m . (MyPeer e, MonadIO m)
                    => Hash HbSync
                    -> Peer e
                    -> Integer
                    -> BlockDownloadM e m ()

updateBlockPeerSize h p s = do
  tv <- asks (view blockPeers)

  setBlockHasSize h

  let alt = \case
        Nothing -> Just $ HashMap.singleton p s
        Just hm -> Just $ HashMap.insert p s hm

  liftIO $ atomically $ modifyTVar tv (HashMap.alter alt h)


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


