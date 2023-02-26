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
import HBS2.Net.Proto.BlockInfo
import HBS2.Net.Proto.Definition
import HBS2.Net.Proto.Sessions
import HBS2.Prelude.Plated
import HBS2.Storage
import HBS2.Net.PeerLocator
import HBS2.System.Logger.Simple

import PeerInfo

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
  , _peerPostponed  :: TVar   (HashMap (Hash HbSync) ())
  , _blockStored    :: Cache  (Hash HbSync) ()
  , _blockBanned    :: Cache  (Hash HbSync, Peer e) ()
  , _blocksWipCnt   :: TVar Int
  }

makeLenses 'DownloadEnv


newDownloadEnv :: (MonadIO m, MyPeer e) => m (DownloadEnv e)
newDownloadEnv = liftIO do
  DownloadEnv <$> newTQueueIO
              <*> newTVarIO mempty
              <*> newTVarIO mempty
              <*> Cache.newCache (Just defBlockWipTimeout)
              <*> newTVarIO mempty
              <*> newTVarIO mempty
              <*> newTVarIO mempty
              <*> newTVarIO mempty
              <*> Cache.newCache (Just defBlockWipTimeout)
              <*> Cache.newCache (Just defBlockBanTime)
              <*> newTVarIO 0

newtype BlockDownloadM e m a =
  BlockDownloadM { fromBlockDownloadM :: ReaderT (DownloadEnv e) m a }
  deriving newtype ( Functor
                   , Applicative
                   , Monad
                   , MonadIO
                   , MonadReader (DownloadEnv e)
                   , MonadTrans
                   )

runDownloadM :: (MyPeer e, MonadIO m) => BlockDownloadM e m a -> m a
runDownloadM m = runReaderT ( fromBlockDownloadM m ) =<< newDownloadEnv

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
    now     <- getTime MonotonicCoarse
    tvlast  <- newTVarIO now
    tvreq   <- newTVarIO 0
    tvsz    <- newTVarIO False
    let defState = BlockState now tvreq tvlast tvsz
    atomically $ stateTVar sh $ \hm -> case HashMap.lookup h hm of
                                         Nothing -> (defState, HashMap.insert h defState hm)
                                         Just x  -> (x, hm)

banBlock ::  (MyPeer e, MonadIO m) => Peer e -> Hash HbSync -> BlockDownloadM e m ()
banBlock p h = do
  banned <- asks (view blockBanned)
  liftIO $ Cache.insert banned (h,p) ()

isBanned :: (MyPeer e, MonadIO m) => Peer e -> Hash HbSync -> BlockDownloadM e m Bool
isBanned p h = do
  banned <- asks (view blockBanned)
  liftIO $ Cache.lookup banned (h,p) <&> isJust

delBlockState :: MonadIO m => Hash HbSync -> BlockDownloadM e m ()
delBlockState h = do
  sh <- asks (view blockState)
  liftIO $ atomically $ modifyTVar sh (HashMap.delete h)

incBlockSizeReqCount :: MonadIO m => Hash HbSync -> BlockDownloadM e m ()
incBlockSizeReqCount h = do
  blk  <- fetchBlockState h
  now  <- liftIO $ getTime MonotonicCoarse
  seen <- liftIO $ readTVarIO (view bsLastSeen blk)
  let elapsed = realToFrac (toNanoSecs (now - seen))  / 1e9
  noSize <- liftIO $ readTVarIO (view bsHasSize blk) <&> not

  when (elapsed > 1.0 && noSize) do
    liftIO $ atomically $ do
      writeTVar (view bsLastSeen blk) now
      modifyTVar (view bsReqSizeTimes blk) succ

-- FIXME: что-то более обоснованно
calcWaitTime :: MonadIO m => BlockDownloadM e m Double
calcWaitTime = do
  wip <- asks (view blockWip) >>= liftIO . Cache.size
  let wipn = realToFrac wip * 3
  let waiting = 5 + ( (realToFrac (toNanoSeconds defBlockWaitMax) * wipn) / 1e9 )
  pure waiting

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

addDownload :: forall e m . ( MyPeer e
                            , MonadIO m
                            , HasPeerLocator e (BlockDownloadM e m)
                            , HasStorage m -- (BlockDownloadM e m)
                            , Block ByteString ~ ByteString
                            )
            => Hash HbSync -> BlockDownloadM e m ()

addDownload h = do

  po <- asks (view peerPostponed)

  tinq <- asks (view blockInQ)
  wipCnt <- asks (view blocksWipCnt)

  -- doAdd <- do liftIO $ atomically $ stateTVar tinq
  --                                 \hm -> case HashMap.lookup h hm of
  --                                          Nothing -> (True,  HashMap.insert h () hm)
  --                                          Just{}  -> (False, HashMap.insert h () hm)

  doAdd <- isBlockHereCached h <&> not

  notPostponed <- liftIO $ readTVarIO po <&> isNothing . HashMap.lookup h

  when (doAdd && notPostponed) do

    q <- asks (view downloadQ)
    wip <- asks (view blockWip)

    liftIO do
      atomically $ do
        modifyTVar tinq $ HashMap.insert h ()
        writeTQueue q h

      Cache.insert wip h ()

     -- | False -> do -- not hasSize -> do

     --  po <- asks (view peerPostponed)
     --  liftIO $ atomically $ do
     --    modifyTVar po $ HashMap.insert h ()

     --  trace $ "postpone block" <+> pretty h <+> pretty brt
     --                           <+> "here:" <+> pretty (not missed)

     --  | otherwise -> do
     --    -- TODO: counter-on-this-situation
     --    none

returnPostponed :: forall e m . ( MyPeer e
                               , MonadIO m
                               , HasStorage m
                               , HasPeerLocator e (BlockDownloadM e m)
                               )
            => Hash HbSync -> BlockDownloadM e m ()

returnPostponed h = do
  tinq <- asks (view blockInQ)
  -- TODO: atomic-operations
  delFromPostponed h
  delBlockState h
  liftIO $ atomically $ modifyTVar' tinq (HashMap.delete h)
  addDownload h

delFromPostponed :: MonadIO m => Hash HbSync -> BlockDownloadM e m ()
delFromPostponed h = do
  po  <- asks (view peerPostponed)
  liftIO $ atomically $ do
    modifyTVar' po (HashMap.delete h)

removeFromWip :: (MyPeer e, MonadIO m) => Hash HbSync -> BlockDownloadM e m ()
removeFromWip h = do
  wip <- asks (view blockWip)
  st  <- asks (view blockState)
  sz  <- asks (view blockPeers)
  tinq <- asks (view blockInQ)
  po  <- asks (view peerPostponed)
  wi <- asks (view blocksWipCnt)
  ba <- asks (view blockBanned)

  liftIO $ Cache.delete wip h
  liftIO $ Cache.filterWithKey (\(hx,_) _ -> hx /= h) ba

  liftIO $ atomically $ do
    modifyTVar' st   (HashMap.delete h)
    modifyTVar' sz   (HashMap.delete h)
    modifyTVar' tinq (HashMap.delete h)
    modifyTVar' po (HashMap.delete h)
    modifyTVar' wi (max 0 . pred)

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

newPeerThread :: (MyPeer e, MonadIO m) => Peer e -> Async () -> BlockDownloadM e m ()
newPeerThread p m = do
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
  addDownload h

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

