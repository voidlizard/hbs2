{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language TemplateHaskell #-}
{-# Language UndecidableInstances #-}
module PeerTypes where

import HBS2.Clock
import HBS2.Defaults
import HBS2.Hash
import HBS2.Net.Proto
import HBS2.Net.Proto.BlockInfo
import HBS2.Net.Proto.Definition
import HBS2.Net.Proto.Sessions
import HBS2.Prelude.Plated
import HBS2.Storage
import HBS2.System.Logger.Simple
import HBS2.Net.Messaging.UDP (UDP)

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
import Numeric (showGFloat)
import Prettyprinter

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

data BsFSM = Initial
           | Downloading
           | Postpone

data BlockState =
  BlockState
  { _bsStart      :: TimeSpec
  , _bsTimes      :: Int
  , _bsState      :: BsFSM
  , _bsWipTo      :: Double
  }

makeLenses 'BlockState


data PeerTask e = DoDownload

data PeerThread e =
  PeerThread
  { _peerThreadAsync   :: Async ()
  , _peerThreadMailbox :: TQueue (PeerTask e)
  }

data DownloadEnv e =
  DownloadEnv
  { _downloadQ      :: TQueue (Hash HbSync)
  , _peerBusy       :: TVar   (HashMap (Peer e) ())
  , _blockPeers     :: TVar   (HashMap (Hash HbSync) (HashMap (Peer e) Integer) )
  , _blockWip       :: Cache  (Hash HbSync) ()
  , _blockState     :: TVar   (HashMap (Hash HbSync) BlockState)
  , _blockPostponed :: Cache  (Hash HbSync) ()
  , _blockInQ       :: TVar   (HashMap (Hash HbSync) ())
  , _peerThreads    :: TVar   (HashMap (Peer e) (PeerThread e))
  }

makeLenses 'DownloadEnv

class (Eq (Peer e), Hashable (Peer e), Pretty (Peer e)) => MyPeer e
instance (Eq (Peer e), Hashable (Peer e), Pretty (Peer e)) => MyPeer e

newDownloadEnv :: (MonadIO m, MyPeer e) => m (DownloadEnv e)
newDownloadEnv = liftIO do
  DownloadEnv <$> newTQueueIO
              <*> newTVarIO mempty
              <*> newTVarIO mempty
              <*> Cache.newCache (Just defBlockWipTimeout)
              <*> newTVarIO mempty
              <*> Cache.newCache Nothing
              <*> newTVarIO mempty
              <*> newTVarIO mempty

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

withDownload :: (MyPeer e, MonadIO m) => DownloadEnv e -> BlockDownloadM e m a -> m a
withDownload e m = runReaderT ( fromBlockDownloadM m ) e

setBlockState :: MonadIO m => Hash HbSync -> BlockState -> BlockDownloadM e m ()
setBlockState h s = do
  sh <- asks (view blockState)
  liftIO $ atomically $ modifyTVar' sh (HashMap.insert h s)

-- FIXME: что-то более обоснованное
calcWaitTime :: MonadIO m => BlockDownloadM e m Double
calcWaitTime = do
  wip <- asks (view blockWip) >>= liftIO . Cache.size
  let wipn = realToFrac wip * 3
  let waiting = 5 + ( (realToFrac (toNanoSeconds defBlockWaitMax) * wipn) / 1e9 )
  pure waiting


touchBlockState :: MonadIO m => Hash HbSync -> BsFSM -> BlockDownloadM e m BlockState
touchBlockState h st = do
  sh <- asks (view blockState)
  t <- liftIO $ getTime MonotonicCoarse
  wo <- calcWaitTime

  let s = BlockState t 0 st wo

  sn <- liftIO $ atomically $ do
          modifyTVar sh (HashMap.alter (doAlter s) h)
          readTVar sh <&> fromMaybe s . HashMap.lookup h

  case view bsState sn of
    Initial -> do

      let t0 = view bsStart sn
      let dt = realToFrac (toNanoSecs t - toNanoSecs t0) / 1e9

      wip <- asks (view blockWip) >>= liftIO . Cache.size

      let waiting = view bsWipTo sn

      if dt > waiting  then do -- FIXME: remove-hardcode
        debug $ "pospone block" <+> pretty h <+> pretty dt <+> pretty (showGFloat (Just 2) waiting "")
        let sn1 = sn { _bsState = Postpone }
        liftIO $ atomically $ modifyTVar sh (HashMap.insert h sn1)
        pure sn1
      else do
        pure sn

    _ -> pure sn

  where
    doAlter s1 = \case
      Nothing -> Just s1
      Just s  -> Just $ over bsTimes succ s

getBlockState :: MonadIO m => Hash HbSync -> BlockDownloadM e m BlockState
getBlockState h = do
  sh <- asks (view blockState)
  touchBlockState h Initial

addDownload :: MonadIO m => Hash HbSync -> BlockDownloadM e m ()
addDownload h = do

  tinq <- asks (view blockInQ)

  doAdd <- do liftIO $ atomically $ stateTVar tinq
                                  \hm -> case HashMap.lookup h hm of
                                           Nothing -> (True,  HashMap.insert h () hm)
                                           Just{}  -> (False, HashMap.insert h () hm)
  when doAdd $ do

    q <- asks (view downloadQ)
    wip <- asks (view blockWip)

    liftIO do
      atomically $ writeTQueue q h
      Cache.insert wip h ()

    void $ touchBlockState h Initial

removeFromWip :: MonadIO m => Hash HbSync -> BlockDownloadM e m ()
removeFromWip h = do
  wip <- asks (view blockWip)
  st  <- asks (view blockState)
  po  <- asks (view blockPostponed)
  liftIO $ Cache.delete wip h
  liftIO $ Cache.delete po h
  liftIO $ atomically $ modifyTVar' st (HashMap.delete h)

hasPeerThread :: (MyPeer e, MonadIO m) => Peer e -> BlockDownloadM e m Bool
hasPeerThread p = do
  threads <- asks (view peerThreads)
  liftIO $ readTVarIO threads <&> HashMap.member p

newPeerThread :: (MyPeer e, MonadIO m) => Peer e -> Async () -> BlockDownloadM e m ()
newPeerThread p m = do
  q <- liftIO  newTQueueIO
  let pt = PeerThread m q
  threads <- asks (view peerThreads)
  liftIO $ atomically $ modifyTVar threads $ HashMap.insert p pt

