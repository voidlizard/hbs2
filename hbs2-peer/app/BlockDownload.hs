{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language TemplateHaskell #-}
{-# Language UndecidableInstances #-}
module BlockDownload where

import HBS2.Actors.Peer
import HBS2.Clock
import HBS2.Data.Detect
import HBS2.Data.Types.Refs
import HBS2.Defaults
import HBS2.Events
import HBS2.Hash
import HBS2.Merkle
import HBS2.Net.PeerLocator
import HBS2.Net.Proto
import HBS2.Net.Proto.Definition
import HBS2.Net.Proto.Sessions
import HBS2.Prelude.Plated
import HBS2.Storage
import HBS2.System.Logger.Simple

import PeerInfo

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.ByteString.Lazy (ByteString)
import Data.Cache (Cache)
import Data.Cache qualified as Cache
import Data.Foldable hiding (find)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Maybe
import Data.Set qualified as Set
import Data.Set (Set)
import Lens.Micro.Platform
import Prettyprinter
import System.Random.Shuffle


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


-- data MyBlkInfo e =
--   MyBlkInfo (Peer e) Integer
  -- deriving stock (Eq,Ord)

data DownloadEnv e =
  DownloadEnv
  { _downloadQ   :: TQueue (Hash HbSync)
  , _peerBusy    :: TVar   (HashMap (Peer e) ())
  , _blockPeers  :: TVar   (HashMap (Hash HbSync) (HashMap (Peer e) Integer) )
  , _blockWip    :: Cache  (Hash HbSync) ()
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

addDownload :: MonadIO m => Hash HbSync -> BlockDownloadM e m ()
addDownload h = do
  q <- asks (view downloadQ)
  wip <- asks (view blockWip)

  liftIO do
    atomically $ writeTQueue q h
    Cache.insert wip h ()

removeFromWip :: MonadIO m => Hash HbSync -> BlockDownloadM e m ()
removeFromWip h = do
  wip <- asks (view blockWip)
  liftIO $ Cache.delete wip h

withFreePeer :: (MyPeer e, MonadIO m)
             => Peer e
             -> BlockDownloadM e m ()
             -> BlockDownloadM e m ()
             -> BlockDownloadM e m ()

withFreePeer p n m = do
  busy <- asks (view peerBusy)
  avail <- liftIO $ atomically
                  $ stateTVar busy $
                      \s -> case HashMap.lookup p s of
                              Nothing -> (True, HashMap.insert p () s)
                              Just{}  -> (False, s)
  if not avail
    then n
    else do
      r <- m
      liftIO $ atomically $ modifyTVar busy $ HashMap.delete p
      pure r

-- NOTE: dangerous! if called in
--       wrong place/wrong time,
--       if may cause a drastical
--       download speed degradation

dismissPeer  :: (MyPeer e, MonadIO m)
             => Peer e
             -> BlockDownloadM e m ()
dismissPeer p = do
  busy <- asks (view peerBusy)
  liftIO $ atomically $ modifyTVar busy $ HashMap.delete p

getBlockForDownload :: MonadIO m => BlockDownloadM e m (Hash HbSync)
getBlockForDownload = do
  q <- asks (view downloadQ)
  liftIO $ atomically $ readTQueue q

addBlockInfo :: (MonadIO m, MyPeer e)
             => Peer e
             -> Hash HbSync
             -> Integer
             -> BlockDownloadM e m ()

addBlockInfo pip h size = do
  -- debug $ "addBlockInfo" <+> pretty h <+> pretty pip <+> pretty size
  tv <- asks (view blockPeers)
  let mySize = HashMap.singleton pip size
  liftIO $ atomically
         $ modifyTVar tv (HashMap.insertWith (<>) h mySize)

getPeersForBlock :: (MonadIO m, MyPeer e)
                => Hash HbSync
                -> BlockDownloadM e m [(Peer e, Integer)]

getPeersForBlock h = do
  tv <- asks (view blockPeers)
  liftIO $ readTVarIO tv <&> foldMap HashMap.toList
                                            . maybeToList
                                            . HashMap.lookup h

processBlock :: forall e m . ( MonadIO m
                             , HasStorage m
                             , Block ByteString ~ ByteString
                             )
             => Hash HbSync
             -> BlockDownloadM e m ()

processBlock h = do

   sto <- lift getStorage

   bt <- liftIO $ getBlock sto h <&> fmap (tryDetect h)

   -- FIXME:  если блок нашёлся, то удаляем его из wip

   when (isJust bt) (removeFromWip h)

   case bt of
     Nothing -> addDownload h

     Just (AnnRef{}) -> pure ()

     Just (Merkle{}) -> do
       debug $ "GOT MERKLE. requesting nodes/leaves" <+> pretty h
       walkMerkle h (liftIO . getBlock sto)  $ \(hr :: [HashRef]) -> do

         for_ hr $ \(HashRef blk) -> do

           -- debug $ pretty blk

           here <- liftIO (hasBlock sto blk) <&> isJust

           if here then do
             debug $ "block" <+> pretty blk <+> "is already here"
             processBlock blk -- NOTE: хуже не стало
                              -- FIXME:  fugure out if it's really required

             pure () -- we don't need to recurse, cause walkMerkle is recursing for us

           else do
              addDownload blk


     Just (Blob{}) -> do
       pure ()

-- NOTE: if peer does not have a block, it may
--       cause to an unpleasant timeouts
--       So make sure that this peer really answered to
--       GetBlockSize request

downloadFromWithPeer :: forall e m . ( MyPeer e
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
                     => Peer e
                     -> Integer
                     -> Hash HbSync
                     -> BlockDownloadM e m ()
downloadFromWithPeer peer thisBkSize h = do

  npi <- newPeerInfo
  pinfo <- lift $ fetch True npi (PeerInfoKey peer) id

  sto <- lift getStorage

  coo <- genCookie (peer,h)
  let key = DownloadSessionKey (peer, coo)
  let chusz = defChunkSize
  dnwld <- newBlockDownload h
  let chuQ = view sBlockChunks dnwld
  let new =   set sBlockChunkSize chusz
            . set sBlockSize (fromIntegral thisBkSize)
              $ dnwld

  lift $ update @e new key id

  let burstSizeT = view peerBurst pinfo

  burstSize <- liftIO $ readTVarIO burstSizeT

  let offsets = calcChunks thisBkSize (fromIntegral chusz)  :: [(Offset, Size)]

  let chunkNums = [ 0 .. pred (length offsets) ]

  let bursts = calcBursts burstSize chunkNums

  -- debug $ "bursts: " <+> pretty bursts

  r  <- liftIO $ newTVarIO (mempty :: IntMap ByteString)
  rq <- liftIO newTQueueIO

  for_ bursts $ liftIO . atomically . writeTQueue rq

  fix \next -> do
    burst <- liftIO $ atomically $ tryReadTQueue rq

    case burst of

      Just (i,chunksN) -> do
        let req = BlockGetChunks h chusz (fromIntegral i) (fromIntegral chunksN)
        lift $ request peer (BlockChunks @e coo req)

        -- TODO: here wait for all requested chunks!
        -- FIXME: it may blocks forever, so must be timeout and retry

        catched <- either id id <$> liftIO ( race ( pause defChunkWaitMax >> pure mempty )
                                                  ( replicateM chunksN
                                                       $ atomically
                                                          $ readTQueue chuQ )

                                           )
        if not (null catched) then do
          liftIO $ atomically do
            modifyTVar (view peerDownloaded pinfo) (+chunksN)
            writeTVar  (view peerPingFailed pinfo) 0

        else do

            liftIO $ atomically $ modifyTVar (view peerErrors pinfo) succ
            updatePeerInfo pinfo

            newBurst <- liftIO $ readTVarIO burstSizeT

            liftIO $ atomically $ modifyTVar (view peerDownloaded pinfo) (+chunksN)

            let chuchu = calcBursts newBurst [ i + n | n <- [0 .. chunksN] ]

            liftIO $ atomically $ modifyTVar (view peerErrors pinfo) succ

            debug $ "new burst: " <+> pretty newBurst
            debug $ "missed chunks for request" <+> pretty (i,chunksN)

            for_ chuchu $ liftIO . atomically . writeTQueue rq

        for_ catched $ \(num,bs) -> do
          liftIO $ atomically $ modifyTVar' r (IntMap.insert (fromIntegral num) bs)

        next

      Nothing -> do

        sz <- liftIO $ readTVarIO r <&> IntMap.size

        if sz == length offsets then do
          pieces <- liftIO $ readTVarIO r <&> IntMap.elems
          let block = mconcat pieces
          let h1 = hashObject @HbSync block

          if h1 == h then do
            -- debug "PROCESS BLOCK"
            lift $ expire @e key
            void $ liftIO $ putBlock sto block
            void $ processBlock h
          else do
            debug "HASH NOT MATCH"
            debug "MAYBE THAT PEER IS JERK"

        else do
          debug "RETRY BLOCK DOWNLOADING / ASK FOR MISSED CHUNKS"
          got  <- liftIO $ readTVarIO r <&> IntMap.keysSet
          let need = IntSet.fromList (fmap fromIntegral chunkNums)

          let missed = IntSet.toList $ need `IntSet.difference` got

          -- normally this should not happen
          -- however, let's try do download the tails
          -- by one chunk a time
          for_ missed $ \n -> do
            liftIO $ atomically $ writeTQueue rq (n,1)


instance HasPeerLocator e m => HasPeerLocator e (BlockDownloadM e m) where
  getPeerLocator = lift getPeerLocator


updatePeerInfo :: MonadIO m => PeerInfo e -> m ()
updatePeerInfo pinfo = do

  t1 <- liftIO $ getTime MonotonicCoarse

  void $ liftIO $ atomically $ do

          bu        <- readTVar (view peerBurst pinfo)
          errs      <- readTVar (view peerErrors pinfo)
          errsLast  <- readTVar (view peerErrorsLast pinfo)
          t0        <- readTVar (view peerLastWatched pinfo)
          down      <- readTVar (view peerDownloaded pinfo)
          downLast  <- readTVar (view peerDownloadedLast pinfo)

          let dE  =  realToFrac $ max 0 (errs - errsLast)
          let dT  =  realToFrac (max 1 (toNanoSecs t1 - toNanoSecs t0)) / 1e9

          let eps = floor (dE / dT)

          let bu1 = if down - downLast > 0 then
                      max 1 $ min defBurstMax
                              $ if eps == 0 then
                                   ceiling $ realToFrac bu * 1.05 -- FIXME: to defaults
                                 else
                                   floor $ realToFrac bu * 0.65
                    else
                      max defBurst $ floor (realToFrac bu * 0.65)

          writeTVar (view peerErrorsLast pinfo) errs
          writeTVar (view peerLastWatched pinfo) t1
          writeTVar (view peerErrorsPerSec pinfo) eps
          writeTVar (view peerBurst pinfo) bu1
          writeTVar (view peerDownloadedLast pinfo) down


blockDownloadLoop :: forall e  m . ( m ~ PeerM e IO
                                   ,  MonadIO m
                                   , Request e (BlockInfo e) m
                                   , Request e (BlockAnnounce e) m
                                   , HasProtocol e (BlockInfo e)
                                   , HasProtocol e (BlockAnnounce e)
                                   , HasProtocol e (BlockChunks e)
                                   , EventListener e (BlockInfo e) m
                                   , EventListener e (BlockChunks e) m
                                   , EventListener e (BlockAnnounce e) m
                                   , EventEmitter e (BlockChunks e) m
                                   , Sessions e (BlockChunks e) m
                                   , Sessions e (PeerInfo e) m
                                   , PeerSessionKey e (PeerInfo e)
                                   -- , Typeable (SessionKey e (BlockChunks e))
                                   -- , Typeable (SessionKey e (BlockInfo e))
                                   , HasStorage m
                                   , Pretty (Peer e)
                                   , Block ByteString ~ ByteString
                                   , PeerMessaging e
                                   )
                  => DownloadEnv e -> m ()
blockDownloadLoop env0 = do

  e    <- ask
  stor <- getStorage

  let blks = mempty

  pl <- getPeerLocator @e


  void $ liftIO $ async $ forever $ withPeerM e do
    pause @'Seconds 0.5

    pee <- knownPeers @e pl
    npi <- newPeerInfo


    for_ pee $ \p -> do
      pinfo <- fetch True npi (PeerInfoKey p) id
      updatePeerInfo pinfo

  -- TODO: peer info loop
  void $ liftIO $ async $ forever $ withPeerM e $ do
    pause @'Seconds 20
    pee <- knownPeers @e pl

    npi <- newPeerInfo

    debug $ "known peers" <+> pretty pee

    for_ pee $ \p -> do
      pinfo <- fetch True npi (PeerInfoKey p) id
      burst  <- liftIO $ readTVarIO (view peerBurst pinfo)
      errors <- liftIO $ readTVarIO (view peerErrorsPerSec pinfo)
      debug $ "peer" <+> pretty p <+> "burst: " <+> pretty burst
                                  <+> "errors:" <+> pretty errors
      pure ()

  void $ liftIO $ async $ forever $ withPeerM e $ withDownload env0 do
    pause @'Seconds 5 -- FIXME: put to defaults
                      --        we need to show download stats

    tinfo <- asks (view blockPeers)
    binfo <- liftIO $ readTVarIO tinfo
    wip  <- asks (view blockWip)

    liftIO $ Cache.purgeExpired wip

    aliveWip <- Set.fromList <$> liftIO (Cache.keys wip)

    let alive = HashMap.fromList [ (h,i)
                                   | (h,i) <- HashMap.toList binfo
                                   , Set.member h aliveWip
                                 ]

    liftIO $ atomically $ writeTVar tinfo alive

    debug $ "maintain blocks wip" <+> pretty (Set.size aliveWip)

  withDownload env0 do

    env <- ask

    let again h = do
          -- debug $ "retrying block: " <+> pretty h
          withPeerM e $ withDownload env (addDownload h)

    mapM_ processBlock blks

    fix \next -> do

      h <- getBlockForDownload

      here <- liftIO $ hasBlock stor h <&> isJust

      unless here do

        peers <- getPeersForBlock h

        when (null peers) $ do

          lift do -- in PeerM
            subscribe (BlockSizeEventKey h) $ \(BlockSizeEvent (p1,hx,s)) -> do
              withDownload env (addBlockInfo p1 hx s)

            pips <- knownPeers @e pl
            for_ pips $ \pip -> request pip (GetBlockSize @e h)

        p  <- knownPeers @e pl >>= liftIO . shuffleM

        -- debug $ "known peers" <+> pretty p
        -- debug $ "peers/blocks" <+> pretty peers

        p0 <- headMay <$> liftIO (shuffleM peers) -- FIXME: random choice to work faster

        let withAllShit f = withPeerM e $ withDownload env f

        maybe1 p0 (again h) $ \(p1,size) -> do
            withFreePeer p1 (again h) $
              liftIO do
                  re <- race ( pause defBlockWaitMax ) $
                          withAllShit $ downloadFromWithPeer p1 size h

                  case re of
                    Left{}  -> withAllShit (again h)
                    Right{} -> withAllShit (processBlock h)

      next


-- NOTE: this is an adapter for a ResponseM monad
--       because response is working in ResponseM monad (ha!)
--       So don't be confused with types
--
mkAdapter :: forall e m . ( m ~  PeerM e IO
                          , HasProtocol e (BlockChunks e)
                          , Hashable (SessionKey e (BlockChunks e))
                          , Sessions e (BlockChunks e) (ResponseM e m)
                          , Typeable (SessionKey e (BlockChunks e))
                          , EventEmitter e (BlockChunks e) m
                          , Pretty (Peer e)
                          , Block ByteString ~ ByteString
                          )
          => m (BlockChunksI e (ResponseM e m ))
mkAdapter = do
  storage <- getStorage
  pure $
    BlockChunksI
    { blkSize     = liftIO . hasBlock storage
    , blkChunk    = \h o s -> liftIO (getChunk storage h o s)
    , blkGetHash  = \c -> find (DownloadSessionKey @e c) (view sBlockHash)

    , blkAcceptChunk = \(c,p,h,n,bs) -> void $ runMaybeT $ do
        let cKey = DownloadSessionKey (p,c)
        dwnld <- MaybeT $ find cKey (view sBlockChunks)
        liftIO $ atomically $ writeTQueue dwnld (n, bs)
    }


