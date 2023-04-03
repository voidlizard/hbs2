{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language UndecidableInstances #-}
{-# Language MultiWayIf #-}
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
import HBS2.Net.Proto.Peer
import HBS2.Net.Proto.RefLog
import HBS2.Net.Proto.Sessions
import HBS2.Prelude.Plated
import HBS2.Storage
import HBS2.System.Logger.Simple

import PeerTypes
import PeerInfo
import Brains

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.ByteString.Lazy (ByteString)
import Data.Cache qualified as Cache
import Data.Foldable hiding (find)
import Data.HashMap.Strict qualified as HashMap
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List qualified as List
import Data.Maybe
import Lens.Micro.Platform
import System.Random (randomRIO)
import System.Random.Shuffle (shuffleM)
import Numeric (showGFloat)

getBlockForDownload :: forall e m . (MonadIO m, IsPeerAddr e m, MyPeer e)
                     => Peer e
                     -> BlockDownloadM e m (Maybe (Hash HbSync))

getBlockForDownload peer = do
  pa <- lift $ toPeerAddr peer
  tinq <- asks (view blockInQ)
  brains <- asks (view downloadBrains)
  prop <- asks (view blockProposed)

  inq <- liftIO $ readTVarIO tinq
  let size = HashMap.size inq

  if size == 0 then
    pure Nothing
  else do
    i <- randomRIO (0, size - 1)
    let blk = HashMap.keys inq !! i
    peers <- advisePeersForBlock @e brains blk

    proposed <- liftIO $ Cache.lookup prop (blk, peer) <&> isJust

    r <- if | proposed -> do
              pure Nothing

            | List.null peers -> do
                pure $ Just blk

            | pa `elem` peers -> do
               pure $ Just blk

            | otherwise -> do
               newOne <- shouldDownloadBlock @e brains peer blk
               let chance = if newOne then 1 else 5
               lucky <- liftIO $ shuffleM (True : replicate chance False) <&> headDef False
               if lucky then
                 pure $ Just blk
               else do
                 pure Nothing

    case r of
      Nothing -> none
      Just h -> do
        liftIO $ Cache.insert prop (h, peer) ()

    pure r

processBlock :: forall e m . ( MonadIO m
                             , HasStorage m
                             , MyPeer e
                             , HasPeerLocator e (BlockDownloadM e m)
                             , Block ByteString ~ ByteString
                             )
             => Hash HbSync
             -> BlockDownloadM e m ()

processBlock h = do

   sto <- lift getStorage

   brains <- asks (view downloadBrains)

   let parent = Just h

   bt <- liftIO $ getBlock sto h <&> fmap (tryDetect h)

   -- FIXME:  если блок нашёлся, то удаляем его из wip

   when (isJust bt) (removeFromWip h)

   let handleHrr = \(hrr :: Either (Hash HbSync) [HashRef]) -> do

            case hrr of
              Left hx -> addDownload parent hx
              Right hr -> do

                for_ hr $ \(HashRef blk) -> do

                  -- debug $ pretty blk

                  here <- liftIO (hasBlock sto blk) <&> isJust

                  if here then do
                    pure ()
                    -- debug $ "block" <+> pretty blk <+> "is already here"
                    -- unless (h == blk) do
                    --   processBlock blk -- NOTE: хуже не стало
                                      -- FIXME:  fugure out if it's really required

                  else do
                    addDownload parent blk

   case bt of
     Nothing -> addDownload mzero h

     Just (SeqRef (SequentialRef n (AnnotatedHashRef a' b))) -> do
      maybe1 a' none $ \a -> do
        addDownload parent (fromHashRef a)

      addDownload parent (fromHashRef b)

     Just (AnnRef (AnnotatedHashRef ann hx)) -> do
       maybe1 ann none $ addDownload parent . fromHashRef
       addDownload parent (fromHashRef hx)

     Just (MerkleAnn ann) -> do
       case _mtaMeta ann of
          NoMetaData -> pure ()
          ShortMetadata {} -> pure ()
          AnnHashRef hx -> addDownload parent hx

       case (_mtaCrypt ann) of
          NullEncryption -> pure ()
          CryptAccessKeyNaClAsymm h -> addDownload parent h

       debug $ "GOT WRAPPED MERKLE. requesting nodes/leaves" <+> pretty h
       walkMerkleTree (_mtaTree ann) (liftIO . getBlock sto) handleHrr

     Just (Merkle{}) -> do
       debug $ "GOT MERKLE. requesting nodes/leaves" <+> pretty h
       walkMerkle h (liftIO . getBlock sto) handleHrr

     Just (Blob{}) -> do
       pure ()

-- NOTE: if peer does not have a block, it may
--       cause to an unpleasant timeouts
--       So make sure that this peer really answered to
--       GetBlockSize request


downloadFromWithPeer :: forall e m . ( DownloadFromPeerStuff e m
                                     , HasPeerLocator e (BlockDownloadM e m) )
                     => Peer e
                     -> Integer
                     -> Hash HbSync
                     -> BlockDownloadM e m ()
downloadFromWithPeer peer thisBkSize h = do

  brains <- asks (view downloadBrains)

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

  let w = max defChunkWaitMax $ realToFrac (toNanoSeconds defBlockWaitMax)  / realToFrac  (length bursts) / 1e9 * 2

  let burstTime = realToFrac w :: Timeout 'Seconds -- defChunkWaitMax  -- min defBlockWaitMax (0.8 * realToFrac burstSize * defChunkWaitMax)

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

        catched <- either id id <$> liftIO ( race ( pause burstTime >> pure mempty )
                                                  ( replicateM chunksN
                                                       $ atomically
                                                          $ readTQueue chuQ )

                                           )
        if not (null catched) then do
          liftIO $ atomically do
            modifyTVar (view peerDownloaded pinfo) (+chunksN)
            writeTVar  (view peerPingFailed pinfo) 0

        else do

            -- liftIO $ atomically $ modifyTVar (view peerErrors pinfo) succ
            updatePeerInfo True pinfo

            newBurst <- liftIO $ readTVarIO burstSizeT
            -- let newBurst = max defBurst $ floor (realToFrac newBurst' * 0.5 )

            liftIO $ atomically $ modifyTVar (view peerDownloaded pinfo) (+chunksN)

            let chuchu = calcBursts newBurst [ i + n | n <- [0 .. chunksN] ]

            liftIO $ atomically $ modifyTVar (view peerErrors pinfo) succ

            trace $ "new burst: " <+> pretty newBurst
            trace $ "missed chunks for request" <+> pretty (i,chunksN)
            trace $ "burst time" <+> pretty burstTime

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
            onBlockDownloaded brains peer h
            void $ processBlock h
          else do
            trace "HASH NOT MATCH / PEER MAYBE JERK"

        else do
          trace "RETRY BLOCK DOWNLOADING / ASK FOR MISSED CHUNKS"
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


-- NOTE: updatePeerInfo is CC
--   updatePeerInfo is actuall doing CC (congestion control)

updatePeerInfo :: MonadIO m => Bool -> PeerInfo e -> m ()
updatePeerInfo onError pinfo = do

  t1 <- liftIO getTimeCoarse

  void $ liftIO $ atomically $ do

          bu        <- readTVar (view peerBurst pinfo)
          buMax     <- readTVar (view peerBurstMax pinfo)
          buSet     <- readTVar (view peerBurstSet pinfo)
          errs      <- readTVar (view peerErrors pinfo)
          errsLast  <- readTVar (view peerErrorsLast pinfo)
          t0        <- readTVar (view peerLastWatched pinfo)
          down      <- readTVar (view peerDownloaded pinfo)
          downLast  <- readTVar (view peerDownloadedLast pinfo)
          -- downFail  <- readTVar (view peerDownloadFail pinfo)
          -- downBlk   <- readTVar (view peerDownloadedBlk pinfo)

          let dE  =  realToFrac $ max 0 (errs - errsLast)
          let dT  =  realToFrac (max 1 (toNanoSecs t1 - toNanoSecs t0)) / 1e9

          let eps = floor (dE / dT)

          let win =  min 10 $ 4 * (defBurstMax - defBurst)

          when (down - downLast > 0 || onError) do

            (bu1, bus) <- if  eps == 0 && not onError then do
                            let bmm = fromMaybe defBurstMax buMax
                            let buN = min bmm (ceiling (realToFrac bu * 1.05))
                            pure (buN, trimUp win $ IntSet.insert buN buSet)
                          else do
                            let buM = headMay $ drop 2 $ IntSet.toDescList buSet
                            writeTVar (view peerBurstMax pinfo) buM
                            let buN = headDef defBurst $ drop 4 $ IntSet.toDescList buSet
                            pure (buN, trimDown win $ IntSet.insert buN buSet)


            writeTVar (view peerErrorsLast pinfo) errs
            writeTVar (view peerLastWatched pinfo) t1
            writeTVar (view peerErrorsPerSec pinfo) eps
            writeTVar (view peerBurst pinfo) bu1
            writeTVar (view peerBurstSet pinfo) bus
            writeTVar (view peerDownloadedLast pinfo) down
            -- writeTVar (view peerUsefulness pinfo) usefulN

    where
      trimUp n s | IntSet.size s >= n = IntSet.deleteMin s
                 | otherwise       = s

      trimDown n s | IntSet.size s >= n = IntSet.deleteMax s
                   | otherwise       = s

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
                                   , EventListener e (PeerHandshake e) m
                                   , EventListener e (RefLogUpdateEv e) m
                                   , EventListener e (RefLogRequestAnswer e) m
                                   , EventEmitter e (BlockChunks e) m
                                   , EventEmitter e (DownloadReq e) m
                                   , Sessions e (BlockChunks e) m
                                   , Sessions e (PeerInfo e) m
                                   , Sessions e (KnownPeer e) m
                                   , PeerSessionKey e (PeerInfo e)
                                   , HasStorage m
                                   , Pretty (Peer e)
                                   , Block ByteString ~ ByteString
                                   , PeerMessaging e
                                   , IsPeerAddr e m
                                   )
                  => DownloadEnv e -> m ()
blockDownloadLoop env0 = do

  e    <- ask

  let blks = mempty

  pl <- getPeerLocator @e

  pause @'Seconds 3.81

  void $ liftIO $ async $ forever $ withPeerM e $ withDownload env0 do
    pause @'Seconds 10
    debug "I'm peer thread sweeping thread"

    known <- knownPeers @e pl

    peers' <- forM known $ \p -> do
                auth <- lift $ find (KnownPeerKey p) id <&> isJust
                pinfo <- lift $ find (PeerInfoKey p) id <&> isJust
                if auth && pinfo then
                  pure [(p,())]
                else
                  pure mempty

    let auth = HashMap.fromList (mconcat peers')

    pts <- asks (view peerThreads)

    r <- liftIO $ atomically $ stateTVar pts $ \x ->
            let items = HashMap.toList x
            in let (alive,dead) = List.partition (\(k,_) -> HashMap.member k auth ) items
            in (dead, HashMap.fromList alive)

    debug $ "peers to delete" <+> pretty (length r)

    for_ r $ killPeerThread . fst

  void $ liftIO $ async $ forever $ withPeerM e do
    pause @'Seconds 1
    -- debug "I'm a peer maintaining thread"

    brains <- withDownload env0 $ asks (view downloadBrains)
    pee <- knownPeers @e pl

    onKnownPeers brains pee

    for_ pee $ \p -> do
      pinfo' <- find (PeerInfoKey p) id
      auth <- find (KnownPeerKey p) id <&> isJust
      maybe1 pinfo' none $ \pinfo -> do

        fails <- liftIO $ readTVarIO (view peerDownloadFail pinfo)

        when (fails >= defDownloadFails) do
          trace $ "peer" <+> pretty p <+> "has too many failures:" <+> pretty fails

        here <- withDownload env0 $ hasPeerThread p

        if | not here && auth -> do

              debug $ "peer" <+> pretty p <+> "does not have a thread"
              runPeer <-  liftIO $ async $ liftIO (withPeerM e $ withDownload env0 (peerDownloadLoop p))
              withDownload env0 $ newPeerThread p runPeer

           | not auth -> do
              pure ()

           | otherwise -> pure ()

  void $ liftIO $ async $ forever $ withPeerM e do
    pause @'Seconds 30

    pee <- knownPeers @e pl
    npi <- newPeerInfo

    for_ pee $ \p -> do
      pinfo <- fetch True npi (PeerInfoKey p) id
      liftIO $ atomically $ writeTVar (view peerBurstMax pinfo) Nothing


  void $ liftIO $ async $ forever $ withPeerM e do
    pause @'Seconds 1.5

    pee <- knownPeers @e pl
    npi <- newPeerInfo

    for_ pee $ \p -> do
      pinfo <- fetch True npi (PeerInfoKey p) id
      updatePeerInfo False pinfo

  -- TODO: peer info loop
  void $ liftIO $ async $ forever $ withPeerM e $ do
    pause @'Seconds 10
    pee <- knownPeers @e pl

    npi <- newPeerInfo

    debug $ "known peers" <+> pretty pee

    for_ pee $ \p -> do
      pinfo <- fetch True npi (PeerInfoKey p) id
      burst  <- liftIO $ readTVarIO (view peerBurst pinfo)
      buM    <- liftIO $ readTVarIO (view peerBurstMax pinfo)
      errors <- liftIO $ readTVarIO (view peerErrorsPerSec pinfo)
      downFails <- liftIO $ readTVarIO (view peerDownloadFail pinfo)
      down      <- liftIO $ readTVarIO (view peerDownloadedBlk pinfo)
      rtt       <- liftIO $ readTVarIO (view peerRTT pinfo) <&> fmap realToFrac

      let rttMs = (/1e6) <$> rtt <&> (\x -> showGFloat (Just 2) x "") <&> (<> "ms")

      notice $ "peer" <+> pretty p <+> "burst:" <+> pretty burst
                                   <+> "burst-max:" <+> pretty buM
                                   <+> "errors:" <+> pretty (downFails + errors)
                                   <+> "down:" <+> pretty down
                                   <+> "rtt:" <+> pretty rttMs
      pure ()

  void $ liftIO $ async $ forever $ withPeerM e $ withDownload env0 do
    pause @'Seconds 5 -- FIXME: put to defaults
                      --        we need to show download stats

    wipNum  <- asks (view blockInQ) >>= liftIO . readTVarIO <&> HashMap.size
    let po = 0

    notice $ "maintain blocks wip" <+> pretty wipNum
                                   <+> "postponed"
                                   <+> pretty po

  withDownload env0 do

    mapM_ processBlock blks

    proposed <- asks (view blockProposed)

    forever do
      pause @'Seconds 20
      debug "block download loop. does not do anything"
      liftIO $ Cache.purgeExpired proposed


postponedLoop :: forall e m . ( MyPeer e
                              , Sessions e (KnownPeer e) m
                              , Request e (BlockInfo e) m
                              , EventListener e (BlockInfo e) m
                              , DownloadFromPeerStuff e m
                              , HasPeerLocator e m
                              , m ~ PeerM e IO
                              )
              => DownloadEnv e -> m ()
postponedLoop env0 = do
  e <- ask

  pause @'Seconds 2.57

  void $ liftIO $ async $ withPeerM e $ withDownload env0 do
    q <- asks (view blockDelayTo)
    fix \next -> do
      w <- liftIO $ atomically $ readTQueue q
      pause defInterBlockDelay
      addDownload mzero w
      -- ws <- liftIO $ atomically $ flushTQueue q
      -- for_ (w:ws) $ addDownload mzero
      next

  void $ liftIO $ async $ withPeerM e $ withDownload env0 do
    forever do
      pause @'Seconds 20
      trace "UNPOSTPONE LOOP"
      po <- asks (view blockPostponedTo) >>= liftIO . Cache.toList
      for_ po $ \(h, _, expired) -> do
        when (isJust expired) do
          unpostponeBlock h

peerDownloadLoop :: forall e m . ( MyPeer e
                                 , Sessions e (KnownPeer e) m
                                 , Request e (BlockInfo e) m
                                 , EventListener e (BlockInfo e) m
                                 , DownloadFromPeerStuff e m
                                 , HasPeerLocator e m
                                 , IsPeerAddr e m
                                 , m ~ PeerM e IO
                                 ) => Peer e -> BlockDownloadM e m ()
peerDownloadLoop peer = do

  pe <- lift ask
  e <- ask

  brains <- asks (view downloadBrains)

  let doBlockSizeRequest h = do
        q <- liftIO newTQueueIO
        lift do
          subscribe @e (BlockSizeEventKey h) $ \case
            BlockSizeEvent (p1,_,s) -> do
              when (p1 == peer) do
                liftIO $ atomically $ writeTQueue q (Just s)
                onBlockSize brains peer h s

            NoBlockEvent{} -> do
              -- TODO: ban-block-for-some-seconds
              liftIO $ atomically $ writeTQueue q Nothing
              pure ()

          request peer (GetBlockSize @e h)

          liftIO $ race ( pause defBlockInfoTimeout )
                        ( atomically $ do
                           s <- readTQueue q
                           void $ flushTQueue q
                           pure s
                        )

  let tryDownload pinfo h size = do

        trace $ "tryDownload" <+> pretty peer <+> pretty h

        here <- isBlockHereCached h

        if here then do
          trace $ pretty peer <+> "block" <+> pretty h <+> "is already here"
          processBlock h
        else do
          lift $ onBlockDownloadAttempt brains peer h
          let downFail = view peerDownloadFail pinfo
          let downBlk  = view peerDownloadedBlk pinfo

          r <- liftIO $ race ( pause defBlockWaitMax )
                          $ withPeerM pe
                          $ withDownload e
                          $ downloadFromWithPeer peer size h
          case r of
            Left{} -> do
              trace $ "FAIL" <+> pretty peer <+> "download block" <+> pretty h
              liftIO $ atomically $ modifyTVar downFail succ
              failedDownload peer h

            Right{} -> do
              trace $ "OK" <+> pretty peer <+> "dowloaded block" <+> pretty h
              onBlockDownloaded brains peer h
              processBlock h
              liftIO $ atomically do
                writeTVar  downFail 0
                modifyTVar downBlk succ

  let warnExit = warn $ "peer loop exit" <+> pretty peer
  -- let stopLoop = none

  idle  <- liftIO $ newTVarIO 0

  fix \next -> do

    let thenNext m = m >> next

    npi <- newPeerInfo

    auth' <-  lift $ find (KnownPeerKey peer) id
    pinfo  <- lift $ fetch True npi (PeerInfoKey peer) id

    let mbauth = (,) <$> auth' <*> pure pinfo

    let noAuth = do
          let authNone = if isNothing auth' then "noauth" else ""
          warn ( "lost peer auth"  <+> pretty peer <+> pretty authNone  )
          warnExit

    maybe1 mbauth noAuth $ \_ -> do

      pt' <- getPeerThread peer

      maybe1 pt' warnExit $ \pt -> do

        liftIO $ atomically $ modifyTVar (view peerBlocksWip pt) (max 0 . pred)

        mbh <- getBlockForDownload peer

        case mbh of
          Nothing -> thenNext do
            idleNum <- liftIO $ atomically $ stateTVar idle $ \x -> (x, succ x)

            when (idleNum > 5) do
              trace $ "peer IDLE" <+> pretty peer
              liftIO $ atomically $ writeTVar idle 0
              x <- lift $ randomRIO (2.85, 10.47)
              pause @'Seconds (realToFrac x)

          Just h -> thenNext do

              liftIO $ atomically $ writeTVar idle 0

              trace $ "start download block" <+> pretty peer <+> pretty h

              mbSize2 <- blockSize brains peer h

              case mbSize2 of
                Just size -> do
                  trace $ "HAS SIZE:" <+> pretty peer <+> pretty h <+> pretty size
                  tryDownload pinfo h size

                Nothing -> do
                  r <- doBlockSizeRequest h
                  case r of
                    (Right (Just s)) -> do
                      tryDownload pinfo h s
                      pure ()

                    _ -> pure ()


        warnExit
        void $ delPeerThreadData peer

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

        dodo <- lift $ find cKey (view sBlockChunks)

        unless (isJust dodo) $ do
          debug $ "session lost for peer !" <+> pretty p

        dwnld <- MaybeT $ find cKey (view sBlockChunks)
        liftIO $ atomically $ writeTQueue dwnld (n, bs)
    }


