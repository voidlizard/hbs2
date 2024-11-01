{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language UndecidableInstances #-}
module BlockDownload where

import HBS2.Peer.Prelude
import HBS2.Actors.Peer
import HBS2.Data.Types.Peer
import HBS2.Data.Detect
import HBS2.Data.Types.Refs
import HBS2.Data.Bundle
import HBS2.Data.Types.SignedBox
import HBS2.Defaults
import HBS2.Events
import HBS2.Hash
import HBS2.Merkle
import HBS2.Net.PeerLocator
import HBS2.Peer.Proto
import HBS2.Storage
import HBS2.Storage.Operations.Missed
import HBS2.Misc.PrettyStuff

import PeerTypes
import PeerInfo

import Control.Concurrent.STM qualified as STM
import Control.Monad.Trans.Cont
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.HashMap.Strict qualified as HM
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Maybe
import Data.Either
import Data.ByteString.Lazy (ByteString)
import Data.List qualified as L
import Lens.Micro.Platform
import Codec.Serialise
import Streaming.Prelude qualified as S
import System.Random

import UnliftIO


trimFactor :: Double
trimFactor = 100


-- NOTE: if peer does not have a block, it may
--       cause to an unpleasant timeouts
--       So make sure that this peer really answered to
--       GetBlockSize request


downloadFromWithPeer :: forall e m . ( DownloadFromPeerStuff e m
                                     , e ~ L4Proto
                                     , HasPeerLocator e (BlockDownloadM e m) )
                     => Peer e
                     -> Integer
                     -> Hash HbSync
                     -> BlockDownloadM e m (Maybe ByteString)
downloadFromWithPeer peer thisBkSize h = do

  brains <- asks (view downloadBrains)

  npi <- newPeerInfo
  pinfo <- lift $ fetch True npi (PeerInfoKey peer) id

  sto <- lift getStorage

  let chunkSize = case view sockType peer of
        UDP -> defChunkSize
        TCP -> defChunkSize

  coo <- genCookie (peer,h)
  let key = DownloadSessionKey (peer, coo)
  let chusz = fromIntegral chunkSize -- defChunkSize
  dnwld <- newBlockDownload h
  let chuQ = view sBlockChunks dnwld
  let new =   set sBlockChunkSize chusz
            . set sBlockSize (fromIntegral thisBkSize)
              $ dnwld

  trace $ "downloadFromWithPeer STARTED" <+> pretty coo

  lift $ update @e new key id

  let burstSizeT = view peerBurst pinfo

  burstSize <- liftIO $ readTVarIO burstSizeT

  let offsets = calcChunks thisBkSize (fromIntegral chusz)  :: [(Offset, Size)]

  let chunkNums = [ 0 .. pred (length offsets) ]

  let bursts = calcBursts burstSize chunkNums

  -- let burstTime = min defChunkWaitMax $ realToFrac w :: Timeout 'Seconds
  -- trace $ "BURST TIME" <+> pretty burstTime

  let r = view sBlockChunks2 new
  rq <- liftIO newTQueueIO

  for_ bursts $ liftIO . atomically . writeTQueue rq

  rtt <- medianPeerRTT pinfo <&> fmap ( (/1e9) . realToFrac )
                             <&> fromMaybe 0.1

  r <- fix \next -> do
    burst <- liftIO $ atomically $ tryReadTQueue rq

    case burst of

      Just (i,chunksN) -> do
        let req = BlockGetChunks h chusz (fromIntegral i) (fromIntegral chunksN)

        void $ liftIO $ atomically $ STM.flushTQueue chuQ

        lift $ request peer (BlockChunks @e coo req)

        let waity = do
              fix \zzz -> do

                 wt <- race ( pause @'Seconds 1 ) (atomically $ peekTQueue chuQ >> STM.flushTQueue chuQ)

                 case wt of
                  Left{} -> pure False
                  Right{} -> do

                   d <- atomically do
                          m <- readTVar r
                          hc <- forM [i .. i + chunksN-1 ] $ \j -> do
                            pure (IntMap.member j m)

                          pure ( and hc )

                   if d then pure True else zzz

        catched <- race (pause @'Seconds 3 >> pure False) waity <&> either id id

        void $ liftIO $ atomically $ STM.flushTQueue chuQ

        if catched then do
          liftIO $ atomically do
            modifyTVar (view peerDownloaded pinfo) (+chunksN)
            writeTVar  (view peerPingFailed pinfo) 0

        else do

          liftIO $ atomically $ modifyTVar (view peerErrors pinfo) succ
          updatePeerInfo True peer pinfo

          newBurst <- liftIO $ readTVarIO burstSizeT
          -- let newBurst = max defBurst $ floor (realToFrac newBurst' * 0.5 )

          liftIO $ atomically $ modifyTVar (view peerDownloaded pinfo) (+chunksN)

          let chuchu = calcBursts newBurst [ i + n | n <- [0 .. chunksN] ]

          liftIO $ atomically $ modifyTVar (view peerErrors pinfo) succ

          trace $ "new burst: " <+> pretty newBurst
          trace1 $ red $ "missed chunks for request" <+> pretty (peer,i,chunksN)

          for_ chuchu $ liftIO . atomically . writeTQueue rq

        next

      Nothing -> do

        sz <- liftIO $ readTVarIO r <&> IntMap.size

        if sz >= length offsets then do
          pieces <- liftIO $ readTVarIO r <&> IntMap.elems
          let block = mconcat pieces
          let h1 = hashObject @HbSync block

          if h1 == h then do
            trace $ "PROCESS BLOCK" <+> pretty coo <+> pretty h
            -- onBlockDownloaded brains peer h
            pure (Just block)
          else do
            debug $ red "HASH NOT MATCH / PEER MAYBE JERK"
            pure Nothing

        else do
          debug $ red $ "RETRY BLOCK DOWNLOADING / ASK FOR MISSED CHUNKS"
          got  <- liftIO $ readTVarIO r <&> IntMap.keysSet
          let need = IntSet.fromList (fmap fromIntegral chunkNums)

          let missed = IntSet.toList $ need `IntSet.difference` got

          -- normally this should not happen
          -- however, let's try do download the tails
          -- by one chunk a time
          for_ missed $ \n -> do
            debug $ "MISSED CHUNK" <+> pretty coo <+> pretty n
            liftIO $ atomically $ writeTQueue rq (n,1)

          next

  lift $ expire @e key
  debug $ yellow $ "downloadFromWithPeer EXIT" <+> pretty coo
  pure r


instance HasPeerLocator e m => HasPeerLocator e (BlockDownloadM e m) where
  getPeerLocator = lift getPeerLocator


-- NOTE: updatePeerInfo is CC
--   updatePeerInfo is actuall doing CC (congestion control)

updatePeerInfo :: forall e m . (e ~ L4Proto, MonadIO m) => Bool -> Peer e -> PeerInfo e -> m ()

updatePeerInfo _ p pinfo | view sockType p == TCP = do
  liftIO $ atomically $ writeTVar (view peerBurst pinfo) 256

updatePeerInfo onError _ pinfo = do

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
                            let buN = min bmm (ceiling (realToFrac bu * 1.10))
                            pure (buN, trimUp win $ IntSet.insert buN buSet)
                          else do
                            let buM = headMay $ drop 1 $ IntSet.toDescList buSet
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



downloadOnBlockSize :: (MonadIO m, IsPeerAddr e m, MyPeer e)
                    => DownloadEnv e
                    -> (Peer e, Hash HbSync, Maybe Integer)
                    -> m ()

downloadOnBlockSize denv item@(p,h,size) = do
  let f = if isJust size then green else red
  debug $ f "GOT BLOCK SIZE" <+> pretty p <+> pretty h <+> pretty size
  atomically $ writeTVar (_blockInDirty denv) True
  atomically $ writeTQueue (_blockSizeRecvQ denv) item

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
                                   , PeerMessaging e
                                   , IsPeerAddr e m
                                   , HasPeerLocator e m
                                   , e ~ L4Proto
                                   )
                  => DownloadEnv e -> m ()
blockDownloadLoop env0 = do

  let blkInfoLock = 5  :: Timeout 'Seconds
  let blkWaitLock = 60 :: Timeout 'Seconds
  let workloadFactor = 1.10

  e <- ask
  sto <- getStorage

  let downT = 8
  let sizeT = 1

  inQ    <- withDownload env0 $ asks (view blockInQ)
  checkQ <- withDownload env0 $ asks (view blockCheckQ)
  sizeQ  <- newTQueueIO
  fetchQ <- newTQueueIO
  parseQ <- newTQueueIO
  sizeRQ <- withDownload env0 $ asks (view blockSizeRecvQ)

  -- FIXME: cleanup-nonce
  nonces <- newTVarIO (mempty :: HashMap (Peer e) PeerNonce)

  -- FIXME: cleanup-busy
  busy   <- newTVarIO (mempty :: HashMap PeerNonce Double)

  rates  <- newTVarIO (mempty :: IntMap.IntMap [(Peer e,PeerNonce)])

  fetchH <- newTVarIO (mempty :: HashSet (Hash HbSync))
  sizes  <- newTVarIO (mempty :: HashMap (Peer e, Hash HbSync) (Maybe Integer, TimeSpec))
  sizeReq <- newTVarIO (mempty :: HashMap (Hash HbSync) TimeSpec)

  seen   <- newTVarIO (mempty :: HashMap (Hash HbSync) Int)

  flip runContT pure do
    void $ ContT $ withAsync updatePeers

    -- UPDATE-STATS-LOOP
    void $ ContT $ withAsync $ updateRates e rates nonces

    replicateM_ downT $ ContT $ withAsync do
      forever do
        pause @'Seconds 120
        atomically do
          q <- readTVar inQ
          let isInQ x = HashMap.member x q
          modifyTVar' fetchH (HS.filter isInQ)
          modifyTVar' sizeReq (HM.filterWithKey (curry (isInQ . fst)))
          modifyTVar' sizes (HM.filterWithKey (curry (isInQ . snd . fst)))
          modifyTVar' seen (HM.filterWithKey (curry (isInQ . fst)))

          livePeers <- readTVar rates <&> mconcat . IntMap.elems
          let liveNonce = HS.fromList (fmap snd livePeers)
          let livePeer  = HS.fromList (fmap fst livePeers)

          modifyTVar' busy (HM.filterWithKey (\x _  -> HS.member x liveNonce))
          modifyTVar' nonces (HM.filterWithKey (\x _  -> HS.member x livePeer))

    replicateM_ downT $ ContT $ withAsync do
      forever do
        blk <-  atomically $ readTQueue checkQ
        here <- hasBlock sto blk <&> isJust
        if not here then do
          atomically $ writeTQueue sizeQ blk
        else do
          atomically $ writeTQueue parseQ blk

    void $ ContT $ withAsync do
      forever do
        blk <-  atomically $ readTQueue parseQ
        withDownload env0 do

          blks <- findMissedBlocks sto (HashRef blk)

          for_ blks $ \b -> do
            addDownload (Just blk) (fromHashRef b)

          processBlock blk
          deleteBlockFromQ blk

    replicateM_ 1 $ ContT $ withAsync do
      forever do

        -- pause @'Seconds 0.25

        items <-  atomically do
                     peekTQueue sizeRQ >> STM.flushTQueue sizeRQ

        now <- getTimeCoarse

        todo <- atomically do
          w <- for items $ \(p,h,s) -> do
                  modifyTVar sizes (HashMap.insert (p,h) (s, now))
                  readTVar nonces <&> HashMap.lookup p >>= \case
                    Nothing -> pure ()
                    Just nonce -> setBusySTM nonce busy (Just (setFactor 0 (0.01-)))
                  pure h

          for (L.nub w) pure

        for_ todo $ \b -> do
          here <- hasBlock sto b <&> isJust

          already <- atomically do
            readTVar fetchH <&> HS.member b

          when (not here && not already) do
            atomically $ writeTQueue fetchQ b

    replicateM_ sizeT $ ContT $ withAsync do

      -- TODO: trim-sizeReq
      let blocks = readTVarIO sizeReq <&> HashMap.keys <&> fmap (,2)

      polling (Polling 1 1) blocks $ \h -> do
         pips <- readTVarIO nonces <&> HashMap.keys
         s <- readTVarIO sizes <&> HashMap.toList

         for_ pips $ \p -> do
          here <- lookupSizeIO sizes p h <&> isRight

          if here then do
            atomically $ modifyTVar sizeReq (HashMap.delete h)
          else
            request p (GetBlockSize @e h)


    replicateM_ sizeT $ ContT $ withAsync do

      forever do

        blk <-  atomically do
          readTVar rates <&> not . IntMap.null >>= STM.check
          readTQueue sizeQ

        debug $ green "PEER SIZE THREAD" <+> pretty blk


        r <- readTVarIO rates <&> IntMap.toDescList
                              <&> foldMap snd


        answ <- for r $ \(p,nonce) -> do
          lookupSizeIO sizes p blk >>= \case
            -- уже спрашивали, отрицает
            Left{} -> do
              npi <- newPeerInfo
              PeerInfo{..} <- fetch True npi (PeerInfoKey p) id

              atomically do
                setBusySTM nonce busy (Just (setFactor 0 (+(-0.01))))
                modifyTVar _peerDownloadMiss succ
                modifyTVar seen (HashMap.insertWith (+) blk 1)
                modifyTVar sizeReq (HashMap.delete blk)

              debug $ red "NONE:" <+> pretty p <+> pretty blk
              pure 0

            -- уже спрашивали, ответил
            Right (Just w) -> do

              atomically do
                setBusySTM nonce busy (Just (setFactor 0 (+(-0.01))))
                modifyTVar sizeReq (HashMap.delete blk)

              debug $ red "SIZE:" <+> pretty p <+> pretty blk <+> pretty w
              pure 1

            -- не спрашивали еще
            Right Nothing -> do
              (doReq, f) <- atomically do
                f <- lookupBusySTM nonce busy
                if f > workloadFactor then
                  pure (False, f)
                else do
                  setBusySTM nonce busy (Just (setFactor 0.01 (+0.01)))
                  pure (True, f)

              debug $ green "BUSY" <+> pretty p <+> pretty f

              when doReq do
                debug $ red "SEND REQUEST FOR SIZE" <+> pretty p <+> pretty blk
                async $ do
                  pause blkInfoLock
                  atomically (setBusySTM nonce busy (Just (setFactor 0 (+(-0.01)))))

                withPeerM e $ request p (GetBlockSize @e blk)
                now <- getTimeCoarse
                atomically $ modifyTVar sizeReq (HashMap.insert blk now)

              pure 0

        if sum answ > 0 then do
          atomically do
            here <- readTVar fetchH <&> HS.member blk
            readTVar seen <&> HM.delete blk
            unless here $
              writeTQueue fetchQ blk

        else do
          howMany <- readTVarIO seen  <&> (fromMaybe 0 . HashMap.lookup blk)
          pips <- readTVarIO nonces <&> HM.size
          -- FIXME: hardcode
          when (howMany < 10) do
            atomically $ writeTQueue sizeQ blk

    void $ ContT $ withAsync do
      -- FIXME: ban-time-hardcode
      let loosers = readTVarIO seen <&> fmap (,120) . HashMap.keys
      polling (Polling 1 10) loosers $ \it -> do
        atomically $ writeTQueue checkQ it
        atomically $ modifyTVar seen (HashMap.delete it)

    replicateM_ downT $ ContT $ withAsync do

      gen <- newStdGen

      forever do

        flip runContT pure $ callCC \exit -> do

          blk <- atomically $ readTQueue fetchQ

          atomically do
            modifyTVar fetchH (HS.insert blk)

          here <- hasBlock sto blk <&> isJust

          when here $ exit ()

          debug $ green "PEER DOWNLOAD THREAD" <+> pretty blk

          -- TODO: already-downloaded-possible

          let ws = round . (*trimFactor) <$> randomRs (0, 2.5) gen

          work <- lift $ race (pause @'Seconds 60) $ atomically do
            r0 <- readTVar rates <&> IntMap.toList
            bsy <- readTVar busy

            let bx nonce =
                 round $ trimFactor * (1.75 / (1.0 + fromMaybe 0 (HashMap.lookup nonce bsy)))

            let w = [ (-(v + w0 + bx nonce), p)
                    | (v, (w0, peers)) <- zip ws r0, p@(_,nonce) <- peers
                    ] & L.sortOn fst & fmap snd

            avail' <- for w $ \(peer,nonce) -> do
                       p <- readTVar busy <&> HashMap.lookup nonce
                       sz <- lookupSizeSTM sizes peer blk
                       if p < Just workloadFactor then
                         pure (Just (peer,nonce, sz))
                       else
                         pure Nothing

            let avail = catMaybes avail'

            STM.check (not $ L.null avail)

            found <- for avail $ \(pip, nonce, msz) -> case msz of
              Right (Just sz) -> do
                pure $ Just (blk, pip, nonce, sz)

              _  -> pure Nothing

            case headMay (catMaybes found) of
              Nothing -> do
                writeTQueue checkQ blk
                modifyTVar fetchH (HS.delete blk)
                pure Nothing

              Just what@(_,_,nonce,_) -> do
                setBusySTM nonce busy (Just (setFactor 1.0 (+1.0)))
                pure $ Just what

          case work of
            Right (Just (b,p,nonce,s)) -> do
              debug $ green "WORKER CHOOSEN" <+> pretty p <+> pretty blk <+> pretty s
              r <- lift $ race (pause @'Seconds 60) (withDownload env0 $ downloadFromWithPeer p s b)

              atomically do
                setBusySTM nonce busy (Just (setFactor 0 (const 0)))

              npi <- newPeerInfo
              PeerInfo{..} <- lift $ fetch True npi (PeerInfoKey p) id

              debug $ green "DOWNLOAD DONE!" <+> pretty p <+> pretty blk <+> pretty s <+> pretty (isRight r)

              atomically $ modifyTVar fetchH (HS.delete blk)

              case r of
                Right (Just block) -> do
                  mh <- putBlock sto block
                  atomically do
                    modifyTVar _peerDownloaded succ
                    modifyTVar _peerDownloadedBlk succ

                  case mh of
                    Nothing -> err $ red "storage write error!"
                    Just h-> do
                      atomically $ writeTQueue parseQ h
                _ -> do
                  debug $ red "DOWNLOAD FAILED / TIMEOUT"
                  atomically do
                    modifyTVar _peerDownloadFail succ
                    modifyTVar _peerErrors succ
                    writeTQueue checkQ blk

            _ -> do
                debug $ red "WAIT FOR PEERS TIMEOUT" <+> pretty blk
                atomically $ writeTVar busy mempty

    void $ ContT $ withAsync $ forever do
      pause @'Seconds 10
      let DownloadEnv{..} = env0
      let DownloadMonEnv{..} = _downloadMon
      p <- readTVarIO _downloadProbe
      acceptReport p =<< S.toList_ do
        S.yield =<< liftIO (readTVarIO _blockInQ <&> ("blockInQ",) . fromIntegral . HashMap.size)
        S.yield =<< liftIO (readTVarIO _downloads <&> ("downloads",) . fromIntegral . HashMap.size)
        S.yield =<< liftIO (readTVarIO nonces <&> ("nonces",) . fromIntegral . HashMap.size)
        S.yield =<< liftIO (readTVarIO busy <&> ("busy",) . fromIntegral . HashMap.size)
        S.yield =<< liftIO (readTVarIO rates <&> ("rates",) . fromIntegral . IntMap.size)
        S.yield =<< liftIO (readTVarIO fetchH <&> ("fetchH",) . fromIntegral . HS.size)
        S.yield =<< liftIO (readTVarIO sizes <&> ("sizes",) . fromIntegral . HashMap.size)
        S.yield =<< liftIO (readTVarIO sizeReq <&> ("sizeReq",) . fromIntegral . HashMap.size)
        S.yield =<< liftIO (readTVarIO seen <&> ("seen",) . fromIntegral . HashMap.size)

    forever do
      withPeerM e $ withDownload env0 do
        pause @'Seconds 5
        wip <- asks _blockInQ >>= readTVarIO <&> HashMap.size
        notice $ yellow "wip" <+> pretty wip

  where

    setFactor d f = \case
      Nothing -> Just d
      Just v  -> Just (g v)
      where
        g y = f y & max 0

    setBusySTM nonce busy = \case
      Nothing -> modifyTVar busy (HashMap.delete nonce)
      Just fn -> modifyTVar busy (HashMap.alter fn nonce)

    lookupBusySTM nonce busy =
      readTVar busy <&> fromMaybe 0 . HashMap.lookup nonce

    lookupSizeSTM sizes p h = do
        readTVar sizes
          <&> HashMap.lookup (p,h)
          <&> \case
            Nothing           -> Right Nothing
            Just (Just x,_)   -> Right (Just x)
            Just (Nothing,_)  -> Left ()

    lookupSizeIO sizes p h = do
      atomically $ lookupSizeSTM sizes p h

    updateRates e rates nonces = withPeerM e do

      let wRtt = 5
      let wUdp = 1.5
      let wTcp = 1.1
      let wS   = 1.5
      let eps  = 1e-8

      forever do
        pause @'Seconds 20

        new <- S.toList_ do
          withPeerM e $ forKnownPeers @e $ \peer pd -> do
            pinfo <- find (PeerInfoKey peer) id
            maybe1 pinfo none $ \pip -> do

              let nonce = _peerOwnNonce pd

              atomically $ modifyTVar nonces (HashMap.insert peer nonce)

              sr <- readTVarIO (_peerDownloaded pip)
              er <- readTVarIO (_peerDownloadFail pip)

              let s = (eps  + realToFrac sr) / (eps + realToFrac (sr + er))

{- HLINT ignore "Functor law" -}
              rtt <- medianPeerRTT pip
                       <&> fmap ( (/1e9) . realToFrac )
                       <&> fromMaybe 1.0

              let (udp,tcp) = case view sockType peer of
                                UDP -> (0, wUdp * 1.0)
                                TCP -> (wTcp * 1.0, 0)

              let r = udp + tcp + wS*s
              lift $ S.yield (peer, nonce, (r, rtt))

        let maxRtt = maximumDef 1.0 [ rtt | (_, _, (_, rtt)) <- new ]

        let mkRate s rtt = round $ trimFactor * (s + wRtt * (1 / (1 + rtt / maxRtt)))

        let newRates = [ (mkRate s rtt, [(p,nonce)] )
                       | (p, nonce, (s, rtt)) <- new
                       ]


        atomically do
          writeTVar rates (IntMap.fromListWith (<>) newRates)

        debug $ green "PEER RATES" <+> line <> vcat (fmap fmt newRates)

      where
        fmt (r,prs) = pretty r <+> hcat (fmap (pretty . view _1) prs)

    updatePeers = do
      e <- ask
      pl <- getPeerLocator @e
      forever $ withPeerM e do
          pause @'Seconds 3.0

          pee <- knownPeers @e pl
          npi <- newPeerInfo

          for_ pee $ \p -> do
            pinfo <- fetch True npi (PeerInfoKey p) id
            updatePeerInfo False p pinfo


processBlock :: forall e s m . ( MonadIO m
                               , HasStorage m
                               , MyPeer e
                               , ForSignedBox s
                               , s ~ Encryption e
                               , HasPeerLocator e (BlockDownloadM e m)
                               )
             => Hash HbSync
             -> BlockDownloadM e m ()

processBlock h = do

   sto <- lift getStorage

   brains <- asks (view downloadBrains)

   let parent = Just h

   block <- liftIO $ getBlock sto h

   let bt = tryDetect h <$> block

   -- FIXME:  если блок нашёлся, то удаляем его из wip

   when (isJust bt) (deleteBlockFromQ h)

   let handleHrr (hrr :: Either (Hash HbSync) [HashRef]) = do
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
        debug $ "GOT AnnotatedHashRef" <+> pretty a
        processBlock (fromHashRef a)

      addDownload parent (fromHashRef b)

     Just (AnnRef (AnnotatedHashRef ann hx)) -> do
       maybe1 ann none $ addDownload parent . fromHashRef
       addDownload parent (fromHashRef hx)

     Just (MerkleAnn ann) -> do
       case _mtaMeta ann of
          NoMetaData -> pure ()
          ShortMetadata {} -> pure ()
          AnnHashRef hx -> addDownload parent hx

       case _mtaCrypt ann of
          NullEncryption -> pure ()
          CryptAccessKeyNaClAsymm h -> addDownload parent h
          EncryptGroupNaClSymm h _  -> addDownload parent h

       trace $ "GOT WRAPPED MERKLE. requesting nodes/leaves" <+> pretty h
       walkMerkleTree (_mtaTree ann) (liftIO . getBlock sto) handleHrr

     Just (Merkle{}) -> do
       trace $ "GOT MERKLE. requesting nodes/leaves" <+> pretty h
       walkMerkle h (liftIO . getBlock sto) handleHrr

     Just (Blob{}) -> do
       -- NOTE: bundle-ref-detection-note
       --  добавлять обработку BundleRefValue в tryDetect
       --  слишком накладно, т.к. требует большого количества
       --  констрейнтов, которые не предполагались там
       --  изначально. Как временная мера -- пробуем Bundle
       --  обнаруживать здесь.
       mon <- asks (view downloadMon)
       runMaybeT do
        bs <- MaybeT $ pure block

        -- TODO: check-if-we-somehow-trust-this-key
        (pk, BundleRefSimple ref) <- MaybeT $ pure $ deserialiseOrFail @(BundleRefValue s) bs
                                       & either (const Nothing) unboxBundleRef

        debug $ "GOT BundleRefValue" <+> parens (pretty ref)

        downloadMonAdd mon ref do
          debug $ "Downloaded bundle:" <+> pretty ref
          r <- importBundle sto (void . putBlock sto . snd) ref
          case  r of
            Right{} -> debug $ "Imported bundle: " <+> pretty ref
            Left e  -> err (viaShow e)

        lift $ addDownload parent (fromHashRef ref)

       pure ()

    where
      unboxBundleRef (BundleRefValue box) = unboxSignedBox0 box


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

--        debug $ "FINDING-SESSION:" <+> pretty c <+> pretty n
--        debug $ "GOT SHIT" <+> pretty c <+> pretty n

        se <- MaybeT $ find cKey id
        let dwnld  = view sBlockChunks se
        let dwnld2 = view sBlockChunks2 se

        -- debug $ "WRITE SHIT" <+> pretty c <+> pretty n
        liftIO $ atomically do
          writeTQueue dwnld (n, bs)
          modifyTVar' dwnld2 (IntMap.insert (fromIntegral n) bs)
    }


