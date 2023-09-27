{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language UndecidableInstances #-}
module BlockDownload where

import HBS2.Actors.Peer
import HBS2.Clock
import HBS2.Data.Detect
import HBS2.Data.Types.Refs
import HBS2.Data.Bundle
import HBS2.Data.Types.SignedBox
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
import DownloadMon

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TSem
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Cache qualified as Cache
import Data.Foldable hiding (find)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List qualified as List
import Data.Maybe
import Lens.Micro.Platform
import System.Random (randomRIO)
import System.Random.Shuffle (shuffleM)
import Codec.Serialise

getBlockForDownload :: forall e m . (MonadIO m, IsPeerAddr e m, MyPeer e, HasStorage m)
                     => Peer e
                     -> BlockDownloadM e m (Maybe (Hash HbSync))

getBlockForDownload peer = do
  pa <- lift $ toPeerAddr peer
  tinq <- asks (view blockInQ)
  brains <- asks (view downloadBrains)
  prop <- asks (view blockProposed)

  sto <- lift getStorage

  inq <- liftIO $ readTVarIO tinq
  -- let size = HashMap.size inq

  let allBlks = HashMap.keys inq

  hs' <- forM allBlks $ \blk -> do
          here <- liftIO $ hasBlock sto blk <&> isJust
          newOne <- shouldDownloadBlock @e brains peer blk

          if not here && newOne then do
            pure $ Just blk
          else do
            po <- shouldPostponeBlock @e brains blk

            when po do
              postponeBlock blk

            pure Nothing

  let hs = catMaybes hs'
  let size = length hs

  if size == 0 then do
    pure Nothing
  else do
    i <- randomRIO (0, size - 1)
    let blk = HashMap.keys inq !! i
    pure $ Just blk


processBlock :: forall e m . ( MonadIO m
                             , HasStorage m
                             , MyPeer e
                             , ForSignedBox e
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

   when (isJust bt) (removeFromWip h)

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

       case _mtaCrypt ann of
          NullEncryption -> pure ()
          CryptAccessKeyNaClAsymm h -> addDownload parent h
          EncryptGroupNaClSymm h _  -> addDownload parent h

       debug $ "GOT WRAPPED MERKLE. requesting nodes/leaves" <+> pretty h
       walkMerkleTree (_mtaTree ann) (liftIO . getBlock sto) handleHrr

     Just (Merkle{}) -> do
       debug $ "GOT MERKLE. requesting nodes/leaves" <+> pretty h
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
        (pk, BundleRefSimple ref) <- MaybeT $ pure $ deserialiseOrFail @(BundleRefValue e) bs
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
                     -> BlockDownloadM e m ()
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

  rtt <- medianPeerRTT pinfo <&> fmap ( (/1e9) . realToFrac )
                             <&> fromMaybe defChunkWaitMax

  let w = 4 * rtt * realToFrac (length bursts)

  let burstTime = max defChunkWaitMax $ realToFrac w :: Timeout 'Seconds

  trace $ "BURST TIME" <+> pretty burstTime

  let r = view sBlockChunks2 new
  rq <- liftIO newTQueueIO

  for_ bursts $ liftIO . atomically . writeTQueue rq

  fix \next -> do
    burst <- liftIO $ atomically $ tryReadTQueue rq

    case burst of

      Just (i,chunksN) -> do
        let req = BlockGetChunks h chusz (fromIntegral i) (fromIntegral chunksN)

        void $ liftIO $ atomically $ flushTQueue chuQ

        lift $ request peer (BlockChunks @e coo req)

        let waity = liftIO $ race ( pause burstTime >> pure False ) do
              fix \zzz -> do
                hc <- atomically do
                  forM [i .. i + chunksN-1 ] $ \j -> do
                    m <- readTVar r
                    pure (j, IntMap.member j m)

                let here = and $ fmap snd hc
                if here then do
                    pure here

                else do
                    pause rtt
                    zzz

        void $ liftIO $ race ( pause (2 * rtt)  ) $ atomically do
                 void $ peekTQueue chuQ
                 flushTQueue chuQ

        catched <- waity <&> either id id

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
          trace $ "missed chunks for request" <+> pretty (i,chunksN)
          trace $ "burst time" <+> pretty burstTime

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
            trace $ "MISSED CHUNK" <+> pretty coo <+> pretty n
            liftIO $ atomically $ writeTQueue rq (n,1)

          next

  lift $ expire @e key
  trace $ "downloadFromWithPeer EXIT" <+> pretty coo


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
                            let buN = min bmm (ceiling (realToFrac bu * 1.25))
                            pure (buN, trimUp win $ IntSet.insert buN buSet)
                          else do
                            let buM = headMay $ drop 1 $ IntSet.toDescList buSet
                            writeTVar (view peerBurstMax pinfo) buM
                            let buN = headDef defBurst $ drop 2 $ IntSet.toDescList buSet
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
                                   , PeerMessaging e
                                   , IsPeerAddr e m
                                   , HasPeerLocator e m
                                   , e ~ L4Proto
                                   )
                  => DownloadEnv e -> m ()
blockDownloadLoop env0 = do

  e    <- ask

  let blks = mempty

  pl <- getPeerLocator @e

  pause @'Seconds 3.81

  let withAllStuff = withPeerM e . withDownload env0

  -- FIXME: exception-handling
  void $ liftIO $ async $ withPeerM e do
    downloadMonLoop (view downloadMon env0)

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
      updatePeerInfo False p pinfo


  void $ liftIO $ async $ forever $ withAllStuff do
    pause @'Seconds 5 -- FIXME: put to defaults
                      --        we need to show download stats

    wipNum  <- asks (view blockInQ) >>= liftIO . readTVarIO <&> HashMap.size
    po <- postponedNum

    notice $ "maintain blocks wip" <+> pretty wipNum
                                   <+> "postponed"
                                   <+> pretty po

  busyPeers <- liftIO $ newTVarIO (mempty :: HashSet (Peer e))
  released  <- liftIO newTQueueIO

  npi <- newPeerInfo

  liftIO $ withAllStuff do
    brains <- asks (view downloadBrains)

    fix \next -> do
      wipNum  <- asks (view blockInQ) >>= liftIO . readTVarIO <&> HashMap.size

      when (wipNum == 0) do
        pause @'Seconds 1
        next

      allPips <- lift $ getKnownPeers @e

      onKnownPeers brains allPips

      pips <- flip filterM allPips $
                \p -> liftIO do
                 busy <- readTVarIO busyPeers <&> HashSet.member p
                 pure $ not busy

      when (List.null pips) do
        void $ liftIO $ race (pause @'Seconds 5) $ do
          trace "ALL PEERS BUSY"
          void $ liftIO $ atomically $ do
            p <- readTQueue released
            ps <- flushTQueue released
            for_ (p:ps) $ \x -> do
              modifyTVar busyPeers (HashSet.delete x)
        next

      for_ pips $ \p -> do
        h0 <- getBlockForDownload p

        -- trace $ "getBlockForDownload" <+> pretty p <+> pretty h0

        -- FIXME: busyloop-when-no-block-for-peer
        maybe1 h0 (pure ()) $ \h -> do

          liftIO $ atomically $ do
            modifyTVar busyPeers (HashSet.insert p)

          void $ liftIO $ async $ withAllStuff do

            -- trace $ "start downloading shit" <+> pretty p <+> pretty h

            lift $ onBlockDownloadAttempt brains p h

            pinfo <- lift $ fetch True npi (PeerInfoKey p) id
            size' <- blockSize brains p h

            esize <- case size' of
              Nothing -> do
                doBlockSizeRequest p h

              Just s  -> pure (Right (Just s))

            case esize of
              Left{}         -> pure ()
              Right Nothing  -> do
                let downMiss = view peerDownloadMiss pinfo
                liftIO $ atomically $ modifyTVar downMiss succ

              Right (Just size) -> do
                -- trace $ "BLOCK SIZE" <+> pretty p <+> pretty h <+> pretty size
                let downFail = view peerDownloadFail pinfo
                let downBlk  = view peerDownloadedBlk pinfo

                r <- liftIO $ race ( pause defBlockWaitMax )
                                $ withAllStuff
                                $ downloadFromWithPeer p size h

                liftIO $ atomically $ writeTQueue released p

                case r of
                  Left{} -> do
                    liftIO $ atomically $ modifyTVar downFail succ
                    failedDownload p h

                  Right{} -> do
                    onBlockDownloaded brains p h
                    liftIO $ withAllStuff $ processBlock h
                    liftIO $ atomically do
                      writeTVar  downFail 0
                      modifyTVar downBlk succ

            -- trace $ "exit download thread" <+> pretty p <+> pretty h
            liftIO $ atomically $ writeTQueue released p

      next

  withDownload env0 do

    mapM_ processBlock blks

    proposed <- asks (view blockProposed)

    void $ liftIO $ async $ forever do
      pause @'Seconds 20
      -- debug "block download loop. does not do anything"
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

  void $ liftIO $ withPeerM e $ withDownload env0 do
    forever do
      pause @'Seconds 30
      trace "UNPOSTPONE LOOP"
      po <- asks (view blockPostponedTo) >>= liftIO . Cache.toList
      for_ po $ \(h, _, expired) -> do
        when (isJust expired) do
          unpostponeBlock h

doBlockSizeRequest :: forall e m . ( MyPeer e
                                 , Sessions e (KnownPeer e) m
                                 , Request e (BlockInfo e) m
                                 , EventListener e (BlockInfo e) m
                                 , DownloadFromPeerStuff e m
                                 , HasPeerLocator e m
                                 , IsPeerAddr e m
                                 , m ~ PeerM e IO
                                 )
                   => Peer e
                   -> Hash HbSync
                   -> BlockDownloadM e m (Either () (Maybe Integer))

doBlockSizeRequest peer h = do

  brains <- asks (view downloadBrains)

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


