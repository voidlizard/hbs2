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
import HBS2.Storage.Operations.Missed
import HBS2.System.Logger.Simple

import PeerTypes
import PeerInfo
import Brains
import DownloadMon

import Control.Concurrent.STM qualified as STM
import Control.Monad.Trans.Cont
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.Cache qualified as Cache
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.IntMap qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Maybe
import Lens.Micro.Platform
import Codec.Serialise
import Data.Hashable
import System.Random.Shuffle (shuffleM)
import Control.Concurrent (getNumCapabilities)

import UnliftIO


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

  let w = 10 * rtt * realToFrac (length bursts)

  let burstTime = min defChunkWaitMax $ realToFrac w :: Timeout 'Seconds

  trace $ "BURST TIME" <+> pretty burstTime

  let r = view sBlockChunks2 new
  rq <- liftIO newTQueueIO

  for_ bursts $ liftIO . atomically . writeTQueue rq

  fix \next -> do
    burst <- liftIO $ atomically $ tryReadTQueue rq

    case burst of

      Just (i,chunksN) -> do
        let req = BlockGetChunks h chusz (fromIntegral i) (fromIntegral chunksN)

        void $ liftIO $ atomically $ STM.flushTQueue chuQ

        lift $ request peer (BlockChunks @e coo req)

        let waity = liftIO $ race ( pause burstTime >> pure False ) do
              fix \zzz -> do
                hc <- atomically do
                  forM [i .. i + chunksN-1 ] $ \j -> do
                    m <- readTVar r
                    pure (j, IntMap.member j m)

                let here = all snd hc
                if here then do
                    pure here

                else do
                    pause rtt
                    zzz

        void $ liftIO $ race ( pause (8 * rtt)  ) $ atomically do
                 void $ peekTQueue chuQ
                 STM.flushTQueue chuQ

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
            deleteBlockFromQ h
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

data ByFirst a b = ByFirst a b

instance Eq a => Eq (ByFirst a b) where
  (==) (ByFirst a _) (ByFirst b _) = a == b

instance Hashable a => Hashable (ByFirst a b) where
  hashWithSalt s (ByFirst a _) = hashWithSalt s a


data DTask =
  DTask
  { _dtaskBlock     :: Hash HbSync
  , _dtaskBlockSize :: Integer
  }

data DState e =
  DState
  { _dPeerInbox   :: TVar (HashMap (Peer e) (TBQueue DTask, [Async ()]))
  }

data PState =
    PIdle
  | PWork DTask
  | PCheckPeer

newDState :: forall e m . (MonadUnliftIO m, MyPeer e) => m (DState e)
newDState = DState @e <$> newTVarIO mempty

downloadOnBlockSize :: (MonadIO m, IsPeerAddr e m)
                    => DownloadEnv e
                    -> (Peer e, Hash HbSync, Maybe Integer)
                    -> m ()

downloadOnBlockSize denv (p,h,size) = do
  maybe1 size none $ \s -> do
    debug $ "GOT BLOCK SIZE" <+> pretty h
    onBlockSize (_downloadBrains denv) p h s
    atomically $ writeTVar (_blockInDirty denv) True

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

-- [dmz@minipig:~/w/hbs2]$ hbs2 cat 8is4yaZLi4sK3mPSS7Z9yrJK8dRXQyrcD54qe1GWi8qe | wc -c
-- 1278173938

   -- MiB       (RX Bytes/second)
   -- 90.25 .....|.............||.....
   -- 75.21 .....||||||..||||.|||.....
   -- 60.17 ....||||||||||||||||||....
   -- 45.13 ....||||||||||||||||||....
   -- 30.08 ....|||||||||||||||||||...
   -- 15.04 ::::|||||||||||||||||||:::
   --     1     15   20   25   30   35

   -- MiB       (RX Bytes/second)
   -- 74.60 ......|||||..|||||||.|.|...
   -- 62.17 ......||||||||||||||||||...
   -- 49.74 ......||||||||||||||||||...
   -- 37.30 ......|||||||||||||||||||..
   -- 24.87 ......|||||||||||||||||||..
   -- 12.43 :::::|||||||||||||||||||||:
     --     1  10   15   20   25   30

  -- FIXME: asap-fix-async-spawning

  e    <- ask

  pl <- getPeerLocator @e

  sto <- getStorage

  pause @'Seconds 3.81

  let withAllStuff = withPeerM e . withDownload env0

  flip runContT pure do

    -- FIXME: exception-handling
    void $ ContT $ withAsync $ withPeerM e do
      downloadMonLoop (view downloadMon env0)

    void $ ContT $ withAsync $ forever $ withPeerM e do
      pause @'Seconds 30

      pee <- knownPeers @e pl
      npi <- newPeerInfo

      for_ pee $ \p -> do
        pinfo <- fetch True npi (PeerInfoKey p) id
        liftIO $ atomically $ writeTVar (view peerBurstMax pinfo) Nothing


    void $ ContT $ withAsync $ forever $ withPeerM e do
      pause @'Seconds 1.5

      pee <- knownPeers @e pl
      npi <- newPeerInfo

      for_ pee $ \p -> do
        pinfo <- fetch True npi (PeerInfoKey p) id
        updatePeerInfo False p pinfo


    void $ ContT $ withAsync $ withAllStuff do

      brains <- asks (view downloadBrains)
      q <- asks (view blockInQ)

      let refs = liftIO $ readTVarIO q <&> HashMap.keys <&> fmap (,2)

      polling (Polling 5 2.5) refs $ \h -> do
        here <- hasBlock sto h <&> isJust

        if here then do
          deleteBlockFromQ h
        else  do
          po <- shouldPostponeBlock @e brains h
          when po do
            postponeBlock h

    void $ ContT $ withAsync $ forever $ withAllStuff do
      printDownloadStats

    -- inQ <- asks (view blockInQ)
    -- brains <- asks (view downloadBrains)

    -- void $ ContT $ withAsync (withPeerM e (preRequestSizes brains rcache inQ))

    state <- liftIO $ newDState @e

    cores <- liftIO getNumCapabilities

    -- FIXME: limit-cores-number
    trace $ "!@!! CORES !!!!!" <+> pretty cores

    let inboxCap = 200
    sizeRq <- newTBQueueIO (10 * inboxCap)

    void $ ContT $ withAsync $ withAllStuff $ forever do
      req <- atomically (readTBQueue sizeRq)
      withPeerM e $ broadCastMessage @e  req

    void $ ContT $ withAsync $ withAllStuff $ forever do
      q <- asks (view blockInQ)
      dirty <- asks (view blockInDirty)
      brains <- asks (view downloadBrains)

      now <- liftIO getTimeCoarse

      blocks <- readTVarIO q <&> HashMap.toList
                     >>= liftIO . shuffleM

      for_ blocks $ \(block, status) -> void $ runMaybeT do
         sst <- readTVarIO (_bsState status)

         case sst of
           BlkNew -> do
            trace $ "GOT NEW BLOCK" <+> pretty block
            atomically $ do
              full <- isFullTBQueue sizeRq
              unless full do
                writeTVar (_bsState status) (BlkSizeAsked now)
                writeTBQueue sizeRq (GetBlockSize @e block)

           BlkSizeAsked t0 -> do

             trace $ "BLOCK WAIT SIZE" <+> pretty block

             ss <- readTVarIO (_dPeerInbox state)

             candidates' <- for (HashMap.toList ss) $ \(peer, inbox) -> do
               pinfo <- withPeerM e $ find (PeerInfoKey peer) id

               rtt  <-  runMaybeT (toMPlus pinfo >>= medianPeerRTT >>= toMPlus)
                          <&> fmap (logBase 10 . realToFrac)

               bs <- blockSize brains peer block

               maybe1 bs (pure Nothing) $ \size -> do
                 should <- shouldDownloadBlock @e brains peer block
                 if not should
                   then pure Nothing
                 else do
                   pure (Just (peer, size, inbox))

             candidate <- liftIO $ shuffleM (catMaybes candidates') <&> headMay
             -- candidate <- pure (catMaybes candidates') <&> headMay

             forM_ candidate $ \(_, size, inbox) -> do
               -- поток-читатель исчез, по таймауту, скорее всего. ДИХСН.
               -- может, в лог написать.
               void $ liftIO $ try @_ @SomeException $ atomically do
                full <- isFullTBQueue (fst inbox)
                unless full do
                  writeTVar ( _bsState status) (BlkDownloadStarted now)
                  writeTBQueue (fst inbox) (DTask block size)

             when (isNothing candidate && expired defBlockInfoTimeout (now - t0) ) do
                -- на самом деле можно считать, и отправлять блоки в отстой
                atomically $ writeTVar (_bsState status) BlkNew

           BlkDownloadStarted t0 | expired (600 :: Timeout 'Seconds) (now - t0) -> do
             here <- liftIO $ hasBlock sto block <&> isJust
             if here then do
                lift $ deleteBlockFromQ block
             else do
                trace $ "BLOCK DOWNLOAD FAIL" <+> pretty block
                atomically $ writeTVar (_bsState status) BlkNew

           _ -> none

      -- FIXME: normal-waiting-for-what?
      -- тут надо как-то моднее ждать
      void $ race (pause @'Seconds 1) $ atomically do
        readTVar dirty >>= STM.check
        writeTVar dirty False

    npi <- newPeerInfo

    lift $ withAllStuff do
      brains <- asks (view downloadBrains)
      dirty <-  asks (view blockInDirty)

      let refs = withPeerM e (getKnownPeers @e <&> fmap (,60))

      polling (Polling 5 60) refs $ \peer -> do
        debug $ "SOME FUCKING PEER:" <+> pretty peer

        -- ШАГ 1. Поллим пиров, создаём новых, если для них нет зареганой очереди
        here <- readTVarIO (_dPeerInbox state) <&> HashMap.member peer

        -- ШАГ 2. Создаём тред + инбокс если нету
        unless here do
          q <- newTBQueueIO inboxCap

          ass <- replicateM cores $ async $ flip runContT pure do

                   pinfo <- withPeerM e $ fetch True npi (PeerInfoKey peer) id

                   let downFail = view peerDownloadFail pinfo
                   let downBlk  = view peerDownloadedBlk pinfo

                   void $ ContT $ bracket none $ const $ do
                     atomically do
                      m <- readTVar (_dPeerInbox state)
                      let v =  HashMap.lookup peer m
                      forM_ v (STM.flushTBQueue  . fst)
                      writeTVar (_dPeerInbox state) (HashMap.delete peer m)

                   -- pause @'Seconds 1
                   flip fix PIdle $ \next -> \case
                     PIdle -> do
                        what <- liftIO do
                                  r <- race (pause @'Seconds 60)
                                            (try @_ @SomeException (atomically $ readTBQueue q))
                                  case r of
                                    Left _          -> pure (Left True)
                                    Right (Left{})  -> pure (Left False)
                                    Right (Right x) -> pure (Right x)

                        case what of
                          Left True -> next PCheckPeer
                          Left False -> pure ()
                          Right todo -> do
                            next (PWork todo)

                     PCheckPeer -> do
                        debug $ "PEER CHECK" <+> pretty peer
                        auth <- withPeerM e (find (KnownPeerKey peer) id <&> isJust)

                        when auth do
                          next PIdle

                        debug "PEER FINISHING"

                     PWork (DTask{..}) -> do
                        debug $ "PEER IS WORKING"  <+> pretty peer <+> pretty _dtaskBlock

                        let (p,h) = (peer, _dtaskBlock)

                        onBlockDownloadAttempt brains peer h

                        -- FIXME: ASAP-hardcode
                        r <- liftIO $ race ( pause ( 10 :: Timeout 'Seconds) )
                                          $ withPeerM e
                                          $ withDownload env0
                                          $ downloadFromWithPeer peer _dtaskBlockSize _dtaskBlock

                        withPeerM e $ withDownload env0 do
                          case r of
                            Left{} -> do
                              -- liftIO $ atomically $ modifyTVar downFail succ
                              failedDownload p h
                              atomically $ modifyTVar downFail succ
                              debug $ "DOWNLOAD FAILED!" <+> pretty p <+> pretty h
                              -- addDownload Nothing h

                            Right{} -> do
                              deleteBlockFromQ h
                              liftIO $ atomically do
                                writeTVar  downFail 0
                                modifyTVar downBlk succ

                              debug $ "DOWNLOAD SUCCEED" <+> pretty p <+> pretty h

                        next PIdle


          atomically $ modifyTVar (_dPeerInbox state) (HashMap.insert peer (q, ass))


  where
      printDownloadStats = do
        pause @'Seconds 5 -- FIXME: put to defaults
                          --        we need to show download stats

        q <- asks (view blockInQ)

        wipNum  <- liftIO (readTVarIO q) <&> HashMap.size
        po <- postponedNum

        notice $ "maintain blocks wip" <+> pretty wipNum
                                       <+> "postponed"
                                       <+> pretty po

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
                     void $ STM.flushTQueue q
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


