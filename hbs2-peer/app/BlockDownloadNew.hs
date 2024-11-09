{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language UndecidableInstances #-}
{-# Language AllowAmbiguousTypes #-}
{-# Language ImplicitParams #-}
module BlockDownloadNew where

import HBS2.Prelude.Plated
import HBS2.Clock
import HBS2.OrDie
import HBS2.Data.Detect
import HBS2.Hash
import HBS2.Merkle
import HBS2.Defaults
import HBS2.Events
import HBS2.Net.Proto.Service
import HBS2.Net.Proto.Sessions
import HBS2.Data.Bundle
import HBS2.Data.Types.SignedBox

import HBS2.Base58
import HBS2.Data.Types.Peer
import HBS2.Data.Types.Refs
import HBS2.Actors.Peer
import HBS2.Peer.Proto.Peer
import HBS2.Peer.Proto.BlockInfo
import HBS2.Peer.Proto.BlockChunks
import HBS2.Peer.Brains
import HBS2.Storage
import HBS2.Storage.Operations.Missed
import HBS2.Misc.PrettyStuff
import HBS2.Clock
import HBS2.Net.Auth.Schema

import HBS2.Peer.RPC.Internal.Types
import HBS2.Peer.RPC.API.Peer

import HBS2.Net.Messaging.TCP

import PeerTypes
import PeerInfo

import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Cont
import Control.Concurrent.STM (flushTQueue,retry)
import Data.Map qualified as Map
import Data.Map (Map)
import Data.HashPSQ qualified as HPSQ
import Data.HashPSQ ( HashPSQ(..) )
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.IntMap qualified as IntMap
import Data.IntMap (IntMap)
import Data.List.Split qualified as Split
import Data.Text qualified as Text
import Data.Either
import Data.Maybe
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy (ByteString)
import Data.Vector qualified as V
import Data.ByteString qualified as BS
import Data.List qualified as L
import Data.Coerce
import Numeric
import UnliftIO
import Control.Concurrent.STM qualified as STM
import UnliftIO.Concurrent
import System.Random
import Lens.Micro.Platform
import Streaming.Prelude qualified as S



data DownloadError e =
   DownloadStuckError HashRef (Peer e)
 | StorageError
 | UnknownPeerError   (Peer e)
 | InternalError Int
 | PeerMissBlockError HashRef (Peer e)
 | PeerBlockHashMismatch (Peer e)
 | PeerRequestTimeout (Peer e)
 | Incomplete HashRef
  deriving stock (Generic,Typeable)

instance Pretty (Peer e) => Show (DownloadError e) where
  show (DownloadStuckError h p)  = show $ parens $ "DownloadStuck" <+> pretty h <+> pretty p
  show (UnknownPeerError p)  = show $ parens $ "UnknownPeerError" <+> pretty p
  show (PeerMissBlockError h p)  = show $ parens $ "PeerMissBlockError" <+> pretty h <+> pretty p
  show (PeerRequestTimeout p)  = show $ parens $ "PeerRequestTimeout" <+> pretty p
  show (PeerBlockHashMismatch p)  = show $ parens $ "PeerBlockHashMismatch" <+> pretty p
  show StorageError  = show "StorageError"
  show (InternalError n) = show $ parens "InternalError" <+> pretty n
  show (Incomplete h) = show $ parens "Incomplete" <+> pretty h

instance (Typeable e, Pretty (Peer e)) => Exception (DownloadError e)


class BlockSizeCache e cache where

  cacheBlockSize :: forall m . MonadUnliftIO m
                 => cache
                 -> PubKey 'Sign (Encryption e)
                 -> Hash HbSync
                 -> Integer
                 -> m ()

  findBlockSize :: forall m . MonadUnliftIO m
                => cache
                -> PubKey 'Sign (Encryption e)
                -> Hash HbSync
                -> m (Maybe Integer)

instance BlockSizeCache e () where
  cacheBlockSize _ _ _ _ = pure ()
  findBlockSize _ _ _ = pure Nothing

instance BlockSizeCache e (SomeBrains e) where
  cacheBlockSize = brainsCacheBlockSize @e
  findBlockSize = brainsFindBlockSize @e

queryBlockSizeFromPeer :: forall e cache m . ( e ~ L4Proto
                                             , MonadUnliftIO m
                                             , BlockSizeCache e cache
                                             )
                 => cache
                 -> PeerEnv e
                 -> Hash HbSync
                 -> Peer e
                 -> m (Either (DownloadError e) (Maybe Integer))

queryBlockSizeFromPeer cache e h peer = do

  what <- try @_ @(DownloadError e) $ liftIO $ withPeerM e do

    flip runContT pure $ callCC \exit -> do

      PeerData{..} <- lift $ find (KnownPeerKey peer) id
                       >>= orThrow (UnknownPeerError peer)

      s <- lift $ findBlockSize @e cache _peerSignKey h

      debug $ "FOUND CACHED VALUE" <+> pretty h <+> pretty s

      maybe none (exit . Just) s

      lift do

        sizeQ <- newTQueueIO

        subscribe @e (BlockSizeEventKey peer) $ \case
          BlockSizeEvent (that, hx, sz) | hx == h -> do
            atomically $ writeTQueue sizeQ (Just sz)
            cacheBlockSize @e cache _peerSignKey h sz

          _ -> do
            atomically $ writeTQueue sizeQ Nothing

        request peer (GetBlockSize @e h)

        race ( pause defBlockInfoTimeout ) (atomically $ readTQueue sizeQ )
           >>= orThrow (PeerRequestTimeout peer)

  case what of
    Left{}  -> pure $ Left (PeerRequestTimeout peer)
    Right x -> pure (Right x)


data BurstMachine =
  BurstMachine
  { _buTimeout  :: Double
  , _buBurstMax :: Int
  , _buStepUp   :: Double
  , _buStepDown :: Double
  , _buCurrent  :: TVar Double
  , _buErrors   :: TVar Int
  }

burstMachineAddErrors :: MonadUnliftIO m => BurstMachine -> Int -> m ()
burstMachineAddErrors BurstMachine{..} n =
  atomically $ modifyTVar _buErrors (+n)

newBurstMachine :: MonadUnliftIO m
                => Double      -- ^ timeout
                -> Int         -- ^ max burst
                -> Maybe Int   -- ^ start burst
                -> Double      -- ^ step up
                -> Double      -- ^ step down
                -> m BurstMachine

newBurstMachine t0 buMax buStart up' down' = do
  BurstMachine t0 buMax up down
    <$> newTVarIO bu0
    <*> newTVarIO 0

  where
    bu0 = realToFrac $ fromMaybe (max 2 (buMax `div` 2)) buStart
    down = min 0.85 down'
    up   = min 0.5 up'

getCurrentBurst :: MonadUnliftIO m => BurstMachine -> m Int
getCurrentBurst BurstMachine{..} = readTVarIO _buCurrent <&> round

runBurstMachine :: MonadUnliftIO m
                => BurstMachine
                -> m ()

runBurstMachine BurstMachine{..} = do


  bu0  <- readTVarIO _buCurrent <&> realToFrac
  let buMax = realToFrac _buBurstMax
  let down  = _buStepDown
  let up    = _buStepUp

  _dEdT <- newTVarIO 0.00

  _rates <- newTVarIO (mempty :: Map Double Double)

  _buMaxReal <- newTVarIO buMax

  pause @'Seconds (realToFrac _buTimeout)

  flip runContT pure do

    void $ ContT $ withAsync do
      forever do
        pause @'Seconds (realToFrac _buTimeout * 10)

        atomically do
          e <- headDef bu0 . Map.elems <$> readTVar _rates
          nrates <- readTVar _rates <&> take 100 . Map.toList
          writeTVar _rates (Map.fromList nrates)
          modifyTVar _buMaxReal (max e)

    void $ ContT $ withAsync do
      forever do
        pause @'Seconds 600
        atomically $ writeTVar _buMaxReal buMax


    void $ ContT $ withAsync do
      forever do
        pause @'Seconds (realToFrac _buTimeout * 2.0)
        ddt <- readTVarIO _dEdT

        when (ddt <= 0) do
          atomically do
            buMaxReal <- readTVar _buMaxReal
            current <- readTVar _buCurrent
            let new =  min buMaxReal (current * (1.0 + up))
            writeTVar _buCurrent new

    flip fix 0 $ \next e1 -> do

      let dt = realToFrac _buTimeout

      eNew <- atomically do

        e2      <- readTVar _buErrors
        current <- readTVar _buCurrent

        new <- if e2 > e1 then do
                    let d = max 2.0 (current * (1.0 - down))
                    nrates <- readTVar _rates <&> drop 3 . Map.toList
                    let newFucked = maybe d snd (headMay nrates)
                    writeTVar _rates (Map.fromList nrates)
                    pure newFucked

                  else
                    pure current -- $ min buMaxReal (current * (1.0 + up))

        writeTVar _buErrors 0
        writeTVar _buCurrent new

        let dedt = realToFrac (e2 - e1) / realToFrac dt

        writeTVar _dEdT (realToFrac dedt)

        modifyTVar _rates ( Map.insertWith max dedt current )

        pure e2

      pause @'Seconds dt
      next eNew

data S =
    SInit
  | SFetchQ
  | SFetchPost (Hash HbSync) ByteString
  | SCheckBefore
  | SCheckAfter

-- | downloads block with dependencies recursively
downloadFromPeerRec :: forall e t cache m . ( e ~ L4Proto
                                            , MonadUnliftIO m
                                            , IsTimeout t
                                            , BlockSizeCache e cache
                                            )
                 => Timeout t
                 -> Int
                 -> cache
                 -> PeerEnv e
                 -> Hash HbSync
                 -> Peer e
                 -> m (Either (DownloadError e) ())

downloadFromPeerRec t bu0 cache env h0 peer = do

  sto <- withPeerM env getStorage

  p <- newTQueueIO
  q <- newTQueueIO
  qq <- newTQueueIO
  toq <- newTVarIO ( mempty :: [Int] )

  bm <- newBurstMachine 0.5 256 (Just bu0) 0.05 0.10

  flip runContT pure do

    ContT $ withAsync $ forever do
      join $ atomically (readTQueue p)

    ContT $ withAsync $ forever do
      h <- atomically (readTQueue qq)
      void $ queryBlockSizeFromPeer cache env h peer
      pause @'Seconds 1.5

    ContT $ withAsync $ flip fix 10000000 $ \next m0 -> do
      txs <- readTVarIO toq <&> L.take 1000
      let m1 = fromMaybe m0 $ median txs
      when ( m1 > m0 ) $ burstMachineAddErrors bm 1
      pause @'Seconds 3
      next m1

    ContT $ withAsync $ runBurstMachine bm

    flip fix SInit $ \next -> \case

      SInit -> do
        debug "SInit"
        atomically $ writeTQueue q h0
        next SCheckBefore

      SCheckBefore -> do
        here <- hasBlock sto h0 <&> isJust
        if here then next SCheckAfter else next SFetchQ

      SFetchQ -> do
        debug "SFetchQ"

        done <- atomically do
          pe <- isEmptyTQueue p
          qe <- isEmptyTQueue q
          when (qe && not pe) retry
          -- when (not pe) retry
          pure qe

        if done then
          next SCheckAfter
        else do

          h <- atomically $ readTQueue q
          mbs <- getBlock sto h

          case mbs of
            Just bs -> next (SFetchPost h bs)
            Nothing -> none

          bu <- lift $ getCurrentBurst bm

          t0 <- getTimeCoarse
          w  <- lift $ downloadFromPeer t bu cache env (coerce h) peer
          t1 <- getTimeCoarse
          let dt = toMicroSeconds $ TimeoutTS (t1 - t0)
          atomically $ modifyTVar toq ( dt : )

          case w of
            Right bs -> do
              next (SFetchPost h bs)

            Left e -> do
             lift $ burstMachineAddErrors bm 1
             err $ "DOWNLOAD ERROR" <+> viaShow e
             next SFetchQ

      SFetchPost h bs -> do
        debug $ "SFetchPost" <+> pretty h

        let parse = do
             let refs = extractBlockRefs h bs
             atomically $ mapM_ (writeTQueue q . coerce) refs
             mapM_ (atomically . writeTQueue qq . coerce) refs

        atomically $ writeTQueue p parse

        next SFetchQ

      SCheckAfter -> do
        debug "SCheckAfter"
        missed <- findMissedBlocks sto (HashRef h0)
        mapM_ (atomically . writeTQueue q . coerce) missed
        mapM_ (atomically . writeTQueue qq . coerce) missed
        unless (L.null missed) $ next SFetchQ

    pure $ Right ()


downloadFromPeer :: forall e t cache m . ( e ~ L4Proto
                                         , MonadUnliftIO m
                                         , IsTimeout t
                                         , BlockSizeCache e cache
                                         )
                 => Timeout t
                 -> Int
                 -> cache
                 -> PeerEnv e
                 -> Hash HbSync
                 -> Peer e
                 -> m (Either (DownloadError e) ByteString)

downloadFromPeer t bu cache env h peer = liftIO $ withPeerM env do

  pd@PeerData{..} <- find (KnownPeerKey peer) id
                   >>= orThrow (UnknownPeerError peer)

  pinfo <- find (PeerInfoKey peer) id
             >>= orThrow (UnknownPeerError peer)

  rtt <- liftIO $ medianPeerRTT pinfo
             <&> fmap ((*1) . realToFrac)
             <&> fromMaybe 1000
             <&> (/1e6)

  let waity = 10 * rtt

  sto <- getStorage

  let chunkSize = defChunkSize

  flip runContT pure $ callCC \exit -> do

    size <- lift (findBlockSize @e cache _peerSignKey h)
             >>= maybe (queryBlockSize exit) pure

    coo <- genCookie (peer,h)
    let key = DownloadSessionKey (peer, coo)
    down@BlockDownload{..} <- newBlockDownload h
    let chuQ = _sBlockChunks
    let new =   set sBlockChunkSize chunkSize
              . set sBlockSize (fromIntegral size)
                $ down

    lift $ update @e new key id

    let offsets = calcChunks size (fromIntegral chunkSize)  :: [(Offset, Size)]

    let chunkNums = [ 0 .. pred (length offsets) ]

    let bursts = calcBursts bu chunkNums

    callCC $ \exit2 -> do

      _wx <- newTVarIO waity

      for_ bursts $ \(i,chunkN) -> do

        -- atomically $ flushTQueue chuQ

        let req = BlockChunks @e coo (BlockGetChunks h chunkSize (fromIntegral i) (fromIntegral chunkN))

        lift $ request peer req

        -- pause @'MilliSeconds ( rtt * 0.01 )

        t0 <- getTimeCoarse

        let watchdog = fix \next -> do
             wx <- readTVarIO _wx <&> realToFrac
             -- debug $ "WATCHDOG" <+> pretty wx <+> pretty waity
             r <- race (pause @'MilliSeconds (min wx waity)) do
                    void $ atomically $ readTQueue chuQ
             either (const none) (const next) r

        r <- liftIO $ race watchdog do
            atomically do
              pieces <- readTVar _sBlockChunks2
              let done =  and [ IntMap.member j pieces | j <- [i .. i + chunkN-1] ]
              unless done retry -- $ pause @'MilliSeconds ( 0.25 * rtt ) >> next

        atomically $ flushTQueue chuQ

        t1 <- getTimeCoarse

        atomically do
          wx0 <- readTVar _wx
          let wx1 = realToFrac (t1 - t0) * 100 / 1e6 -- millis
          writeTVar _wx wx1

        case r of
          Left{} -> exit2 (Left $ DownloadStuckError (HashRef h) peer)
          _ -> pure ()


      blk <- readTVarIO _sBlockChunks2
      let rs = LBS.concat $ IntMap.elems blk

      ha <- enqueueBlock sto rs

      -- let ha = Just $ hashObject @HbSync rs

      lift $ expire @e key

      case ha of
        Nothing -> pure $ Left StorageError

        Just h1 | h1 == h -> do
          pure $ Right rs

        Just h1 -> do
          delBlock sto h1
          pure $ Left (PeerBlockHashMismatch peer)

  where

    queryBlockSize exit = do
      what <- lift $ queryBlockSizeFromPeer cache env h peer
      case what of
        Left{}  -> exit (Left (PeerRequestTimeout peer))
        Right Nothing -> exit (Left (PeerMissBlockError (HashRef h) peer))
        Right (Just s) -> pure s



data S1 =
    S1Init
  | S1QuerySize (Hash HbSync)
  | S1CheckMissed (Hash HbSync)


data S2 =
    S2Init (Hash HbSync)
  | S2CheckBlock1 (Hash HbSync) ByteString
  | S2CheckBlock2 (Hash HbSync)
  | S2FetchBlock (Hash HbSync)
  | S2Exit

newtype KnownSize = KnownSize Integer

instance BlockSizeCache  e KnownSize where
  cacheBlockSize _ _ _ _ = pure ()
  findBlockSize (KnownSize s) _ _ = pure (Just s)


downloadDispatcher2 :: forall e m . ( e ~ L4Proto
                                   , MonadUnliftIO m
                                   )
                 => SomeBrains e
                 -> PeerEnv e
                 -> m ()

downloadDispatcher2 brains e = do

  let blkInfoLock = 5  :: Timeout 'Seconds
  let blkWaitLock = 60 :: Timeout 'Seconds
  let workloadFactor = 2.5

  sto <- withPeerM e getStorage

  let downT = 16
  let sizeT = 4

  inQ       <- newTVarIO mempty
  sizeRQ    <- newTQueueIO
  checkQ    <- newTQueueIO

  -- inQ    <- withDownload env0 $ asks (view blockInQ)
  -- checkQ <- withDownload env0 $ asks (view blockCheckQ)
  sizeQ  <- newTQueueIO
  fetchQ <- newTQueueIO
  parseQ <- newTQueueIO
  -- sizeRQ <- withDownload env0 $ asks (view blockSizeRecvQ)

  -- FIXME: cleanup-nonce
  nonces <- newTVarIO (mempty :: HashMap (Peer e) PeerNonce)

  -- FIXME: cleanup-busy
  busy   <- newTVarIO (mempty :: HashMap PeerNonce Double)

  rates  <- newTVarIO (mempty :: IntMap.IntMap [(Peer e,PeerNonce)])

  fetchH <- newTVarIO (mempty :: HashSet (Hash HbSync))
  sizes  <- newTVarIO (mempty :: HashMap (Peer e, Hash HbSync) (Maybe Integer, TimeSpec))
  sizeReq <- newTVarIO (mempty :: HashMap (Hash HbSync) TimeSpec)

  seen   <- newTVarIO (mempty :: HashMap (Hash HbSync) Int)

  peerz <- newTVarIO ( mempty :: HashMap (Peer e) BurstMachine)

  defBurst <- liftIO $ newBurstMachine 0.5 256 (Just 50) 0.05 0.10

  liftIO $ withPeerM e do
    subscribe @e DownloadReqKey $ \(DownloadReqData h) -> do
      here <- hasBlock sto h <&> isJust
      unless here do
        atomically $ modifyTVar inQ (HM.insert h ())
        atomically $ writeTQueue sizeQ h

  flip runContT pure do

    -- UPDATE-STATS-LOOP
    void $ ContT $ withAsync $ updateRates e rates nonces

    void $ ContT $ withAsync $ forever do
      pips <- withPeerM e $ getKnownPeers @e <&> HS.fromList

      for_ pips $ \p -> do
        here <- readTVarIO peerz <&> HM.member p

        unless here do
          newBum <- liftIO $ newBurstMachine 0.5 256 (Just 50) 0.05 0.10
          atomically $ modifyTVar peerz (HM.insert p newBum)

      atomically do
        modifyTVar peerz ( HM.filterWithKey (\k _ -> HS.member k pips))

      pause @Seconds 5

    void $ ContT $ withAsync do
      forever do
        pause @'Seconds 120
        atomically do
          q <- readTVar inQ
          let isInQ x = HM.member x q
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

    void $ ContT $ withAsync $ liftIO $ withPeerM e do
      forever do
        blk <-  atomically $ readTQueue parseQ

        blks <- findMissedBlocks sto (HashRef blk)

        for_ blks $ \b -> do
          addDownload @e (Just blk) (fromHashRef b)

          processBlock blk
          atomically $ modifyTVar inQ (HM.delete (coerce blk))

    replicateM_ 1 $ ContT $ withAsync do
      forever do

        -- pause @'Seconds 0.25

        items <-  atomically do
                     peekTQueue sizeRQ >> STM.flushTQueue sizeRQ

        now <- getTimeCoarse

        todo <- atomically do
          w <- for items $ \(p,h,s) -> do
                  modifyTVar sizes (HM.insert (p,h) (s, now))
                  readTVar nonces <&> HM.lookup p >>= \case
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
      let blocks = readTVarIO sizeReq <&> HM.keys <&> fmap (,2)

      polling (Polling 1 1) blocks $ \h -> do
         pips <- readTVarIO nonces <&> HM.keys
         s <- readTVarIO sizes <&> HM.toList

         for_ pips $ \p -> do
          here <- lookupSizeIO sizes p h <&> isRight

          if here then do
            atomically $ modifyTVar sizeReq (HM.delete h)
          else
            withPeerM e $ request p (GetBlockSize @e h)


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
              PeerInfo{..} <- withPeerM e $ fetch True npi (PeerInfoKey p) id

              atomically do
                setBusySTM nonce busy (Just (setFactor 0 (+(-0.01))))
                modifyTVar _peerDownloadMiss succ
                modifyTVar seen (HM.insertWith (+) blk 1)
                modifyTVar sizeReq (HM.delete blk)

              debug $ red "NONE:" <+> pretty p <+> pretty blk
              pure 0

            -- уже спрашивали, ответил
            Right (Just w) -> do

              atomically do
                setBusySTM nonce busy (Just (setFactor 0 (+(-0.01))))
                modifyTVar sizeReq (HM.delete blk)

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
                atomically $ modifyTVar sizeReq (HM.insert blk now)

              pure 0

        if sum answ > 0 then do
          atomically do
            here <- readTVar fetchH <&> HS.member blk
            readTVar seen <&> HM.delete blk
            unless here $
              writeTQueue fetchQ blk

        else do
          howMany <- readTVarIO seen  <&> (fromMaybe 0 . HM.lookup blk)
          pips <- readTVarIO nonces <&> HM.size
          -- FIXME: hardcode
          when (howMany < 10) do
            atomically $ writeTQueue sizeQ blk

    void $ ContT $ withAsync do
      -- FIXME: ban-time-hardcode
      let loosers = readTVarIO seen <&> fmap (,120) . HM.keys
      polling (Polling 1 10) loosers $ \it -> do
        atomically $ writeTQueue checkQ it
        atomically $ modifyTVar seen (HM.delete it)

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
                 round $ trimFactor * (1.75 / (1.0 + fromMaybe 0 (HM.lookup nonce bsy)))

            let w = [ (-(v + w0 + bx nonce), p)
                    | (v, (w0, peers)) <- zip ws r0, p@(_,nonce) <- peers
                    ] & L.sortOn fst & fmap snd

            avail' <- for w $ \(peer,nonce) -> do
                       p <- readTVar busy <&> HM.lookup nonce
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

              bum <- readTVarIO peerz <&> HM.findWithDefault defBurst p
              bu <- liftIO $ getCurrentBurst bum

              r <- lift $ race (pause @'Seconds 60) do
                     downloadFromPeer (TimeoutSec 55) bu (KnownSize s) e (coerce blk) p

              atomically do
                setBusySTM nonce busy (Just (setFactor 0 (const 0)))

              npi <- newPeerInfo
              PeerInfo{..} <- withPeerM e $ fetch True npi (PeerInfoKey p) id

              debug $ green "DOWNLOAD DONE!" <+> pretty p <+> pretty blk <+> pretty s <+> pretty (isRight r)

              atomically $ modifyTVar fetchH (HS.delete blk)

              case r of
                Right block -> do
                  -- mh <- putBlock sto block
                  atomically do
                    modifyTVar _peerDownloaded succ
                    modifyTVar _peerDownloadedBlk succ

                _ -> do
                  debug $ red "DOWNLOAD FAILED / TIMEOUT"

                  liftIO $ burstMachineAddErrors bum 1

                  atomically do
                    modifyTVar _peerDownloadFail succ
                    modifyTVar _peerErrors succ
                    writeTQueue checkQ blk

            _ -> do
                debug $ red "WAIT FOR PEERS TIMEOUT" <+> pretty blk
                atomically $ writeTVar busy mempty

    forever do
      pause @'Seconds 5
      wip <- readTVarIO inQ <&> HM.size
      notice $ yellow "wip" <+> pretty wip

  where

    trimFactor :: Double
    trimFactor = 100

    setFactor d f = \case
      Nothing -> Just d
      Just v  -> Just (g v)
      where
        g y = f y & max 0

    setBusySTM nonce busy = \case
      Nothing -> modifyTVar busy (HM.delete nonce)
      Just fn -> modifyTVar busy (HM.alter fn nonce)

    lookupBusySTM nonce busy =
      readTVar busy <&> fromMaybe 0 . HM.lookup nonce

    lookupSizeSTM sizes p h = do
        readTVar sizes
          <&> HM.lookup (p,h)
          <&> \case
            Nothing           -> Right Nothing
            Just (Just x,_)   -> Right (Just x)
            Just (Nothing,_)  -> Left ()

    lookupSizeIO sizes p h = do
      atomically $ lookupSizeSTM sizes p h

    processBlock :: forall e s m . ( MonadIO m
                                   , MyPeer e
                                   , ForSignedBox s
                                   , s ~ Encryption e
                                   , HasPeerLocator e m
                                   , m ~ PeerM e IO
                                   )
                 => Hash HbSync
                 -> m ()

    processBlock h = do

       sto <- withPeerM e getStorage

       let parent = Just h

       block <- liftIO $ getBlock sto h

       let bt = tryDetect h <$> block

       -- FIXME:  если блок нашёлся, то удаляем его из wip

       -- when (isJust bt) (deleteBlockFromQ h)

       let handleHrr (hrr :: Either (Hash HbSync) [HashRef]) = do
                case hrr of
                  Left hx -> addDownload @e parent hx
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
                        addDownload @e parent blk

       case bt of
         Nothing -> addDownload @e mzero h

         Just (SeqRef (SequentialRef n (AnnotatedHashRef a' b))) -> do
          maybe1 a' none $ \a -> do
            debug $ "GOT AnnotatedHashRef" <+> pretty a
            processBlock (fromHashRef a)

          addDownload @e parent (fromHashRef b)

         Just (AnnRef (AnnotatedHashRef ann hx)) -> do
           maybe1 ann none $ addDownload @e parent . fromHashRef
           addDownload @e parent (fromHashRef hx)

         Just (MerkleAnn ann) -> do
           case _mtaMeta ann of
              NoMetaData -> pure ()
              ShortMetadata {} -> pure ()
              AnnHashRef hx -> addDownload @e parent hx

           case _mtaCrypt ann of
              NullEncryption -> pure ()
              CryptAccessKeyNaClAsymm h -> addDownload @e parent h
              EncryptGroupNaClSymm h _  -> addDownload @e parent h

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
           pure ()

        where
          unboxBundleRef (BundleRefValue box) = unboxSignedBox0 box



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

              atomically $ modifyTVar nonces (HM.insert peer nonce)

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


downloadDispatcher :: forall e m . ( e ~ L4Proto
                                   , MonadUnliftIO m
                                   )
                 => SomeBrains e
                 -> PeerEnv e
                 -> m ()
downloadDispatcher brains env = flip runContT pure do

  let t0 = 10.00

  pts <- newTVarIO  ( mempty :: HashMap (Peer e) (Async (), TQueue (Hash HbSync,Integer)) )

  let seenLimit = 1000

  seen <- newTVarIO ( HPSQ.empty :: HashPSQ HashRef TimeSpec () )

  blkQ <- newTVarIO ( HPSQ.empty :: HashPSQ HashRef Int NominalDiffTime )

  let sizeCacheLimit = 10000

  sizeCache <- newTVarIO ( HPSQ.empty :: HashPSQ (HashRef,Peer e) TimeSpec (Maybe Integer) )

  sto <- withPeerM env getStorage

  work <- newTQueueIO

  ContT $ bracket none $ const do
    readTVarIO pts >>= mapM_ (cancel . fst)
    atomically $ writeTVar pts mempty

  ContT $ withAsync $ forever do
    join $ atomically (readTQueue work)

  ContT $ withAsync $ forever do
    pause @'Seconds 600

    debug $ "CLEANUP SEEN"
    atomically do
      fix \next -> do
        n <- readTVar seen <&> HPSQ.size
        when (n > seenLimit) do
          modifyTVar seen HPSQ.deleteMin
          next

    debug $ "CLEANUP SIZES"
    atomically do
      fix \next -> do
        n <- readTVar sizeCache <&> HPSQ.size
        when (n > sizeCacheLimit) do
          modifyTVar sizeCache HPSQ.deleteMin
          next

  liftIO $ withPeerM env do
    subscribe @e DownloadReqKey $ \(DownloadReqData h) -> do
      now <- getTimeCoarse
      new <- atomically do
                already <- readTVar seen <&> HPSQ.member (HashRef h)
                if already then do
                  pure False
                else do
                  modifyTVar seen ( HPSQ.insert (HashRef h) now () )
                  modifyTVar blkQ ( HPSQ.insert (HashRef h) 1 t0 )
                  pure True
      when new do
        debug $ green "New download request" <+> pretty h

  let missChk = readTVarIO blkQ <&> fmap tr . HPSQ.toList
       where tr (k,_,v) = (k, realToFrac v)

  let shiftPrio t = \case
        Nothing -> ((), Nothing)
        Just (p,v) -> ((), Just (succ p, v * t ))

  ContT $ withAsync $
    polling (Polling 1 1) missChk $ \h -> do
      pips <- readTVarIO pts <&> HM.keys
      forConcurrently_ pips $ \p -> do
        now <- getTimeCoarse
        here <- readTVarIO sizeCache <&> HPSQ.member (h,p)
        hereB <- hasBlock sto (coerce h) <&> isJust
        when (not here && not hereB) do
          size <- queryBlockSizeFromPeer brains env (coerce h) p
          case size of
            Left{} -> pure ()
            Right w -> do
              atomically $ modifyTVar sizeCache ( HPSQ.insert (h,p) now w )
              debug $ green "GOT SIZE" <+> pretty w <+> pretty h <+> pretty p

      -- atomically $ modifyTVar blkQ ( snd . HPSQ.alter (shiftPrio 1.10) h )


  _wip <- newTVarIO ( mempty :: HashMap HashRef () )

  ContT $ withAsync $ do
    let ws = readTVarIO _wip <&> fmap (,10) . HM.keys
    polling (Polling 1 1) ws $ \h -> do
      atomically $ modifyTVar _wip (HM.delete h)

  ContT $ withAsync $ do

    pause @'Seconds 10

    _i <- newTVarIO 0

    flip runContT pure  $ forever do

      blokz <- readTVarIO blkQ <&> fmap (view _1) . HPSQ.toList

      flip runContT pure do

        k <- for blokz $ \what -> callCC \next -> do

          atomically $ modifyTVar _i succ
          i <- readTVarIO _i

          here <- hasBlock sto (coerce what) <&> isJust

          when here $ next 0

          wip <- readTVarIO _wip <&> HM.member what

          when wip $ next 0

          holders <- readTVarIO sizeCache <&> HPSQ.toList

          wip <- readTVarIO _wip

          let jobs = [ (h,p,s)
                     | ((h,p),_,Just s) <- holders
                     , what == h
                     , not (HM.member h wip)
                     ] & V.fromList

          when ( V.null jobs ) $ next 0

          let j = i `mod` V.length jobs
          let (h,p,s) = V.unsafeIndex jobs j
          worker <- readTVarIO pts <&> HM.lookup p

          n <- maybe1 worker (pure 0) $ \(_,q) -> do
                atomically do
                  modifyTVar _wip $ HM.insert what ()
                  writeTQueue q (coerce h,s)
                  pure 1

          debug $ green "FOUND WORKER" <+> pretty p <+> pretty h
          pure n

        when ( sum k == 0 ) do
          debug $ red "NOTHING"
          pause @'Seconds 0.1

  ContT $ withAsync $ forever do
    polling (Polling 1 1) missChk $ \h -> do
      -- debug $ blue "CHECK MISSED BLOCKS" <+> pretty h

      missed <- findMissedBlocks sto h

      here <- hasBlock sto (coerce h) <&> isJust

      atomically do
        for_ missed $ \hm -> modifyTVar blkQ (HPSQ.insert (coerce hm) 2 t0)
        when here do
          modifyTVar blkQ (HPSQ.delete (coerce h))

  ContT $ withAsync (manageThreads pts)
  -- ContT $ withAsync (sizeQueryLoop pts rq down)

  forever do
    pause @'Seconds 10
    size <- atomically $ readTVar blkQ <&> HPSQ.size
    seenSize <- atomically $ readTVar seen <&> HPSQ.size
    sizeCacheSize <- atomically $ readTVar sizeCache <&> HPSQ.size
    debug $ yellow $ "I'm download dispatcher"
    debug $ yellow $ "wip:" <+> pretty size
    debug $ yellow $ "seen:" <+> pretty seenSize
    debug $ yellow $ "sizes:" <+> pretty sizeCacheSize

  where

    manageThreads pts = withPeerM env $ forever do
      pips <- getKnownPeers @e <&> HS.fromList

      for_ pips $ \p -> do
        here <- readTVarIO pts <&> HM.member p
        unless here do
          q <- newTQueueIO
          a <- async $ peerDownloadLoop p q
          atomically $ modifyTVar pts (HM.insert p (a,q))

        debug $ "downloadDispatcher: knownPeer" <+> yellow (pretty p)

      dead <- atomically do
        total <- readTVar pts <&> HM.toList

        what <- for total  $ \(p,a) -> do
          let pipExist = HS.member p pips
          stillAlive   <- pollSTM (fst a) <&> isNothing

          if pipExist && stillAlive then do
            pure $ Right (p,a)
          else
            pure $ Left (p,a)

        writeTVar pts (HM.fromList (rights what))
        pure $ lefts what

      for_ dead  $ \(p,a) -> do
        cancel (fst a)
        debug $ red "terminating peer loop" <+> pretty p

      pause @'Seconds 5

    peerDownloadLoop p q = do
      bm <- liftIO $ newBurstMachine 0.5 256 (Just 50) 0.05 0.10
      forever do
        (h,s) <- atomically $ readTQueue q
        bu <- getCurrentBurst bm
        blk <- downloadFromPeer (TimeoutSec 5) bu (KnownSize s) env (coerce h) p
        case blk of
          Left{} -> pure ()
          Right bs -> liftIO $ withPeerM env do
            let refs = extractBlockRefs h bs
            for_ refs $ \r -> do
              addDownload @e (Just (coerce h)) r


