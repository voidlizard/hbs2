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
import Data.ByteString qualified as BS
import Data.List qualified as L
import Data.Coerce
import Numeric
import UnliftIO
import UnliftIO.Concurrent
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

      ha <- putBlock sto rs

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

downloadDispatcher :: forall e m . ( e ~ L4Proto
                                   , MonadUnliftIO m
                                   )
                 => SomeBrains e
                 -> PeerEnv e
                 -> m ()
downloadDispatcher brains env = flip runContT pure do

  let t0 = 10.00

  pts <- newTVarIO  ( mempty :: HashMap (Peer e) (Async ()) )

  rq <- newTQueueIO

  let seenLimit = 1000

  seen <- newTVarIO ( HPSQ.empty :: HashPSQ HashRef TimeSpec () )

  blkQ <- newTVarIO ( HPSQ.empty :: HashPSQ HashRef Int NominalDiffTime )

  let sizeCacheLimit = 10000

  sizeCache <- newTVarIO ( HPSQ.empty :: HashPSQ (HashRef,Peer e) TimeSpec (Maybe Integer) )

  sizeQ <- newTVarIO ( HPSQ.empty :: HashPSQ HashRef NominalDiffTime () )

  sto <- withPeerM env getStorage

  work <- newTQueueIO

  ContT $ bracket none $ const do
    readTVarIO pts >>= mapM_ cancel
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

  let shiftPrio = \case
        Nothing -> ((), Nothing)
        Just (p,v) -> ((), Just (succ p, v * 1.10 ))

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

      atomically $ modifyTVar blkQ ( snd . HPSQ.alter shiftPrio h )

  ContT $ withAsync $ forever do
    polling (Polling 1 1) missChk $ \h -> do
      debug $ blue "CHECK MISSED BLOCKS" <+> pretty h

      missed <- findMissedBlocks sto h

      here <- hasBlock sto (coerce h) <&> isJust

      atomically do
        for_ missed $ \hm -> modifyTVar blkQ (HPSQ.insert (coerce hm) 2 t0)
        if here then do
          modifyTVar blkQ (HPSQ.delete (coerce h))
        else
          modifyTVar blkQ ( snd . HPSQ.alter shiftPrio h )

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
          a <- async $ peerDownloadLoop env p
          atomically $ modifyTVar pts (HM.insert p a)

        debug $ "downloadDispatcher: knownPeer" <+> yellow (pretty p)

      dead <- atomically do
        total <- readTVar pts <&> HM.toList

        what <- for total  $ \(p,a) -> do
          let pipExist = HS.member p pips
          stillAlive   <- pollSTM a <&> isNothing

          if pipExist && stillAlive then do
            pure $ Right (p,a)
          else
            pure $ Left (p,a)

        writeTVar pts (HM.fromList (rights what))
        pure $ lefts what

      for_ dead  $ \(p,a) -> do
        cancel a
        debug $ red "terminating peer loop" <+> pretty p

      pause @'Seconds 5

    peerDownloadLoop env p = flip runContT pure do

      sto <- withPeerM env getStorage

      bm <- liftIO $ newBurstMachine 0.5 256 (Just 50) 0.05 0.10

      -- let blokz = atomically do
      --       d <- readTVar down <&> fromMaybe mempty . HM.lookup p
      --       pure $ HM.toList d

      -- void $ ContT $ withAsync $ polling (Polling 1 1) blokz $ \h0 -> do
      --   debug $ "POLL BLOCK DOWNLOAD" <+> pretty h0

      forever do
        pause @'Seconds 10
        debug $ yellow "Peer download loop" <+> pretty p


