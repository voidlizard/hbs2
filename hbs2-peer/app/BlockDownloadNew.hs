{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language UndecidableInstances #-}
{-# Language AllowAmbiguousTypes #-}
{-# Language ImplicitParams #-}
{-# Language RecordWildCards #-}
module BlockDownloadNew where

import HBS2.Prelude.Plated
import HBS2.OrDie
import HBS2.Data.Detect
import HBS2.Hash
import HBS2.Defaults
import HBS2.Events
import HBS2.Net.Proto.Service
import HBS2.Net.Proto.Sessions
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

import PeerTypes
import PeerInfo

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
import Data.Either
import Data.Maybe
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy (ByteString)
import Data.Vector qualified as V
import Data.Vector ((!?))
import Data.ByteString qualified as BS
import Data.List qualified as L
import Data.Coerce
import Data.Kind
import Numeric
import UnliftIO
import Control.Concurrent.STM.TSem (TSem)
import Control.Concurrent.STM.TSem qualified as TSem
import UnliftIO.Concurrent
import UnliftIO.STM
import Lens.Micro.Platform
import System.Random
import System.Random.Shuffle (shuffleM,shuffle')

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

      -- debug $ "FOUND CACHED VALUE" <+> pretty h <+> pretty s

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


class Monad m => IsBurstMachine a m  where
  getCurrentBurst :: a -> m Int
  burstMachineAddErrors ::  a -> Int -> m ()
  burstMachineReset :: a -> m ()
  runBurstMachine ::  a -> m ()

data BurstMachine =
  BurstMachine
  { _buTimeout  :: Double
  , _buStart    :: Double
  , _buBurstMax :: Int
  , _buStepUp   :: Double
  , _buStepDown :: Double
  , _buCurrent  :: TVar Double
  , _buMaxReal  :: TVar Double
  , _buErrors   :: TVar Int
  }

data ConstBurstMachine = ConstBurstMachine Int

data AnyBurstMachine (m :: Type -> Type) = forall a . IsBurstMachine a m => AnyBurstMachine a

instance MonadIO m => IsBurstMachine (AnyBurstMachine m) m where
  getCurrentBurst (AnyBurstMachine a) = getCurrentBurst a
  burstMachineAddErrors (AnyBurstMachine a) = burstMachineAddErrors a
  burstMachineReset (AnyBurstMachine a) = burstMachineReset a
  runBurstMachine (AnyBurstMachine a) = runBurstMachine a

newBurstMachine :: MonadUnliftIO m
                => Double      -- ^ timeout
                -> Int         -- ^ max burst
                -> Maybe Int   -- ^ start burst
                -> Double      -- ^ step up
                -> Double      -- ^ step down
                -> m BurstMachine

newBurstMachine t0 buMax buStart up' down' = do
  BurstMachine t0 bu0 buMax up down
    <$> newTVarIO bu0
    <*> newTVarIO bu0
    <*> newTVarIO 0

  where
    bu0 = realToFrac $ fromMaybe (max 2 (buMax `div` 2)) buStart
    down = min 0.85 down'
    up   = min 0.5 up'

instance MonadUnliftIO m => IsBurstMachine BurstMachine m where
  getCurrentBurst BurstMachine{..} = readTVarIO _buCurrent <&> round

  burstMachineAddErrors BurstMachine{..} n =
    atomically $ modifyTVar _buErrors (+n)

  burstMachineReset BurstMachine{..} = do
    atomically do
        bmr <- readTVar _buMaxReal
        writeTVar _buCurrent (min bmr _buStart)
        writeTVar _buErrors 0

  runBurstMachine BurstMachine{..} = do


    bu0  <- readTVarIO _buCurrent <&> realToFrac
    let buMax = realToFrac _buBurstMax
    let down  = _buStepDown
    let up    = _buStepUp

    _dEdT <- newTVarIO 0.00

    _rates <- newTVarIO (mempty :: Map Double Double)

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
                      nrates <- readTVar _rates <&> drop 2 . Map.toList
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

instance MonadIO m => IsBurstMachine ConstBurstMachine m where
  getCurrentBurst (ConstBurstMachine n) = pure n
  burstMachineAddErrors _ _ = none
  burstMachineReset _ = none
  runBurstMachine _ = forever $ pause @'Seconds 300

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

  bm <- newBurstMachine 10 256 (Just bu0) 0.10 0.35

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
          w  <- lift $ downloadFromPeer bu cache env (coerce h) peer
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


downloadFromPeer :: forall e  cache m . ( e ~ L4Proto
                                        , MonadUnliftIO m
                                        , BlockSizeCache e cache
                                        )
                 => Int
                 -> cache
                 -> PeerEnv e
                 -> Hash HbSync
                 -> Peer e
                 -> m (Either (DownloadError e) ByteString)

downloadFromPeer bu cache env h peer = liftIO $ withPeerM env do

  pd@PeerData{..} <- find (KnownPeerKey peer) id
                   >>= orThrow (UnknownPeerError peer)

  pinfo <- find (PeerInfoKey peer) id
             >>= orThrow (UnknownPeerError peer)

  rtt <- liftIO $ medianPeerRTT pinfo
             <&> fmap ((*1) . realToFrac)
             <&> fromMaybe 1000
             <&> (/1e6)

  let waity = 20 * rtt

  sto <- getStorage

  let chunkSize = defChunkSize

  flip runContT pure $ callCC \exit -> do

    size <- lift (findBlockSize @e cache _peerSignKey h)
             >>= maybe (queryBlockSize exit) pure

    coo <- genCookie (peer,h)
    let key = DownloadSessionKey (peer, coo)
    down@BlockDownload{..} <- newBlockDownload h
    let new =   set sBlockChunkSize chunkSize
              . set sBlockSize (fromIntegral size)
                $ down

    lift $ update @e new key id

    let offsets = calcChunks size (fromIntegral chunkSize)  :: [(Offset, Size)]

    let chunkNums = [ 0 .. pred (length offsets) ]

    let bursts = calcBursts bu chunkNums

    _wx <- newTVarIO 1000

    callCC $ \exit2 -> do

      for_ bursts $ \(i,chunkN) -> do

        let parts = [i .. i + chunkN-1]

        wx <- readTVarIO _wx

        flip fix 0 \again n -> do

          let req = BlockChunks @e coo (BlockGetChunks h chunkSize (fromIntegral i) (fromIntegral chunkN))

          lift $ request peer req

          _num <- atomically do
                   readTVar _sBlockChunks2 <&> IntMap.size >>= newTVar

          let w0 = 2.0 :: Timeout 'MilliSeconds

          let watchdog = flip fix 0 \next x -> do
               r <- race (pause @'MilliSeconds wx) do
                      atomically do
                        y <- readTVar _num
                        if x == y then retry else pure y
               either dontHandle next r

          t0 <- getTimeCoarse

          r <- liftIO $ race (pause w0 >> watchdog) do
                atomically do
                  pieces <- readTVar _sBlockChunks2
                  writeTVar _num ( IntMap.size pieces )
                  let done =  and [ IntMap.member j pieces | j <- parts ]
                  unless done retry

          t1 <- getTimeCoarse


          case r of

            Right{} -> do
              atomically do
                when (isRight r) do
                  let nano = toNanoSeconds $ TimeoutTS (t1 - t0)
                  let wx1 = 100 * realToFrac nano / 1e6 -- millis
                  writeTVar _wx wx1

            Left{} -> do
              if n < 2 then do
                w <- readTVarIO _wx
                pieces <- readTVarIO _sBlockChunks2
                let missed  =  IntMap.difference pieces (IntMap.fromList [ (j,()) | j <- parts ] )

                debug $ red "Retry" <+> pretty i
                                    <+> pretty w
                                    <+> pretty (length missed)
                                    <+> pretty h
                                    <+> pretty peer
                again (succ n)
              else do
                exit2 (Left $ DownloadStuckError (HashRef h) peer)

      blk <- readTVarIO _sBlockChunks2
      let rs = LBS.concat $ IntMap.elems blk

      -- ha <- putBlock sto rs
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


newtype KnownSize = KnownSize Integer

instance BlockSizeCache  e KnownSize where
  cacheBlockSize _ y_ _ _ = pure ()
  findBlockSize (KnownSize s) _ _ = pure (Just s)


-- | Download control block
data DCB =
  DCB
  { dcbStart      :: !TimeSpec
  , dcbParent     :: !(Maybe HashRef)
  , dcbBusy       :: !(TVar Int)
  , dcbDownloaded :: !(TVar Bool)
  }

newDcbSTM :: TimeSpec -> Maybe HashRef -> STM DCB
newDcbSTM ts parent = DCB ts parent <$> newTVar 0 <*> newTVar False

data PSt =
    PChoose
  | PInit         HashRef DCB
  | PQuerySize    HashRef DCB
  | PFetchBlock   HashRef DCB Integer
  | PReleaseBlock HashRef DCB Bool

downloadDispatcher :: forall e m . ( e ~ L4Proto
                                   , MonadUnliftIO m
                                   )
                 => SomeBrains e
                 -> PeerEnv e
                 -> m ()
downloadDispatcher brains env = flip runContT pure do

  pts   <- newTVarIO ( mempty :: HashMap (Peer e) (Async (), PeerNonce) )

  wip       <- newTVarIO ( mempty :: HashMap HashRef DCB )
  parseQ    <- newTQueueIO

  let
    onBlockSTM :: HashRef -> STM ()
    onBlockSTM = writeTQueue parseQ

  void $ ContT $ withAsync $ manageThreads onBlockSTM wip pts

  sto <- withPeerM env getStorage

  liftIO $ withPeerM env do
    subscribe @e DownloadReqKey $ \(DownloadReqData h) -> do
      here <- hasBlock sto h <&> isJust
      unless here do
        now <- getTimeCoarse
        debug $ green "New download request" <+> pretty h
        atomically do
          already <- readTVar wip <&> HM.member (HashRef h)
          dcb <- newDcbSTM now mzero
          let w = 1.0 -- realToFrac now
          unless already do
            modifyTVar wip  (HM.insert (HashRef h) dcb)

  ContT $ withAsync $ forever $ (>> pause @'Seconds 30) do
    debug "Sweep blocks"
    atomically do
      total <- readTVar wip <&> HM.toList

      alive <- for total $ \e@(h,DCB{..}) -> do
        down <- readTVar dcbDownloaded
        if down then
          pure Nothing
        else
          pure (Just e)

      writeTVar wip (HM.fromList (catMaybes alive))


  ContT $ withAsync $ forever do
    what <- atomically $ readTQueue parseQ
    missed <- findMissedBlocks sto what
    now <- getTimeCoarse
    for_ missed $ \hi -> do
      atomically do
        dcb <- newDcbSTM now (Just what)
        let w = realToFrac now
        already <- readTVar wip <&> HM.member hi
        unless already do
          modifyTVar wip (HM.insert hi dcb)

  forever $ (>> pause @'Seconds 10) do
    sw0  <- readTVarIO wip <&> HM.size
    debug $ yellow $ "wip0"   <+> pretty sw0

  where

    manageThreads onBlock wip pts = do
      _pnum <- newTVarIO 1

      _psem <- newTVarIO ( mempty :: HashMap PeerNonce TSem )

      forever $ (>> pause @'Seconds 10) $ withPeerM env do
        debug "MANAGE THREADS"

        peers <- HM.fromList <$> do
                  pips <- getKnownPeers @e <&> HS.fromList
                  S.toList_ $ for_ pips $ \p -> do
                     mpd <- lift $ find  (KnownPeerKey p) id
                     maybe1 mpd (pure ()) $
                        \PeerData{..} -> S.yield (p, _peerOwnNonce)


        for_ (HM.toList peers) $ \(p,nonce) -> do
          here <- readTVarIO pts <&> HM.member p

          (i,sem) <- atomically do
                       j <- stateTVar _pnum (\i -> (i, succ i))
                       psem <- readTVar _psem

                       ssem <- case HM.lookup nonce psem of
                         Just s -> pure s
                         Nothing -> do
                          -- TODO: semaphore-hardcode
                          new <- TSem.newTSem 50
                          modifyTVar _psem (HM.insert nonce new)
                          pure new

                       pure (j,ssem)

          unless here do
            a <- async (peerThread pts sem onBlock p wip)
            atomically $ modifyTVar pts (HM.insert p (a,nonce))

        loosers <- atomically do
          xs <- readTVar pts <&> HM.toList

          writeTVar pts mempty

          loo <- newTQueue

          for_ xs $ \e@(h,v@(a,_)) -> do
             running <- isNothing <$> pollSTM a
             let here = HM.member h peers
             if running && here then do
               modifyTVar pts (HM.insert h v)
             else
               writeTQueue loo e

          flushTQueue loo

        for_ loosers $ \(p, (a, nonce)) -> do
          cancel a
          atomically do
            modifyTVar _psem (HM.delete nonce)

    peerThread pts sem onBlock p wip = flip runContT pure do

      btimes <- newTVarIO ( mempty :: [Double] )

      _errors <- newTVarIO 0
      _avg <- newTVarIO 600
      _blknum <- newTVarIO 0

      _sizeCache <- newTVarIO ( mempty :: HashMap HashRef (Maybe Integer) )

      bm <- liftIO do
              case _sockType p of
                TCP -> AnyBurstMachine @IO <$> pure (ConstBurstMachine 256) -- newBurstMachine 60 256 (Just 256) 0.20 0.10
                UDP -> AnyBurstMachine @IO <$> newBurstMachine 10 256 (Just 128) 0.05 0.25

      void $ ContT $ bracket none $ const do
        debug $ "Cancelling thread for" <+> pretty p

      debug $ yellow "Started thread for" <+> pretty p

      ContT $ withAsync $ forever do

        b0 <- readTVarIO _blknum
        pause @'Seconds 60
        atomically $ writeTVar _errors 0
        b1 <- readTVarIO _blknum

        when (b0 == b1) do
          debug $ blue "Reset burst machine" <+> pretty p
          liftIO $ burstMachineReset bm

        s <- readTVarIO wip <&> HM.size

        when (s == 0) do
          atomically $ writeTVar _sizeCache mempty

      tsweep <- ContT $ withAsync do
        let hashes = readTVarIO _sizeCache <&> fmap (,60) . HM.keys
        polling (Polling 1 10) hashes $ \h -> do
          atomically do
            here <- readTVar wip <&> HM.member h
            unless here do
              modifyTVar _sizeCache (HM.delete h)

      bmt <- ContT $ withAsync $ liftIO $ runBurstMachine bm

      tstat <- ContT $ withAsync $ forever $ (>> pause @'Seconds 5) do
                 tss <- readTVarIO btimes
                 unless (L.null tss) do
                  let avg = sum tss / realToFrac (L.length tss)
                  atomically do
                    writeTVar _avg avg

      tpinfo <- ContT $ withAsync $ forever $ (>> pause @'Seconds 30) $ flip runContT pure do
        pinfo' <- lift $ find @e (PeerInfoKey p) id
        PeerInfo{..} <- ContT $ maybe1 pinfo' none

        bu <- liftIO $ getCurrentBurst bm
        atomically do
          erno <- readTVar _errors
          down <- readTVar _blknum
          writeTVar _peerErrorsLast erno
          writeTVar _peerBurst bu
          writeTVar _peerDownloaded down

      rndGen <- liftIO newStdGen >>= newTVarIO

      twork <- ContT $ withAsync $ forever do

        flip fix PChoose $ \go -> \case

          PChoose -> do

            what <- atomically do
               e <- readTVar _errors
               peerNum <- readTVar pts <&> HM.size

               if e > 5 then
                 pure Nothing
               else do
                 -- TSem.waitTSem sem

                 wpsize <- readTVar wip <&> HM.size
                 let trsh = if wpsize < peerNum then max 3 (peerNum `div` 2) else 0

                 blocks <- readTVar wip

                 when (HM.null blocks) retry

                 let todo = V.fromList (HM.toList blocks)
                 let len = V.length todo
                 i <- stateTVar rndGen ( randomR (0, len - 1) )

                 let (h,dcb@DCB{..}) = V.unsafeIndex todo (i `mod` len)

                 busy   <- readTVar dcbBusy
                 down   <- readTVar dcbDownloaded
                 absent <- readTVar _sizeCache <&> (== Just Nothing) . HM.lookup h

                 if busy > trsh || down || absent then
                   retry
                 else do
                   modifyTVar dcbBusy succ
                   pure (Just (h,dcb))

            case what of
              Just (hx, dcb) -> go (PInit hx dcb)
              Nothing -> do
                erno <- readTVarIO _errors
                if erno > 50 then do
                  pause @'Seconds 60
                else do
                  pause @'Seconds 0.5
                go PChoose

          PInit hx dcb -> do

            trace $ yellow "Block choosen" <+> pretty p <+> pretty hx

            hereSize <- readTVarIO _sizeCache <&> HM.lookup hx

            case hereSize of
              Just (Just size) -> do
                go (PFetchBlock hx dcb size)

              Just Nothing -> do
                debug $ blue "Release block" <+> pretty p <+> pretty hx
                go (PReleaseBlock hx dcb False)

              Nothing -> do
                trace $ blue "Query size" <+> pretty p <+> pretty hx
                go (PQuerySize hx dcb)

          PQuerySize hx dcb -> do
            s <- queryBlockSizeFromPeer brains env (coerce hx) p
            case s of
              Right (Just size) -> do
                trace $ green "HAS BLOCK" <+> pretty p <+> pretty hx <+> pretty size
                atomically $ modifyTVar _sizeCache (HM.insert hx (Just size))
                go (PFetchBlock hx dcb size)

              Right Nothing -> do
                trace $ red "HAS NO BLOCK" <+> pretty p <+> pretty hx
                atomically $ modifyTVar _sizeCache (HM.insert hx Nothing)
                go (PReleaseBlock hx dcb False)

              Left{} -> do
                atomically $ modifyTVar _errors succ
                go (PReleaseBlock hx dcb False)

          PFetchBlock hx dcb size -> do

            bu <- liftIO $ getCurrentBurst bm

            withPeerM env $ do
              request p (GetBlockSize @e (coerce hx))

            t0 <- getTimeCoarse
            r <- downloadFromPeer bu (KnownSize size) env (coerce hx) p
            t1 <- getTimeCoarse

            case r of
              Right bs -> do

                let dtsec = realToFrac (toNanoSeconds (TimeoutTS (t1 - t0))) / 1e9

                avg <- readTVarIO _avg

                when (dtsec > avg) do
                  liftIO $ burstMachineAddErrors bm 1

                atomically do
                  modifyTVar  btimes ( take 100 . (dtsec :) )
                  writeTVar (dcbDownloaded dcb) True
                  modifyTVar _blknum succ
                  onBlock hx

                go (PReleaseBlock hx dcb True)

              Left e -> do
                liftIO $ burstMachineAddErrors bm 1
                atomically $ modifyTVar _errors succ
                debug $ red "BLOCK DOWNLOAD FUCKED" <+> pretty p <+> pretty hx <+> viaShow e

                go (PReleaseBlock hx dcb False)

          PReleaseBlock hx dcb done -> do
            atomically do
              -- TSem.signalTSem sem
              if not done  then do
                modifyTVar (dcbBusy dcb) pred
              else do
                modifyTVar wip (HM.delete hx)
            go PChoose

      bs <- ContT $ withAsync $ forever do
        pause @'Seconds 10
        debug $ yellow "I'm thread" <+> pretty p

      void $ waitAnyCatchCancel [bmt,bs,twork,tstat,tsweep,tpinfo]

