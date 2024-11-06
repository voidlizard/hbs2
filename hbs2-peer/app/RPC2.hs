{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language UndecidableInstances #-}
{-# Language AllowAmbiguousTypes #-}
{-# LANGUAGE ImplicitParams #-}
module RPC2
  ( module RPC2.Peer
  , module RPC2.RefLog
  , module RPC2.RefChan
  , module RPC2.LWWRef
  , HandleMethod(..)
  -- , module RPC2.Mailbox
  ) where


import HBS2.Prelude.Plated
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
import HBS2.Clock
import HBS2.Net.Auth.Schema

import HBS2.Peer.RPC.Internal.Types
import HBS2.Peer.RPC.API.Peer

import HBS2.Net.Messaging.TCP

import Data.Config.Suckless.Script

import RPC2.Peer
import RPC2.RefLog
import RPC2.RefChan
import RPC2.LWWRef
import RPC2.Mailbox()

import PeerTypes
import PeerInfo

import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Cont
import Control.Concurrent.STM (flushTQueue,retry)
import Data.Map qualified as Map
import Data.Map (Map)
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
          writeTVar _rates mempty
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
                    nrates <- readTVar _rates <&> drop 10 . Map.toList
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

        t0 <- getTimeCoarse

        let watchdog = fix \next -> do
             wx <- readTVarIO _wx <&> realToFrac
             -- debug $ "WATCHDOG" <+> pretty wx <+> pretty waity
             r <- race (pause @'MilliSeconds waity) do
                    void $ atomically $ readTQueue chuQ
             either (const none) (const next) r

        r <- liftIO $ race watchdog do

          atomically do
            pieces <- readTVar _sBlockChunks2
            let done  = and [ IntMap.member j pieces | j <- [i .. i + chunkN-1] ]
            unless done retry

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

instance (e ~ L4Proto, MonadUnliftIO m, HasRpcContext PeerAPI RPC2Context m) => HandleMethod m RpcRunScript where
  handleMethod top = do

    co <- getRpcContext @PeerAPI

    let cli = parseTop top & fromRight mempty

    r <- try @_ @SomeException (run (dict co) cli)

    either (pure . Text.pack . show) (pure . Text.pack . show . pretty) r

   where

    dict RPC2Context{..} = makeDict @_ @m do
        entry $ bindMatch "hey" $ const do
          pure $ mkSym @C "hey"

        entry $ bindMatch "tcp:peer:kick" $ \case
          [ StringLike addr ] -> flip runContT pure $ callCC \exit -> do

            peer' <- liftIO $ try @_ @SomeException do
                      let pa = fromString @(PeerAddr L4Proto) addr
                      fromPeerAddr pa

            peer <- either (const $ exit (mkSym "error:invalid-address")) pure peer'

            mess <- ContT $ maybe1 rpcTCP (pure nil)

            tcpPeerKick mess peer

            liftIO $ withPeerM rpcPeerEnv do
              pl <- getPeerLocator @e
              delPeers pl [peer]
              expire (PeerInfoKey peer)
              expire (KnownPeerKey peer)

            pure $ mkList [mkSym "kicked", mkSym (show $ pretty peer) ]

          _ -> pure nil

        entry $ bindMatch "query-block-from-peer:rec" $ \syn -> do

          flip runContT pure $ callCC \exit -> do

            (blk,addr,bu0) <- case syn of
                    [ HashLike blk, StringLike addr ] -> pure (blk, addr, 4)
                    [ HashLike blk, StringLike addr, LitIntVal x ] -> pure (blk, addr, fromIntegral x)

                    _ -> exit nil

            callCC \exit2 -> do

              peer' <- liftIO $ try @_ @SomeException do
                        let pa = fromString @(PeerAddr L4Proto) addr
                        fromPeerAddr pa

              peer <- either (const $ exit2 (mkSym "error:invalid-address")) pure peer'

              what <- lift $ downloadFromPeerRec defChunkWaitMax bu0 rpcBrains rpcPeerEnv (coerce blk) peer

              case what of
                Left e   -> pure $ mkList @C [ mkSym "error" , mkStr (show e) ]
                Right{}  -> pure $ mkList @C [ mkSym "okay" ]

        entry $ bindMatch "query-block-from-peer" \case
          ( HashLike blk :StringLike addr : opts) -> flip runContT pure $ callCC \exit -> do

            peer' <- liftIO $ try @_ @SomeException do
                      let pa = fromString @(PeerAddr L4Proto) addr
                      fromPeerAddr pa

            peer <- either (const $ exit (mkSym "error:invalid-address")) pure peer'

            what <- lift $ downloadFromPeer defChunkWaitMax 4 rpcBrains rpcPeerEnv (coerce blk) peer

            case what of
              Left e   -> pure $ mkList @C [ mkSym "error" , mkStr (show e) ]
              Right bs -> pure $ mkList @C [ mkSym "okay", mkInt (LBS.length bs) ]

          _ -> pure nil

        entry $ bindMatch "query-block-size-from-peer" \case
          [ HashLike blk, StringLike addr ] -> flip runContT pure $ callCC \exit -> do

            peer' <- liftIO $ try @_ @SomeException do
                      let pa = fromString @(PeerAddr L4Proto) addr
                      fromPeerAddr pa

            peer <- either (const $ exit (mkSym "error:invalid-address")) pure peer'

            sz <- lift $ queryBlockSizeFromPeer @e rpcBrains rpcPeerEnv  (coerce blk) peer

            case sz of
              Left e         -> pure $ mkList @C [ mkSym "error", mkStr (show e) ]
              Right Nothing  -> pure $ mkSym "no-block"
              Right (Just s) -> pure $ mkList [mkSym "size", mkInt s]

          _ -> pure $ mkSym "error:invalid-args"

        entry $ bindMatch "request-block-size" \case
          [LitScientificVal w,  HashLike blk] -> do

            let h = coerce blk

            liftIO $ withPeerM rpcPeerEnv do

              answ <- newTQueueIO

              forKnownPeers @e $ \p pd -> do

                subscribe @e (BlockSizeEventKey p) $ \case
                  BlockSizeEvent (that, hx, sz) | hx == h -> do
                    atomically $ writeTQueue answ (sz, that, pd)

                  _ -> none

                request p (GetBlockSize @e h)

              pause @'Seconds (realToFrac w)

              r <- atomically do
                     x <- readTQueue answ
                     xs <- flushTQueue answ
                     pure (x:xs)

              rr <- S.toList_ $ for_ r $ \(s,p, PeerData{..}) -> do
                      let psk = AsBase58 _peerSignKey
                      S.yield $ mkList @C [ mkSym "size"
                                          , mkInt s
                                          , mkSym (show $ pretty $ psk)
                                          , mkSym (show $ pretty p)
                                          ]

              pure $ mkList rr

          _ -> do
            pure nil

        entry $ bindMatch "peer-info" $ const do

          now <- getTimeCoarse

          liftIO $ withPeerM rpcPeerEnv do
            pl <- getPeerLocator @e
            pips <- knownPeers @e pl
            npi <- newPeerInfo

            r <- for pips $ \p -> do
              pinfo@PeerInfo{..} <- fetch True npi (PeerInfoKey p) id
              burst     <- readTVarIO _peerBurst
              buM       <- readTVarIO _peerBurstMax
              errors    <- readTVarIO _peerErrorsPerSec
              downFails <- readTVarIO _peerDownloadFail
              downMiss  <- readTVarIO _peerDownloadMiss
              down      <- readTVarIO _peerDownloadedBlk
              rtt       <- medianPeerRTT pinfo <&> fmap realToFrac
              seen      <- readTVarIO _peerLastWatched
              let l = realToFrac (toNanoSecs $ now - seen) / 1e9

              let rttMs = (/1e6) <$> rtt <&> (\x -> showGFloat (Just 2) x "") <&> (<> "ms")
              let ls = showGFloat (Just 2) l "" <> "s"

              mpde <- find (KnownPeerKey p) id
              let pk = maybe1 mpde mempty $ \PeerData{..} -> do
                        [ mkList [ mkSym "key", mkSym (show $ pretty (AsBase58 _peerSignKey)) ] ]

              let peerStaff = mkList @C $
                    pk <>
                    [ mkList [ mkSym "addr",        mkSym (show $ pretty p) ]
                    , mkList [ mkSym "seen",        mkSym ls ]
                    , mkList [ mkSym "burst",       mkInt burst ]
                    , mkList [ mkSym "burst-max",   mkInt (fromMaybe 0 buM) ]
                    , mkList [ mkSym "errors",      mkInt (downFails + errors) ]
                    , mkList [ mkSym "downloaded",  mkInt down ]
                    , mkList [ mkSym "miss",        mkInt downMiss ]
                    ]
                    <> maybe1 rttMs mempty (\r -> [ mkList [ mkSym "rtt", mkSym r ] ])

              pure $ mkList @C [mkSym "peer", peerStaff ]

            pure $ mkList r

