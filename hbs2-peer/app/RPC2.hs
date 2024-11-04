{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language UndecidableInstances #-}
{-# Language AllowAmbiguousTypes #-}
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
import HBS2.Hash
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
import HBS2.Storage
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
import Data.IntMap qualified as IntMap
import Data.IntMap (IntMap)
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
import Lens.Micro.Platform
import Streaming.Prelude qualified as S


data DownloadError e =
   DownloadStuckError HashRef (Peer e)
 | StorageError
 | UnknownPeerError   (Peer e)
 | PeerMissBlockError HashRef (Peer e)
 | PeerBlockHashMismatch (Peer e)
 | PeerRequestTimeout (Peer e)
  deriving stock (Generic,Typeable)

instance Pretty (Peer e) => Show (DownloadError e) where
  show (DownloadStuckError h p)  = show $ parens $ "DownloadStuck" <+> pretty h <+> pretty p
  show (UnknownPeerError p)  = show $ parens $ "UnknownPeerError" <+> pretty p
  show (PeerMissBlockError h p)  = show $ parens $ "PeerMissBlockError" <+> pretty h <+> pretty p
  show (PeerRequestTimeout p)  = show $ parens $ "PeerRequestTimeout" <+> pretty p
  show (PeerBlockHashMismatch p)  = show $ parens $ "PeerBlockHashMismatch" <+> pretty p
  show StorageError  = show "StorageError"

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

    PeerData{..} <- find (KnownPeerKey peer) id
                     >>= orThrow (UnknownPeerError peer)

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

downloadFromPeer :: forall e t cache m . ( e ~ L4Proto
                                         , MonadUnliftIO m
                                         , IsTimeout t
                                         , BlockSizeCache e cache
                                         )
                 => Timeout t
                 -> cache
                 -> PeerEnv e
                 -> Hash HbSync
                 -> Peer e
                 -> m (Either (DownloadError e) ByteString)

downloadFromPeer t cache env h peer = liftIO $ withPeerM env do

  pd@PeerData{..} <- find (KnownPeerKey peer) id
                   >>= orThrow (UnknownPeerError peer)

  sto <- liftIO $ withPeerM env $ getStorage

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

    (what, rs) <- liftIO $ withPeerM env do
      update @e new key id

      debug $ "FUCKIN WAIT FOR CHUNKS!" <+> pretty h

      request peer (BlockChunks @e coo (BlockGetAllChunks h chunkSize))

      let total = L.length $ calcChunks size (fromIntegral chunkSize)

      blk <- atomically do
        wtf <- readTVar _sBlockChunks2
        unless (IntMap.size wtf == total) retry
        pure wtf

      atomically $ flushTQueue chuQ

      let rs = LBS.concat $ IntMap.elems blk

      ha <- enqueueBlock sto rs

      expire @e key

      pure (ha, rs)

    case what of
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


        entry $ bindMatch "query-block-from-peer" \case
          [ HashLike blk, StringLike addr ] -> flip runContT pure $ callCC \exit -> do

            peer' <- liftIO $ try @_ @SomeException do
                      let pa = fromString @(PeerAddr L4Proto) addr
                      fromPeerAddr pa

            peer <- either (const $ exit (mkSym "error:invalid-address")) pure peer'

            what <- lift $ downloadFromPeer defChunkWaitMax () rpcPeerEnv (coerce blk) peer

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

            sz <- lift $ queryBlockSizeFromPeer @e () rpcPeerEnv  (coerce blk) peer

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

