module HBS2.Git.DashBoard.State.Index.Peer where

import HBS2.Git.DashBoard.Prelude
import HBS2.Git.DashBoard.Types
import HBS2.Git.DashBoard.State
import HBS2.Git.Data.LWWBlock
import HBS2.Git.Data.Tx.Git

import Streaming.Prelude qualified as S

{- HLINT ignore "Functor law" -}

seconds = TimeoutSec

updateIndexFromPeer :: (DashBoardPerks m, MonadReader DashBoardEnv m) => m ()
updateIndexFromPeer = do
  debug "updateIndexFromPeer"

  peer   <- asks _peerAPI
  reflog <- asks _refLogAPI
  lwwAPI <- asks _lwwRefAPI
  sto    <- asks _sto


  polls <- callRpcWaitMay @RpcPollList2 (TimeoutSec 1) peer (Just "lwwref", Nothing)
            <&> join . maybeToList
            <&> fmap (LWWRefKey @HBS2Basic . view _1)

  repos <- S.toList_ $ forM_ polls $ \r -> void $ runMaybeT do

            lwval <- liftIO (callRpcWaitMay @RpcLWWRefGet (seconds 1) lwwAPI r)
                       >>= toMPlus >>= toMPlus

            (lw,blk) <- readLWWBlock sto r >>= toMPlus
            let rk = lwwRefLogPubKey blk

            lift $ S.yield (r,lwval,RefLogKey @'HBS2Basic rk,blk)

  for_ repos $ \(lw,wv,rk,LWWBlockData{..}) -> do

    mhead <- callRpcWaitMay @RpcRefLogGet (TimeoutSec 1) reflog (coerce rk)
              <&> join

    for_ mhead $ \mh -> do

      txs <- S.toList_ $ do
                walkMerkle @[HashRef] (fromHashRef mh) (getBlock sto) $ \case
                  Left{}   -> do
                    pure ()

                  Right hxs -> do
                    for_ hxs $ \htx -> void $ runMaybeT do
                      -- done  <- liftIO $ withDB db (isTxProcessed (HashVal htx))
                      -- done1 <- liftIO $ withDB db (isTxProcessed (processedRepoTx (gitLwwRef,htx)))
                      -- guard (not done && not done1)
                      getBlock sto (fromHashRef htx) >>= toMPlus
                         <&> deserialiseOrFail @(RefLogUpdate L4Proto)
                         >>= toMPlus
                         >>= unpackTx
                         >>= \(n,h,blk) -> lift (S.yield (n,htx,blk))


      headz <- S.toList_ do
        for_ txs $ \(n,tx,blk) -> void $ runMaybeT do
          (rhh, rhead) <- readRepoHeadFromTx sto tx >>= toMPlus
          debug $ yellow "found repo head" <+> pretty rhh <+> pretty "for" <+> pretty lw
          lift $ S.yield (lw, RepoHeadTx tx, RepoHeadRef rhh, rhead)

      withState $ transactional do
        for_ headz $ \(l, tx, rh, rhead) -> do
          let rlwwseq = RepoLwwSeq (fromIntegral $ lwwSeq wv)
          insertRepoHead l rlwwseq (RepoRefLog rk) tx rh rhead

