module HBS2.Git.DashBoard.State.Index.Peer where

import HBS2.Git.DashBoard.Prelude
import HBS2.Git.DashBoard.Types
import HBS2.Git.DashBoard.State
import HBS2.Git.DashBoard.Manifest
import HBS2.Git.Data.LWWBlock
import HBS2.Git.Data.Tx.Git

import HBS2.Hash

import HBS2.System.Dir

import Streaming.Prelude qualified as S

import System.Process.Typed

{- HLINT ignore "Functor law" -}

seconds = TimeoutSec

updateFixmeFor :: ( MonadUnliftIO m
                  , MonadReader DashBoardEnv m
                  )
               => RepoLww
               -> MyRefChan
               -> m ()
updateFixmeFor (RepoLww lw) f = do
  p <- fixmeDataPath f
  debug $ red "UPDATE-FIXME-FOR" <+> pretty (AsBase58 lw) <+> pretty (AsBase58 f) <+> pretty p

  let rcp = show $ pretty (AsBase58 f)

  mkdir p

  let cmdStr = [qc|fixme-new refchan {rcp} and fixme:refchan:import|]
  let cmd = shell cmdStr & setWorkingDir p

  debug $ "run fixme for:" <+> pretty rcp <+> pretty cmdStr

  void $ runProcess cmd


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

                      -- done <- lift $ withState $ isProcessed (HashRef $ hashObject @HbSync (serialise (lw,htx)))

                      -- guard (not done)

                      getBlock sto (fromHashRef htx) >>= toMPlus
                         <&> deserialiseOrFail @(RefLogUpdate L4Proto)
                         >>= toMPlus
                         >>= unpackTx
                         >>= \(n,h,blk) -> lift (S.yield (n,htx,blk))


      headz <- S.toList_ do
        for_ txs $ \(n,tx,blk) -> void $ runMaybeT do
          (rhh, rhead) <- readRepoHeadFromTx sto tx >>= toMPlus
          debug $ yellow "found repo head" <+> pretty rhh <+> pretty "for" <+> pretty lw
          (man, _) <- parseManifest rhead
          let fme = headMay [ x | FixmeRefChanP x <- man ]
          lift $ S.yield (lw, RepoHeadTx tx, RepoHeadRef rhh, rhead, fme)

      withState $ transactional do
        for_ headz $ \(l, tx, rh, rhead, fme) -> do
          let rlwwseq = RepoLwwSeq (fromIntegral $ lwwSeq wv)
          insertRepoHead l rlwwseq (RepoRefLog rk) tx rh rhead

          -- insertProcessed (HashRef $ hashObject @HbSync (serialise (l,coerce @_ @HashRef tx)))

          for_ fme $ \f -> do
            insertRepoFixme l rlwwseq f

      buildCommitTreeIndex (coerce lw)

  fxe <- selectRepoFixme

  for_ fxe $ \(r,f)  -> do
    allowed <- checkFixmeAllowed r
    when allowed do
      env <-ask
      addJob (withDashBoardEnv env $ updateFixmeFor r f)



