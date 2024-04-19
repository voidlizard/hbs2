module HBS2.Git.DashBoard.State.Index.Peer where

import HBS2.Git.DashBoard.Prelude
import HBS2.Git.DashBoard.Types
import HBS2.Git.DashBoard.State
import HBS2.Git.Data.LWWBlock
import HBS2.Git.Data.Tx.Git

import Streaming.Prelude qualified as S

{- HLINT ignore "Functor law" -}

updateIndexFromPeer :: (DashBoardPerks m, HasConf m, MonadReader DashBoardEnv m) => m ()
updateIndexFromPeer = do
  debug "updateIndexFromPeer"

  peer   <- asks _peerAPI
  reflog <- asks _refLogAPI
  sto    <- asks _sto


  polls <- callRpcWaitMay @RpcPollList2 (TimeoutSec 1) peer (Just "lwwref", Nothing)
            <&> join . maybeToList
            <&> fmap (LWWRefKey @HBS2Basic . view _1)

  repos <- S.toList_ $ forM_ polls $ \r -> void $ runMaybeT do
            (lw,blk) <- readLWWBlock sto r >>= toMPlus
            let rk = lwwRefLogPubKey blk

            lift $ S.yield (r,RefLogKey @'HBS2Basic rk,blk)

  for_ repos $ \(lw,rk,LWWBlockData{..}) -> do

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
          lift $ S.yield (lw, rhh, rhead)

      withState $ transactional do
        for_ headz $ \(l, rh, rhead) -> do
          insertRepoHead l rh rhead

  -- db <- asks _db

  -- facts <- S.toList_ do

  --   for_ repos $ \(lw,rk,LWWBlockData{..}) -> do

  --     mhead <- lift $ callRpcWaitMay @RpcRefLogGet (TimeoutSec 1) reflog (coerce rk)
  --               <&> join

  --     for_ mhead $ \mh -> do

  --       txs <- S.toList_ $ do
  --                 walkMerkle @[HashRef] (fromHashRef mh) (getBlock sto) $ \case
  --                   Left{}   -> do
  --                     pure ()

  --                   Right hxs -> do
  --                     for_ hxs $ \htx -> void $ runMaybeT do
  --                       -- done  <- liftIO $ withDB db (isTxProcessed (HashVal htx))
  --                       -- done1 <- liftIO $ withDB db (isTxProcessed (processedRepoTx (gitLwwRef,htx)))
  --                       -- guard (not done && not done1)
  --                       getBlock sto (fromHashRef htx) >>= toMPlus
  --                          <&> deserialiseOrFail @(RefLogUpdate L4Proto)
  --                          >>= toMPlus
  --                          >>= unpackTx
  --                          >>= \(n,h,blk) -> lift (S.yield (n,htx,blk))

  --       relAlready <- lift $ withDB db do
  --         -- FIXME: uncomment-for-speedup
  --         done <- isGitRepoBundleProcessed mh >> pure False
  --         unless done do
  --           transactional do
  --             for_ txs $ \(n,_,bu) -> do
  --               refs <- fromRight mempty <$> readBundleRefs sto bu
  --               for_ refs  $ \r -> do
  --                 debug $ red "bundle-fact" <+> pretty lw <+> pretty r
  --                 insertRepoBundleFact (GitRepoBundle (GitLwwRef lw) (GitBundle r))

  --             insertGitRepoBundleProcessed mh
  --         pure done

  --       -- let tx' = maximumByMay (comparing (view _1)) txs

  --       for_ txs $ \(n,tx,blk) -> void $ runMaybeT do
  --         liftIO $ withDB db do
  --           transactional do
  --             for_ [ t | (i,t,_) <- txs, i < n ] $ \tran -> do
  --               insertTxProcessed (HashVal tran)

  --         (rhh,RepoHeadSimple{..}) <- readRepoHeadFromTx sto tx
  --                                        >>= toMPlus

  --         let name  = Text.take 256 $  _repoHeadName
  --         let brief = Text.take 1024 $ _repoHeadBrief
  --         let manifest = _repoManifest

  --         lift $ S.yield $ GitRepoFacts
  --                            (GitLwwRef lw)
  --                            (GitLwwSeq lwwRefSeed)
  --                            (GitRefLog rk)
  --                            (GitTx tx)
  --                            (GitRepoHeadRef rhh)
  --                            (GitRepoHeadSeq (fromIntegral n))
  --                            (GitName (Just name))
  --                            (GitBrief (Just brief))
  --                            (GitEncrypted _repoHeadGK0)
  --                            [GitRepoExtendedManifest (GitManifest manifest)]

  --         -- yield repo relation facts by common bundles
  --         unless relAlready do

  --           what <- withDB db do
  --             select_ @_ @(GitLwwRef, GitLwwRef) [qc|
  --               select distinct
  --                   b1.lwwref
  --                 , b2.lwwref
  --                from gitrepobundle b1 join gitrepobundle  b2 on b1.bundle = b2.bundle
  --                where b1.lwwref <> b2.lwwref
  --             |]

  --           let r = HM.fromListWith (<>) [ (a, HS.singleton b) | (a,b) <- what ]
  --                    & HM.toList

  --           for_ r $ \(lww, rel) -> do
  --             lift $ S.yield $ GitRepoRelatedFact lww rel

          -- liftIO $ withDB db (insertTxProcessed (HashVal tx))

