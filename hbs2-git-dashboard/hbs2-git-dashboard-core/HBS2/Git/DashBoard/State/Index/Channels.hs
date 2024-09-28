module HBS2.Git.DashBoard.State.Index.Channels where

import HBS2.Git.DashBoard.Prelude
import HBS2.Git.DashBoard.Types
import HBS2.Git.DashBoard.State

import DBPipe.SQLite hiding (insert)
import DBPipe.SQLite.Generic as G

import Streaming.Prelude qualified as S

updateIndexFromChannels :: (DashBoardPerks m, MonadReader DashBoardEnv m) => m ()
updateIndexFromChannels = do
  debug "updateIndexChannels"

  rchanAPI <- asks _refChanAPI
  sto      <- asks _sto

  flip runContT pure do

    es <- lift getIndexEntries

    for_ es $ \rc -> do
      callCC \next -> do
        debug $ red (pretty (AsBase58 rc))

        h <- lift (callRpcWaitMay @RpcRefChanGet (1 :: Timeout 'Seconds) rchanAPI rc)
               <&> join
               >>= maybe (next ()) pure

        debug $ "rechan val" <+> red (pretty h)

        txs <- S.toList_ do
          walkMerkle @[HashRef] (coerce h) (getBlock sto) $ \case
            Left{} -> pure ()
            Right hs -> mapM_ S.yield hs

        for_ txs $ \txh -> void $ runMaybeT do

            done <- lift $ lift $ withState do
                      select @(Only Int)
                             [qc|select 1 from processed where hash = ? limit 1|]
                             (Only (TxHash txh)) <&> isJust . listToMaybe

            guard (not done)

            tx@GitIndexTx{..} <- getBlock sto (coerce txh)
                                    >>= toMPlus
                                    >>= readProposeTranMay @(GitIndexTx 'HBS2Basic) @L4Proto
                                    >>= toMPlus

            lift $ lift $ withState $ transactional do
              let nm  = [ RepoName n  | GitIndexRepoName n <- universeBi gitIndexTxPayload ] & headMay
              let bri = [ RepoBrief n | GitIndexRepoBrief n <- universeBi gitIndexTxPayload ] & headMay

              insert @RepoTable $ onConflictIgnore @RepoTable (Only (RepoLww gitIndexTxRef))

              insert @RepoChannelTable $
                onConflictIgnore @RepoChannelTable (RepoLww gitIndexTxRef, RepoChannel rc)

              -- FIXME: on-conflict-update!
              for_ nm $ \n -> do
                insert @RepoNameTable $
                  onConflictIgnore @RepoNameTable (RepoLww gitIndexTxRef, n)

              for_ bri $ \n -> do
                insert @RepoBriefTable $
                  onConflictIgnore @RepoBriefTable (RepoLww gitIndexTxRef, n)

        lift $ withState $ transactional do
          for_ txs $ \t -> do
            insert @TxProcessedTable $ onConflictIgnore @TxProcessedTable (Only (TxHash t))



