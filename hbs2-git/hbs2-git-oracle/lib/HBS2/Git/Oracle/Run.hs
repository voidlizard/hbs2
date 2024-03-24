module HBS2.Git.Oracle.Run where

import HBS2.Git.Oracle.Prelude
import HBS2.Git.Oracle.App

import HBS2.Merkle

import HBS2.Git.Data.LWWBlock
import HBS2.Git.Data.Tx

import Data.Maybe
import Lens.Micro.Platform

import Data.Word
import Streaming.Prelude qualified as S
import Codec.Serialise
import Control.Monad.Trans.Maybe
import Data.Coerce
import Data.Ord
import Control.Monad.Trans.Except
import Data.List
import Safe

{- HLINT ignore "Functor law" -}

data GitRepoFact =
  GitRepoFact1
  { gitLwwRef :: LWWRefKey HBS2Basic
  , gitLwwSeq :: Word64
  , gitRefLog :: RefLogKey HBS2Basic
  }
  deriving stock (Generic)

data GitRepoHeadFact =
  GitRepoHeadFact
  {
  }
  deriving stock (Generic)


instance Serialise GitRepoFact

instance Pretty GitRepoFact where
  pretty (GitRepoFact1{..}) =
    parens ( "gitrepofact1" <+>  hsep [pretty gitLwwRef, pretty gitLwwSeq, pretty gitRefLog])


makeGitRepoFactBlock :: MonadUnliftIO m => [GitRepoFact] -> Oracle m HashRef
makeGitRepoFactBlock facts = do
  undefined

runOracle :: forall m  . MonadUnliftIO m => Oracle m ()
runOracle = do
  debug "hbs2-git-oracle"

  debug "list all git references from peer"

  peer   <- asks _peerAPI
  reflog <- asks _reflogAPI
  sto    <- asks _storage

  polls <- callRpcWaitMay @RpcPollList2 (TimeoutSec 1) peer (Just "lwwref", Nothing)
            <&> join . maybeToList
            <&> fmap (LWWRefKey @HBS2Basic . view _1)

  repos <- S.toList_ $ forM_ polls $ \r -> void $ runMaybeT do
            (lw,blk) <- readLWWBlock sto r >>= toMPlus
            let rk = lwwRefLogPubKey blk

            lift $ S.yield $
              GitRepoFact1 r
                           (lwwSeq lw)
                           (RefLogKey rk)

  for_ repos $ \what@GitRepoFact1{..} -> do

    mhead <- callRpcWaitMay @RpcRefLogGet (TimeoutSec 1) reflog (coerce gitRefLog)
              <&> join

    forM_ mhead $ \mh -> do

      txs <- S.toList_ $ do
                walkMerkle @[HashRef] (fromHashRef mh) (getBlock sto) $ \case
                  Left{}   -> do
                    pure ()

                  Right hxs -> do
                    for_ hxs $ \htx -> void $ runMaybeT do
                      getBlock sto (fromHashRef htx) >>= toMPlus
                         <&> deserialiseOrFail @(RefLogUpdate L4Proto)
                         >>= toMPlus
                         >>= unpackTx
                         >>= \(n,h,_) -> lift (S.yield (n,htx))

      let tx' = maximumByMay (comparing fst) txs

      forM_ tx' $ \(n,tx) -> void $ runMaybeT do
        RepoHeadSimple{..} <- readRepoHeadFromTx sto tx >>= toMPlus

        let enc = if isJust _repoHeadGK0 then "E" else "P"
        let name  = _repoHeadName
        let brief = _repoHeadBrief
        let manifest = _repoManifest

        debug $ "found head"
          <+> pretty gitLwwRef
          <+> pretty n
          <+> pretty gitRefLog
          <+> pretty name
          <+> pretty brief
          <+> pretty manifest
          <+> pretty enc
          <+> pretty tx


