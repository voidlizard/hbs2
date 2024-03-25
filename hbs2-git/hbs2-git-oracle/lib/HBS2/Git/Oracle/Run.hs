module HBS2.Git.Oracle.Run where

import HBS2.Git.Oracle.Prelude
import HBS2.Git.Oracle.App

import HBS2.Hash
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
import Data.Text qualified as Text
import Control.Monad.Trans.Except
import Data.List
import Safe

{- HLINT ignore "Functor law" -}

data GitRepoRefFact =
  GitRepoFact1
  { gitLwwRef :: LWWRefKey HBS2Basic
  , gitLwwSeq :: Word64
  , gitRefLog :: RefLogKey HBS2Basic
  }
  deriving stock (Generic)

data GitRepoHeadFact =
  GitRepoHeadFact1
  { gitRepoName      :: Text
  , gitRepoBrief     :: Text
  , gitRepoEncrypted :: Bool
  , gitRepoHeadRef   :: HashRef
  }
  deriving stock (Generic)


data GitRepoFacts =
      GitRepoRefFact  GitRepoRefFact
    | GitRepoHeadFact HashRef GitRepoHeadFact
    deriving stock (Generic)

instance Serialise GitRepoRefFact
instance Serialise GitRepoHeadFact
instance Serialise GitRepoFacts

instance Pretty GitRepoRefFact where
  pretty (GitRepoFact1{..}) =
    parens ( "gitrepofact1" <+>  hsep [pretty gitLwwRef, pretty gitLwwSeq, pretty gitRefLog])


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

      headFact <- forM tx' $ \(n,tx) -> void $ runMaybeT do

        (rhh,RepoHeadSimple{..}) <- readRepoHeadFromTx sto tx
                                       >>= toMPlus

        let enc = isJust _repoHeadGK0
        let name  = Text.take 256 $  _repoHeadName
        let brief = Text.take 1024 $ _repoHeadBrief
        let manifest = _repoManifest

        debug $ "found head"
          <+> pretty enc
          <+> pretty n
          <+> pretty gitLwwRef
          <+> pretty gitRefLog
          <+> pretty tx
          <> line
          <+> pretty name
          <+> pretty brief
          <+> pretty manifest
          <> line
          <> line

        let f1 = GitRepoRefFact what
        let f2 = GitRepoHeadFact
                    repoFactHash
                    (GitRepoHeadFact1 name brief enc undefined
                    )

        pure undefined

      pure ()
        -- debug $ "found head"
        --   <+> pretty gitLwwRef
        --   <+> pretty n
        --   <+> pretty enc
        --   <+> pretty gitRefLog
        --   <+> pretty name
        --   <+> pretty brief
        --   <+> pretty manifest
        --   <+> pretty tx

