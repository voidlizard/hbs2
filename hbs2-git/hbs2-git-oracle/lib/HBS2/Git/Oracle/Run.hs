module HBS2.Git.Oracle.Run where

import HBS2.Git.Oracle.Prelude
import HBS2.Git.Oracle.App

import HBS2.Hash
import HBS2.Merkle
import HBS2.Data.Types.SignedBox

import HBS2.KeyMan.Keys.Direct

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
import Data.ByteString.Lazy qualified as LBS
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
  { gitRepoHeadRef   :: HashRef
  , gitRepoName      :: Text
  , gitRepoBrief     :: Text
  , gitRepoEncrypted :: Bool
  }
  deriving stock (Generic)


data GitRepoFacts =
      GitRepoRefFact  GitRepoRefFact
    | GitRepoHeadFact HashRef GitRepoHeadFact
    deriving stock (Generic)


instance Serialise GitRepoRefFact
instance Serialise GitRepoHeadFact
instance Serialise GitRepoFacts

instance Pretty GitRepoFacts  where
  pretty (GitRepoRefFact x)    = pretty x
  pretty (GitRepoHeadFact _ x) = pretty x

instance Pretty GitRepoRefFact where
  pretty (GitRepoFact1{..}) =
    parens ( "gitrepofact1" <+>  hsep [pretty gitLwwRef, pretty gitLwwSeq, pretty gitRefLog])

instance Pretty GitRepoHeadFact where
  pretty (GitRepoHeadFact1{..}) =
    parens ( "gitrepoheadfact1" <+>  hsep [pretty gitRepoHeadRef])


runOracle :: forall m  . MonadUnliftIO m
          => Oracle m ()
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

  facts <- S.toList_ do

    for_ repos $ \what@GitRepoFact1{..} -> do

      mhead <- lift $ callRpcWaitMay @RpcRefLogGet (TimeoutSec 1) reflog (coerce gitRefLog)
                <&> join

      for_ mhead $ \mh -> do

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

        for_ tx' $ \(n,tx) -> void $ runMaybeT do

          (rhh,RepoHeadSimple{..}) <- readRepoHeadFromTx sto tx
                                         >>= toMPlus

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

          let repoFactHash = hashObject @HbSync (serialise what) & HashRef

          let f1 = GitRepoRefFact what
          let f2 = GitRepoHeadFact
                      repoFactHash
                      (GitRepoHeadFact1 rhh name brief enc)

          lift $ S.yield f1
          lift $ S.yield f2

  rchanAPI <- asks _refchanAPI
  chan  <- asks _refchanId
  auPk  <- asks _refchanAuthor

  auCreds <- runKeymanClient do
              loadCredentials auPk >>= orThrowUser "can't load credentials"

  let ppk = view peerSignPk auCreds
  let psk = view peerSignSk auCreds

  for_ facts $ \f -> do
    let box = makeSignedBox @L4Proto ppk psk (LBS.toStrict $ serialise f)
    void $ callRpcWaitMay @RpcRefChanPropose (TimeoutSec 1) rchanAPI (chan, box)
    debug $ "posted tx" <+> pretty (hashObject @HbSync (serialise f))

