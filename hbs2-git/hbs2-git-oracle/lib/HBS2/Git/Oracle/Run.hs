{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
module HBS2.Git.Oracle.Run where

import HBS2.Git.Oracle.Prelude
import HBS2.Git.Oracle.App
import HBS2.Git.Oracle.Facts
import HBS2.Git.Oracle.State

import HBS2.Actors.Peer

import HBS2.Hash
import HBS2.Merkle
import HBS2.Data.Types.SignedBox

import HBS2.KeyMan.Keys.Direct

import HBS2.Git.Data.LWWBlock
import HBS2.Git.Data.Tx

import DBPipe.SQLite

import Data.ByteString.Lazy (ByteString)


import Data.Maybe
import Lens.Micro.Platform hiding ( (.=) )

import Data.Aeson as Aeson
import Data.Aeson.Encode.Pretty qualified as A
import Streaming.Prelude qualified as S
import Codec.Serialise
import Control.Monad.Trans.Maybe
import Data.Coerce
import Data.Ord
import Data.Text qualified as Text
import Data.HashMap.Strict qualified as HM
import Data.ByteString.Lazy qualified as LBS
import System.Process.Typed
import Text.InterpolatedString.Perl6 (qc)
import System.Environment (getProgName, getArgs)

{- HLINT ignore "Functor law" -}

runOracleIndex :: forall m  . MonadUnliftIO m
               => PubKey 'Sign HBS2Basic
               -> Oracle m ()
runOracleIndex auPk = do
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

            lift $ S.yield (r,RefLogKey rk,blk)

  db <- asks _db

  facts <- S.toList_ do

    for_ repos $ \(lw,rk,LWWBlockData{..}) -> do

      mhead <- lift $ callRpcWaitMay @RpcRefLogGet (TimeoutSec 1) reflog (coerce rk)
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
                           >>= \(n,h,_) -> lift (S.yield (n,htx))

        let tx' = maximumByMay (comparing fst) txs

        for_ tx' $ \(n,tx) -> void $ runMaybeT do
          liftIO $ withDB db do
            transactional do
              for_ [ t | (i,t) <- txs, i < n ] $ \tran -> do
                insertTxProcessed (HashVal tran)

          (rhh,RepoHeadSimple{..}) <- readRepoHeadFromTx sto tx
                                         >>= toMPlus

          let name  = Text.take 256 $  _repoHeadName
          let brief = Text.take 1024 $ _repoHeadBrief
          let manifest = _repoManifest

          lift $ S.yield $ GitRepoFacts
                             (GitLwwRef lw)
                             (GitLwwSeq lwwRefSeed)
                             (GitRefLog rk)
                             (GitTx tx)
                             (GitRepoHeadRef rhh)
                             (GitName (Just name))
                             (GitBrief (Just brief))
                             (GitEncrypted _repoHeadGK0)
                             mempty

          -- liftIO $ withDB db (insertTxProcessed (HashVal tx))

  rchanAPI <- asks _refchanAPI
  chan  <- asks _refchanId

  auCreds <- runKeymanClient do
              loadCredentials auPk >>= orThrowUser "can't load credentials"

  let ppk = view peerSignPk auCreds
  let psk = view peerSignSk auCreds

  for_ facts $ \f -> do
    let box = makeSignedBox @L4Proto ppk psk (LBS.toStrict $ serialise f)
    void $ callRpcWaitMay @RpcRefChanPropose (TimeoutSec 1) rchanAPI (chan, box)
    debug $ "posted tx" <+> pretty (hashObject @HbSync (serialise f))

  -- FIXME: ASAP-wait-refchan-actually-updated
  -- pause @'Seconds 0.25

  updateState

runDump :: forall m  . MonadUnliftIO m
        => PKS
        -> m ()

runDump pks = do
  self <- liftIO getProgName

  let cmd = proc self ["pipe", "-r", show (pretty (AsBase58 pks))]
              & setStdin createPipe
              & setStdout createPipe

  flip runContT pure do

    p <- ContT $ withProcessWait cmd

    let ssin = getStdin p
    let sout = getStdout p
    client <- newMessagingPipe (sout,ssin) -- ,sout)

    void $ ContT $ withAsync $ runMessagingPipe client

    caller <- makeServiceCaller @BrowserPluginAPI @PIPE (localPeer client)

    void $ ContT $ withAsync $ liftIO $ runReaderT (runServiceClient caller) client

    wtf <- callService @RpcChannelQuery caller ()
            >>= orThrowUser "can't query rpc"

    r <- ContT $ maybe1 wtf (pure ())

    let val = Aeson.decode @Value r

    liftIO $ LBS.putStr (A.encodePretty val)

data RpcChannelQuery

-- API definition
type BrowserPluginAPI = '[ RpcChannelQuery ]

-- API endpoint definition
type instance Input RpcChannelQuery = ()
type instance Output RpcChannelQuery = Maybe ByteString

class HasOracleEnv m where
  getOracleEnv :: m OracleEnv

-- API handler
instance (MonadUnliftIO m, HasOracleEnv m) => HandleMethod m RpcChannelQuery where
  handleMethod _ = do
    env <- getOracleEnv
    -- let chan  = _refchanId env
    -- let rchanAPI = _refchanAPI env
    -- let sto = _storage env


    withOracleEnv env do
      items <- withState $ select_ @_ @(HashVal, Text, Text) [qc|
                SELECT
                  g.ref,
                  gn.name,
                  gb.brief
                FROM
                  gitrepo AS g
                INNER JOIN
                  gitreponame AS gn ON g.ref = gn.ref
                INNER JOIN
                  gitrepoheadversion AS ghv ON gn.hash = ghv.hash
                LEFT JOIN
                  gitrepobrief AS gb ON g.ref = gb.ref AND ghv.hash = gb.hash
                GROUP BY
                  g.ref, gn.name
                |]

      let root = object [ "rows"  .= items
                        , "desc"  .= [ "entity", "name", "brief" ]
                        ]

      pure $ Just $ A.encodePretty root

-- Codec for protocol
instance HasProtocol PIPE (ServiceProto BrowserPluginAPI PIPE) where
  type instance ProtocolId (ServiceProto BrowserPluginAPI PIPE) = 0xDEADF00D123
  type instance Encoded PIPE = ByteString
  decode = either (const Nothing) Just . deserialiseOrFail
  encode = serialise

-- Some "deferred" implementation for our monad
--   note -- plain asyncs may cause to resource leak
instance (MonadUnliftIO  m, HasProtocol PIPE (ServiceProto api PIPE))
  => HasDeferred (ServiceProto api PIPE) PIPE m where
  deferred m = void (async m)

-- FIXME: looks-hacky
instance (Monad (t (Oracle m)), MonadIO m, MonadTrans t) => HasOracleEnv (ResponseM PIPE (t (Oracle m))) where
  getOracleEnv = lift $ lift ask

runPipe :: forall m  . MonadUnliftIO m
        => Oracle m ()

runPipe = do
  chan <- asks _refchanId
  debug "run pipe"

  flip runContT pure do

    server  <- newMessagingPipe (stdin,stdout)

    void $ ContT $ withAsync $ runMessagingPipe server

    void $ ContT $ withAsync $ forever do
      debug $ yellow "updateState"
      updateState
      pause @'Seconds 60

    -- make server protocol responder
    -- void $ ContT $ withAsync $ flip
    lift $ flip runReaderT server do
        runProto @PIPE
          [ makeResponse (makeServer @BrowserPluginAPI)
          ]


updateState :: MonadUnliftIO m => Oracle m ()
updateState = do
  debug $ yellow "update state"

  chan  <- asks _refchanId
  rchanAPI <- asks _refchanAPI
  sto  <- asks  _storage
  db   <- asks _db

  void $ runMaybeT do

    rv <- lift (callRpcWaitMay @RpcRefChanGet (TimeoutSec 1) rchanAPI chan)
            >>= toMPlus >>= toMPlus

    facts <- S.toList_ do
      walkMerkle @[HashRef] (fromHashRef rv) (getBlock sto) $ \case
        Left{} -> pure ()
        Right txs -> do
          -- FIXME: skip-already-processed-blocks
          for_ txs $ \htx -> void $ runMaybeT do
            done <- liftIO $ withDB db (isTxProcessed (HashVal htx))
            guard (not done)
            getBlock sto (fromHashRef htx)
             >>= toMPlus
             <&> deserialiseOrFail @(RefChanUpdate L4Proto)
             >>= toMPlus
             >>= \case
                   Propose _ box -> pure box
                   _             -> mzero
             <&> unboxSignedBox0
             >>= toMPlus
             <&> snd
             >>= \(ProposeTran _ box) -> toMPlus (unboxSignedBox0 box)
             <&> snd
             <&> deserialiseOrFail @GitRepoFacts . LBS.fromStrict
             >>= lift . S.yield . (htx,)

    lift $ withState $ transactional do

      for_ facts $ \case
        (tx, Right f) -> do
          debug $ "GOOD FACT" <+> pretty tx
          insertRepoFacts f
          insertTxProcessed (HashVal tx)

        (tx, _) -> do
          debug "BAD FACT"
          insertTxProcessed (HashVal tx)

