{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
module HBS2.Git.Oracle.Run where

import HBS2.Git.Oracle.Prelude
import HBS2.Git.Oracle.App
import HBS2.Git.Oracle.Facts
import HBS2.Git.Oracle.State
import HBS2.Git.Oracle.Html

import HBS2.Actors.Peer hiding (handle)

import HBS2.Hash
import HBS2.Merkle
import HBS2.Data.Types.SignedBox

import HBS2.KeyMan.Keys.Direct

import HBS2.Git.Data.LWWBlock
import HBS2.Git.Data.Tx

import HBS2.Peer.HTTP.Root
import HBS2.Peer.Proto.BrowserPlugin

import DBPipe.SQLite

import Data.ByteString.Lazy (ByteString)

import Data.Maybe
import Lens.Micro.Platform hiding ( (.=) )

import Lucid.Base hiding (for_)
import Lucid.Html5 hiding (for_,select_)

import Control.Applicative
import Data.Aeson as Aeson
import Data.Aeson.Encode.Pretty qualified as A
import Streaming.Prelude qualified as S
import Codec.Serialise
import Control.Monad.Trans.Maybe
import Data.Coerce
import Data.Ord
import Data.Text qualified as Text
import Data.List qualified as List
import Data.HashMap.Strict qualified as HM
import Data.ByteString.Lazy qualified as LBS
import System.Process.Typed
import Text.InterpolatedString.Perl6 (qc)
import System.Environment
import System.Posix.Signals
import System.FilePath
import Data.Either
import Control.Monad.Except
import Control.Exception (SomeException)
import Control.Exception qualified as E
import Data.Word


import System.Exit

{- HLINT ignore "Functor law" -}

withParams :: [Text] -> PluginMethod -> PluginMethod
withParams ps (Method p a) = createPluginMethod p (HM.toList a <> pa)
  where pa = zipWith (\n s -> ([qc|_{n}|],s)) [1..] ps

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
                             (GitRepoHeadSeq (fromIntegral n))
                             (GitName (Just name))
                             (GitBrief (Just brief))
                             (GitEncrypted _repoHeadGK0)
                             [GitRepoExtendedManifest (GitManifest manifest)]

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

  let kw = ["OUTPUT", "METHOD", "URL_PREFIX", "RAW_PATH_INFO"]
           <> [ [qc|_{i}|] | i <- [0..9] ]

  self <- liftIO getProgName

  env <- liftIO getEnvironment
           <&> fmap (over _1 Text.pack . over _2 Text.pack)

  path <- liftIO (lookupEnv "PATH_INFO")
           <&> fmap splitDirectories
           <&> fromMaybe mempty
           <&> fmap Text.pack

  let cmd = proc self ["pipe", "-r", show (pretty (AsBase58 pks))]
              & setStdin createPipe
              & setStdout createPipe

  flip runContT (const $ liftIO exitSuccess) do

    p <- ContT $ withProcessWait cmd

    let ssin = getStdin p
    let sout = getStdout p
    client <- newMessagingPipe (sout,ssin) -- ,sout)

    void $ ContT $ withAsync $ runMessagingPipe client

    caller <- makeServiceCaller @BrowserPluginAPI @PIPE (localPeer client)

    void $ ContT $ withAsync $ liftIO $ runReaderT (runServiceClient caller) client

    wtf <- callService @RpcChannelQuery caller (createPluginMethod path env & filterKW kw)
            >>= orThrowUser "can't query rpc"

    r <- ContT $ maybe1 wtf (liftIO (hClose ssin) >> void (waitExitCode p))

    hClose ssin

    liftIO $ LBS.putStr r >> hFlush stdout

    void $ waitExitCode p

class HasOracleEnv m where
  getOracleEnv :: m OracleEnv

instance Monad m => HasOracleEnv (Oracle m) where
  getOracleEnv = ask

instance (Monad m, HasOracleEnv m) => HasOracleEnv (MaybeT m) where
  getOracleEnv = lift getOracleEnv

hardened :: (HasOracleEnv m, MonadIO m)
         => Oracle IO (Maybe a)
         -> m (Maybe a)
hardened m = do
  env <- getOracleEnv
  liftIO $ E.try @SomeException (withOracleEnv env m) >>= \case
    Left e -> err (viaShow e) >> pure Nothing
    Right x -> pure $ x

-- API handler
instance (MonadUnliftIO m, HasOracleEnv m) => HandleMethod m RpcChannelQuery where
  handleMethod req@(Method path args) = hardened do
    env <- getOracleEnv

    debug $ green "PLUGIN: HANDLE METHOD!" <+> viaShow req

    let cmd  = maybe path List.singleton $ HM.lookup "METHOD" args

    case cmd of
      ("debug":_)        -> listEnv req
      []                 -> listEntries req
      ("list-entries":_) -> listEntries req
      ("/":_)            -> listEntries req

      ("repo" : params)  -> do
        renderRepo (withParams params req)

      _                  -> pure Nothing

    where
      listEnv (Method _ a) = do
        pure $ Just $ A.encodePretty a

      listEntries (Method _ a) = do
        env <- getOracleEnv
        withOracleEnv env do
          items <- withState $ select_ @_ @(HashVal, Text, Text, Word64) [qc|

              SELECT
                lwwref,
                name,
                brief,
                repoheadseq
              FROM (
                SELECT
                  lwwref,
                  name,
                  brief,
                  repoheadseq,
                  ROW_NUMBER() OVER (PARTITION BY lwwref ORDER BY lwwseq DESC, repoheadseq DESC) as rn
                FROM gitrepofact
              ) as s0
              WHERE rn = 1;

                   |]

          case HM.lookup "OUTPUT" a of
            Just "html" -> formatHtml items
            Just "json" -> formatJson items
            _           -> formatJson items


      renderRepo req@(Method _ kw) = runMaybeT do
        env <- getOracleEnv
        sto <- getOracleEnv <&> _storage

        debug $ yellow "ONE"

        ref <- HM.lookup "_1" kw  & toMPlus
                 <&> Text.unpack
                 <&> fromStringMay @HashRef
                 >>= toMPlus

        debug $ yellow "TWO"

        mf <- lift ( withOracleEnv env do
                withState $ select @GitRepoPage [qc|
                       select  v.lwwref
                             , v.repohead
                             , v.name
                             , v.brief
                             , m.manifest
                       from vrepofact v left join gitrepomanifest m on v.repohead = m.repohead
                       where v.lwwref = ?
                       limit 1
                       |] (Only ref)
                    )  <&> headMay
                       >>= toMPlus

        renderRepoHtml req mf

      formatJson items = do
          let root = object [ "rows"  .= items
                            , "desc"  .= [ "entity", "name", "brief", "timestamp" ]
                            ]

          pure $ Just $ A.encodePretty root

      formatHtml items = do
        renderEntries req items <&> Just


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

  setLogging @DEBUG  (logPrefix "" . toStderr)
  setLogging @ERROR  (logPrefix "" . toStderr)
  setLogging @WARN   (logPrefix "" . toStderr)

  chan <- asks _refchanId
  debug $ green "RUN PIPE!!!"

  liftIO $ void $ installHandler sigPIPE Ignore Nothing

  flip runContT pure do

    server  <- newMessagingPipe (stdin,stdout)

    void $ ContT $ withAsync (runMessagingPipe server)

    void $ ContT $ withAsync $ do
      pause @'Seconds 10
      forever do
        debug $ yellow "updateState"
        updateState
        pause @'Seconds 60

    -- make server protocol responder
    void $ ContT $ withAsync $ flip runReaderT server do
        runProto @PIPE
          [ makeResponse (makeServer @BrowserPluginAPI)
          ]

    fix \next -> do
      done1 <- hIsClosed stdin
      done2 <- hIsClosed stdout
      done3 <- hIsEOF stdin
      let done = done1 || done2 || done3
      unless done (pause @'Seconds 0.01 >> next)


updateState :: MonadUnliftIO m => Oracle m ()
updateState = do
  debug $ yellow "update state"

  chan  <- asks _refchanId
  rchanAPI <- asks _refchanAPI
  peerAPI <- asks _peerAPI
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

    let refs = [ h | GitRepoHeadRef h <- universeBi facts ]

    w <- for refs $ \r -> do
      -- TODO: dont-fetch-repeatedly
      debug $ red "repo-head-to-fetch" <+> pretty r
      lift $ async (callRpcWaitMay @RpcFetch (TimeoutSec 1) peerAPI r)

    lift $ mapM_ wait w

