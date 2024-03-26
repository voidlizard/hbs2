{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
module HBS2.Git.Oracle.Run where

import HBS2.Git.Oracle.Prelude
import HBS2.Git.Oracle.App

import HBS2.Actors.Peer

import HBS2.Hash
import HBS2.Merkle
import HBS2.Data.Types.SignedBox

import HBS2.KeyMan.Keys.Direct

import HBS2.Git.Data.LWWBlock
import HBS2.Git.Data.Tx

import Data.ByteString.Lazy (ByteString)


import Data.Maybe
import Lens.Micro.Platform hiding ( (.=) )

import Data.Aeson as Aeson
import Data.Aeson.Encode.Pretty qualified as A
import Data.Word
import Streaming.Prelude qualified as S
import Codec.Serialise
import Control.Monad.Trans.Maybe
import Data.Coerce
import Data.Ord
import Data.Text qualified as Text
import Data.HashMap.Strict qualified as HM
import Data.ByteString.Lazy qualified as LBS
import System.Process.Typed

import System.Environment (getProgName, getArgs)
import System.Exit

type PKS = PubKey 'Sign HBS2Basic

{- HLINT ignore "Functor law" -}

deriving instance Data (RefLogKey HBS2Basic)
deriving instance Data (LWWRefKey HBS2Basic)

data GitRepoRefFact =
  GitRepoFact1
  { gitLwwRef :: LWWRefKey HBS2Basic
  , gitLwwSeq :: Word64
  , gitRefLog :: RefLogKey HBS2Basic
  }
  deriving stock (Generic,Data)

data GitRepoHeadFact =
  GitRepoHeadFact1
  { gitRepoHeadRef   :: HashRef
  , gitRepoName      :: Text
  , gitRepoBrief     :: Text
  , gitRepoEncrypted :: Bool
  }
  deriving stock (Generic,Data)


data GitRepoFacts =
      GitRepoRefFact  GitRepoRefFact
    | GitRepoHeadFact HashRef GitRepoHeadFact
    deriving stock (Generic,Data)


instance Serialise GitRepoRefFact
instance Serialise GitRepoHeadFact
instance Serialise GitRepoFacts

instance Pretty GitRepoFacts  where
  pretty (GitRepoRefFact x)    = pretty x
  pretty (GitRepoHeadFact ha x) = pretty ("gitrpoheadfact",ha,x)

instance Pretty GitRepoRefFact where
  pretty (GitRepoFact1{..}) =
    parens ( "gitrepofact1" <+>  hsep [pretty gitLwwRef, pretty gitLwwSeq, pretty gitRefLog])

instance Pretty GitRepoHeadFact where
  pretty (GitRepoHeadFact1{..}) =
    parens ( "gitrepoheadfact1" <+>  hsep [pretty gitRepoHeadRef])


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

          let enc = isJust _repoHeadGK0
          let name  = Text.take 256 $  _repoHeadName
          let brief = Text.take 1024 $ _repoHeadBrief
          let manifest = _repoManifest

          let repoFactHash = hashObject @HbSync (serialise what) & HashRef

          let f1 = GitRepoRefFact what
          let f2 = GitRepoHeadFact
                      repoFactHash
                      (GitRepoHeadFact1 rhh name brief enc)

          lift $ S.yield f1
          lift $ S.yield f2

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


runDump :: forall m  . MonadUnliftIO m
        => PKS
        -> m ()

runDump pks = do
  self <- liftIO getProgName

  debug $ "fucking dump!" <+> pretty self

  let cmd = proc self ["pipe", "-r", show (pretty (AsBase58 pks))]
              & setStdin createPipe
              & setStdout createPipe

  -- let w

  flip runContT pure do

    -- p <- ContT $ withProcessWait cmd
    p <- lift $ startProcess cmd -- ContT $ withProcessWait cmd

    let ssin = getStdin p
    let sout = getStdout p
    client <- newMessagingPipe (sout,ssin) -- ,sout)

    -- forever do
    --   liftIO $ LBS.hPutStr ssin "\x10 AAAAAAAAAAAAAAAAAAAAAAA\r\n"
    --   hFlush ssin
    --   pause @'Seconds 1

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
    let chan  = _refchanId env
    let rchanAPI = _refchanAPI env
    let sto = _storage env

    runMaybeT do

      rv <- lift (callRpcWaitMay @RpcRefChanGet (TimeoutSec 1) rchanAPI chan)
              >>= toMPlus >>= toMPlus

      debug $ "AAAAAA" <+> pretty rv

      facts <- S.toList_ do
        walkMerkle @[HashRef] (fromHashRef rv) (getBlock sto) $ \case
          Left{} -> pure ()
          Right txs -> do
            for_ txs $ \htx -> void $ runMaybeT do
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
               >>= toMPlus
               >>= lift . S.yield

      let rf  = [ (HashRef (hashObject $ serialise f), f)
                | f@GitRepoFact1{} <-  universeBi facts
                ] & HM.fromListWith (\v1 v2 -> if gitLwwSeq v1 > gitLwwSeq v2 then v1 else v2)


      let rhf = [ (h,f) | (GitRepoHeadFact h f) <- universeBi facts ]
                & HM.fromList

      items <- S.toList_ $ for_ (HM.toList rf) $ \(k, GitRepoFact1{..}) -> do
        let d = HM.lookup k rhf
        let nm    = maybe "" gitRepoName d
        let brief = maybe "" gitRepoBrief d

        S.yield $ object [ "item_id"    .= show (pretty gitLwwRef)
                         , "item_title" .= show (pretty nm)
                         , "item_brief" .= show (pretty brief)
                         ]

      let root = object [ "items" .= items
                        , "state" .= show (pretty rv)
                        ]

      pure $ A.encodePretty root

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

  -- liftIO $ hSetBuffering stdin NoBuffering

  -- liftIO $ LBS.getContents >>= LBS.hPutStr stderr
  -- forever (pause @'Seconds 10)

  flip runContT pure do

    server  <- newMessagingPipe (stdin,stdout)

    void $ ContT $ withAsync $ runMessagingPipe server

    -- make server protocol responder
    -- void $ ContT $ withAsync $ flip
    lift $ flip runReaderT server do
        runProto @PIPE
          [ makeResponse (makeServer @BrowserPluginAPI)
          ]

