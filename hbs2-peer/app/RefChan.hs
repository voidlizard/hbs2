{-# Language AllowAmbiguousTypes #-}
{-# Language TemplateHaskell #-}
module RefChan (
    RefChanWorkerEnv(..)
  , refChanWorkerEnvHeadQ
  , refChanWorkerEnvDownload
  , refChanOnHeadFn
  , refChanWriteTranFn
  , refChanValidateTranFn
  , refChanNotifyRelyFn
  , refChanWorker
  , runRefChanRelyWorker
  , refChanWorkerEnv
  , refChanNotifyOnUpdated
  ) where

import HBS2.Prelude.Plated

import HBS2.Actors.Peer
import HBS2.Base58
import HBS2.Hash
import HBS2.Clock
import HBS2.Data.Detect
import HBS2.Defaults
import HBS2.Data.Types.Refs
import HBS2.Data.Types.SignedBox
import HBS2.Events
import HBS2.Merkle
import HBS2.Net.Auth.Credentials
import HBS2.Net.Messaging.Unix
import HBS2.Net.Proto
import HBS2.Net.Proto.Definition()
import HBS2.Net.Proto.Peer
import HBS2.Net.Proto.RefChan
import HBS2.Net.Proto.Sessions
import HBS2.Storage

import HBS2.System.Logger.Simple

import PeerTypes
import PeerConfig
import BlockDownload
import Brains

import Data.Dynamic
import Codec.Serialise
import Control.Concurrent.STM (flushTQueue)
import Control.Exception ()
import Control.Monad.Except ()
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.Cache (Cache)
import Data.Cache qualified as Cache
import Data.ByteString (ByteString)
import Data.Either
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.Heap ()
import Data.Coerce
-- import Data.Heap qualified as Heap
import Data.List qualified as List
import Data.Maybe
import Data.Text qualified as Text
import Lens.Micro.Platform
import Data.Generics.Product
import UnliftIO

import Streaming.Prelude qualified as S
import Streaming()

{- HLINT ignore "Use newtype instead of data" -}

data DataNotReady = DataNotReady deriving (Show)

instance Exception DataNotReady

type OnDownloadComplete = HashRef -> IO ()

data RefChanValidator =
  RefChanValidator
  { rcvInbox    :: TQueue (RefChanValidate UNIX, MVar (RefChanValidate UNIX))
  , rcvAsync    :: Async ()
  }


data RefChanNotifier =
  RefChanNotifier
  { rcnPeer     :: Peer UNIX
  , rcnInbox    :: TQueue (RefChanNotify UNIX)
  , rcnAsync    :: Async ()
  }

data RefChanWorkerEnv e =
  RefChanWorkerEnv
  { _refChanWorkerConf            :: PeerConfig
  , _refChanPeerEnv               :: PeerEnv e
  , _refChanWorkerEnvDEnv         :: DownloadEnv e
  , _refChanWorkerEnvHeadQ        :: TQueue (RefChanId e, RefChanHeadBlockTran e)
  , _refChanWorkerEnvDownload     :: TVar (HashMap HashRef (RefChanId e, (TimeSpec, OnDownloadComplete)))
  , _refChanWorkerEnvNotify       :: TVar (HashMap (RefChanId e) ())
  , _refChanWorkerEnvWriteQ       :: TQueue HashRef
  , _refChanWorkerValidators      :: TVar (HashMap (RefChanId e) RefChanValidator)
  -- FIXME: peer-addr-to-have-multiple-actors-on-single-box
  --   нужно ключом держать Peer e (SockAddr)
  --   что бы можно было завести несколько акторов на одном
  --   боксе в целях отладки.
  , _refChanWorkerNotifiers       :: TVar (HashMap (RefChanId e) [RefChanNotifier])
  , _refChanWorkerNotifiersInbox  :: TQueue (RefChanNotify e) -- ^ to rely messages from clients to gossip
  , _refChanWorkerNotifiersDone   :: Cache (Hash HbSync) ()
  , _refChanWorkerLocalRelyDone   :: Cache (Peer UNIX, Hash HbSync) ()
  }

makeLenses 'RefChanWorkerEnv

refChanWorkerEnv :: forall m e . (MonadIO m, ForRefChans e)
                 => PeerConfig
                 -> PeerEnv e
                 -> DownloadEnv e
                 -> m (RefChanWorkerEnv e)

refChanWorkerEnv conf pe de = liftIO $ RefChanWorkerEnv @e conf pe de
                                      <$> newTQueueIO
                                      <*> newTVarIO mempty
                                      <*> newTVarIO mempty
                                      <*> newTQueueIO
                                      <*> newTVarIO mempty
                                      <*> newTVarIO mempty
                                      <*> newTQueueIO
                                      <*> Cache.newCache (Just defRequestLimit)
                                      <*> Cache.newCache (Just defRequestLimit)

refChanOnHeadFn :: forall e m . (ForRefChans e, MonadIO m) => RefChanWorkerEnv e -> RefChanId e -> RefChanHeadBlockTran e -> m ()
refChanOnHeadFn env chan tran = do
  atomically $ writeTQueue (view refChanWorkerEnvHeadQ env) (chan, tran)


refChanWriteTranFn :: MonadIO m => RefChanWorkerEnv e -> HashRef -> m ()
refChanWriteTranFn env href = do
  atomically $ writeTQueue (view refChanWorkerEnvWriteQ env) href

refChanValidateTranFn :: forall e m . ( MonadUnliftIO m
                                      , ForRefChans e, e ~ L4Proto
                                      , HasNonces (RefChanValidate UNIX) m
                                      )
                      => RefChanWorkerEnv e
                      -> RefChanId e
                      -> HashRef
                      -> m Bool

refChanValidateTranFn env chan htran = do
  mbv <- readTVarIO (view refChanWorkerValidators env) <&> HashMap.lookup chan
  -- отправить запрос в соответствующий... что?
  -- ждать ответа
  debug $ "VALIDATE TRAN" <+> pretty (AsBase58 chan) <+> pretty htran

  r <- maybe1 mbv (pure True) $ \(RefChanValidator q _) -> do
    answ <- newEmptyMVar
    nonce <- newNonce @(RefChanValidate UNIX)
    atomically $ writeTQueue q (RefChanValidate nonce chan (Validate @UNIX htran), answ)
    withMVar answ $ \msg -> case rcvData msg of
      Accepted{} -> pure True
      _          -> pure False


  debug $ "TRANS VALIDATION RESULT: " <+> pretty htran <+> pretty r

  pure r

-- FIXME: leak-when-block-never-really-updated
refChanNotifyOnUpdated :: (MonadIO m, ForRefChans e) => RefChanWorkerEnv e -> RefChanId e -> m ()
refChanNotifyOnUpdated env chan = do
  atomically $ modifyTVar (_refChanWorkerEnvNotify env) (HashMap.insert chan ())


refChanNotifyRelyFn :: forall e m . ( MonadUnliftIO m
                                    , ForRefChans e, e ~ L4Proto
                                    )
                      => RefChanWorkerEnv e
                      -> RefChanId e
                      -> RefChanNotify e
                      -> m ()

refChanNotifyRelyFn env chan msg@(Notify _ (SignedBox k box s)) = do
  debug "refChanNotifyRelyFn"
  -- FIXME: multiple-hash-object-msg-performance
  let h0 = hashObject @HbSync (serialise msg)
  void $ runMaybeT do
    guard =<< liftIO (Cache.lookup (view refChanWorkerNotifiersDone env) h0 <&> isNothing)
    liftIO $ Cache.insert (view refChanWorkerNotifiersDone env) h0 ()

    -- RefChanNotifier q _ <- MaybeT $ liftIO (readTVarIO (view refChanWorkerNotifiers env) <&> HashMap.lookup chan)
    notifiers <- MaybeT $ liftIO (readTVarIO (view refChanWorkerNotifiers env) <&> HashMap.lookup chan)
    forM_ notifiers $ \(RefChanNotifier _ q _)  -> do
      atomically $ writeTQueue q (Notify @UNIX chan (SignedBox k box s))


refChanAddDownload :: forall e m . ( m ~ PeerM e IO
                                   , MyPeer e
                                   )
                   => RefChanWorkerEnv e
                   -> RefChanId e
                   -> HashRef
                   -> OnDownloadComplete
                   -> m ()

refChanAddDownload env chan r onComlete = do
  penv <- ask
  t <- getTimeCoarse
  withPeerM penv $ withDownload (_refChanWorkerEnvDEnv env)
                 $ processBlock @e (fromHashRef r)

  atomically $ modifyTVar (view refChanWorkerEnvDownload env) (HashMap.insert r (chan,(t, onComlete)))



data NotifyEnv =
  NotifyEnv
  { _notifyClient :: Fabriq UNIX
  , _notifySelf   :: Peer UNIX
  }

newtype NotifyProtoM m a = NotifyProto { fromNotifyProto :: ReaderT NotifyEnv m a }
                           deriving newtype ( Functor
                                            , Applicative
                                            , Monad
                                            , MonadIO
                                            , MonadUnliftIO
                                            , MonadReader NotifyEnv
                                            , MonadTrans
                                            )


runNotifyProtoM :: (MonadIO m, PeerMessaging UNIX) => MessagingUnix -> NotifyProtoM m a -> m a
runNotifyProtoM bus m = runReaderT (fromNotifyProto m) (NotifyEnv (Fabriq bus) (msgUnixSelf bus))


instance Monad m => HasFabriq UNIX (NotifyProtoM m) where
  getFabriq = asks _notifyClient

instance Monad m => HasOwnPeer UNIX (NotifyProtoM m) where
  ownPeer = asks _notifySelf

refChanNotifyRpcProto :: forall e m . ( MonadIO m
                                      , Request e (RefChanNotify e) m
                                      -- , HasPeerNonce UNIX m
                                      , e ~ UNIX
                                      -- , m ~ PeerM e IO
                                      )
                    => RefChanWorkerEnv L4Proto
                    -> RefChanNotify e
                    -> m ()

refChanNotifyRpcProto env msg@(ActionRequest chan action) = do

  let penv = view refChanPeerEnv env

  case action of
    RefChanAnnounceBlock h -> do
      debug $ "RefChanNotify: RefChanAnnounceBlock" <+> pretty h

      liftIO $ withPeerM penv $ do
        sto <- getStorage
        sz' <- liftIO $ hasBlock sto (fromHashRef h)
        maybe1 sz' none $ \sz -> do
          ann <- simpleBlockAnnounce @L4Proto sz (fromHashRef h)
          gossip ann

      pure ()

    RefChanFetch h -> do
      debug $ "RefChanNotify: RefChanFetch" <+> pretty h

      liftIO $ withPeerM penv $ do
        refChanAddDownload env chan h (const $ pure ())

    where
      proto = Proxy @(RefChanNotify e)

refChanNotifyRpcProto env msg@(Notify chan (SignedBox pk box si)) = do
  debug "GOT MESSAGE FROM CLIENT"
  atomically $ writeTQueue (view refChanWorkerNotifiersInbox env) (Notify @L4Proto chan (SignedBox pk box si))
  -- тут мы должны переслать всем, кроме отправителя

  let h0 = hashObject @HbSync (serialise msg)

  -- FIXME: squash-this-copypaste
  void $ runMaybeT do
    notifiers <- MaybeT $ liftIO (readTVarIO (view refChanWorkerNotifiers env) <&> HashMap.lookup chan)
    forM_ notifiers $ \(RefChanNotifier peer q _)  -> do
      let lkey  = (peer, h0)
      -- guard =<< liftIO (Cache.lookup (view refChanWorkerLocalRelyDone env) lkey <&> isNothing)
      -- liftIO $ Cache.insert (view refChanWorkerLocalRelyDone env) lkey ()
      atomically $ writeTQueue q msg

refChanWorkerInitNotifiers :: forall e  m . ( MonadIO m
                                             , MonadUnliftIO m
                                             , MyPeer e
                                             -- , ForRefChans e
                                             -- , ForRefChans UNIX
                                             -- , m ~ PeerM e IO
                                             , e ~ L4Proto
                                             )
                           => RefChanWorkerEnv e
                           -> m ()


refChanWorkerInitNotifiers env = do
  debug "refChanWorkerInitNotifiers"

  let (PeerConfig syn) = view refChanWorkerConf env

  let notifiers = [ mkV rc x | ListVal [ SymbolVal "notify"
                                       , SymbolVal "refchan"
                                       , LitStrVal rc
                                       , ListVal [ SymbolVal "socket", SymbolVal "unix", LitStrVal x  ]
                                       ] <- syn
                   ] & catMaybes

  forM_ notifiers $ \(rc, sa) -> do
    debug $ "** NOTIFIER FOR" <+> pretty (AsBase58 rc, sa)

    q <- newTQueueIO
    val <- async $ liftIO $ notifierThread rc sa q

    let rcn = RefChanNotifier (fromString sa) q val

    atomically $ modifyTVar (_refChanWorkerNotifiers env) (HashMap.insertWith (<>) rc [rcn])

  where
    mkV :: Text -> Text -> Maybe (RefChanId e, String)
    mkV rc x = (,Text.unpack x) <$> fromStringMay @(RefChanId e) (Text.unpack rc)


    notifierThread _ sa q = do

      debug $ ">>> NOTIFIER THREAD FOR" <+> pretty sa

      client <- newMessagingUnix False 1.0 sa
      msg <- async $ runMessagingUnix client

      runNotifyProtoM client do
        proto <- async $ runProto [ makeResponse (refChanNotifyRpcProto env) ]

        forever do
           req <- atomically $ readTQueue q
           debug "Rely notification request"
           request @UNIX (fromString sa) req

        wait proto

      mapM_ wait [msg]


data ValidateEnv =
  ValidateEnv
  { _validateClient :: Fabriq UNIX
  , _validateSelf   :: Peer UNIX
  }

newtype ValidateProtoM m a = ValidateProto { fromValidateProto :: ReaderT ValidateEnv m a }
                             deriving newtype ( Functor
                                              , Applicative
                                              , Monad
                                              , MonadIO
                                              , MonadUnliftIO
                                              , MonadReader ValidateEnv
                                              , MonadTrans
                                              )


runValidateProtoM :: (MonadIO m, PeerMessaging UNIX) => MessagingUnix -> ValidateProtoM m a -> m a
runValidateProtoM tran m = runReaderT (fromValidateProto m) (ValidateEnv (Fabriq tran) (msgUnixSelf tran))


instance Monad m => HasFabriq UNIX (ValidateProtoM m) where
  getFabriq = asks _validateClient

instance Monad m => HasOwnPeer UNIX (ValidateProtoM m) where
  ownPeer = asks _validateSelf


refChanValidateProto :: forall e m . ( MonadIO m
                                     , Request e (RefChanValidate e) m
                                     , Response e (RefChanValidate e) m
                                     , e ~ UNIX
                                     )
                     => Cache (RefChanValidateNonce e) (MVar (RefChanValidate e))
                     -> RefChanValidate e
                     -> m ()

refChanValidateProto waiters msg = do
  debug $ "GOT ANSWER FROM VALIDATOR" <+> pretty msg
  case rcvData msg of
    Accepted h -> emitAnswer h msg
    Rejected h -> emitAnswer h msg
    _          -> none

  where
    emitAnswer h m = liftIO do
      debug $ "EMIT ANSWER" <+> pretty h
      mbAnsw <- Cache.lookup waiters (rcvNonce m)
      maybe1 mbAnsw none $ \answ -> do
        putMVar answ m

refChanWorkerInitValidators :: forall e  m . ( MonadIO m
                                             , MonadUnliftIO m
                                             -- , MyPeer e
                                             -- , ForRefChans e
                                             -- , ForRefChans UNIX
                                             -- , m ~ PeerM e IO
                                             , e ~ L4Proto
                                             )
                           => RefChanWorkerEnv e
                           -> m ()


refChanWorkerInitValidators env = do
  debug "refChanWorkerInitValidators"

  let (PeerConfig syn) = view refChanWorkerConf env

  let validators = [ mkV rc x | ListVal [ SymbolVal "validate"
                                        , SymbolVal "refchan"
                                        , LitStrVal rc
                                        , ListVal [ SymbolVal "socket", SymbolVal "unix", LitStrVal x  ]
                                        ] <- syn
                   ] & catMaybes


  forM_ validators $ \(rc, sa) -> do
    debug $ "** VALIDATOR FOR" <+> pretty (AsBase58 rc, sa)

    here <- readTVarIO (_refChanWorkerValidators env) <&> HashMap.member rc

    unless here do
      q <- newTQueueIO
      val <- async $ validatorThread rc sa q
      let rcv = RefChanValidator q val
      atomically $ modifyTVar (_refChanWorkerValidators env) (HashMap.insert rc rcv)

  where

    mkV :: Text -> Text -> Maybe (RefChanId e, String)
    mkV rc x = (,Text.unpack x) <$> fromStringMay @(RefChanId e) (Text.unpack rc)

    -- FIXME: make-thread-respawning
    validatorThread chan sa q = liftIO do
      client <- newMessagingUnix False 1.0 sa
      msg <- async $ runMessagingUnix client

      -- FIXME: hardcoded-timeout
      waiters <- Cache.newCache (Just (toTimeSpec (10 :: Timeout 'Seconds)))

      -- FIXME: hardcoded-timeout
      waiters <- Cache.newCache (Just (toTimeSpec (10 :: Timeout 'Seconds)))

      runValidateProtoM client do

        poke <- async $ forever do
                  pause @'Seconds 10
                  mv <- newEmptyMVar
                  nonce <- newNonce @(RefChanValidate UNIX)
                  atomically $ writeTQueue q (RefChanValidate @UNIX nonce chan Poke, mv)

        z <- async $ runProto
          [ makeResponse (refChanValidateProto waiters)
          ]

        forever do
           (req, answ) <- atomically $ readTQueue q
           case rcvData req of
            Validate href -> do
              debug $ "DO REQUEST VALIDATE" <+> pretty href <+> pretty sa
              liftIO $ Cache.insert waiters (rcvNonce req) answ
              let pa = fromString sa
              request pa req

            Poke{} -> do
              debug "DO SEND POKE"
              let pa = fromString sa
              request pa req

            Poke{} -> do
              debug "DO SEND POKE"
              let pa = fromString sa
              pure ()
              --
              -- request pa req

            _ -> pure ()


        (_, r) <- waitAnyCatch [z,poke]
        debug $ "SOMETHING WRONG:" <+> viaShow r

      cancel msg
      warn $ "validatorThread is terminated for some reasons" <+> pretty (AsBase58 chan)

runRefChanRelyWorker :: forall e  m .
                                ( MonadIO m
                                , m ~ PeerM e IO
                                , e ~ L4Proto
                                )
             => RefChanWorkerEnv e
             -> RefChanAdapter e (ResponseM e m)
             -> IO ()

runRefChanRelyWorker env adapter = liftIO $ forever do
  withPeerM (view refChanPeerEnv env) do
    me <- ownPeer @e
    -- FIXME: use-bounded-queue-ASAP
    mess <- atomically $ readTQueue (view refChanWorkerNotifiersInbox env)
    runResponseM me $ do
      refChanNotifyProto True adapter mess

refChanWorker :: forall e s m . ( MonadIO m
                                , MonadUnliftIO m
                                , MyPeer e
                                , HasStorage m
                                , Request e (RefChanHead e) m
                                , Request e (RefChanRequest e) m
                                , Sessions e (KnownPeer e) m
                                , Signatures s
                                , s ~ Encryption e
                                , IsRefPubKey s
                                -- , Pretty (AsBase58 (PubKey 'Sign s))
                                , ForRefChans e
                                , EventListener e (RefChanRound e) m
                                , EventListener e (RefChanRequest e) m
                                , Sessions e (RefChanRound e) m
                                , m ~ PeerM e IO
                                , e ~ L4Proto
                                )
             => RefChanWorkerEnv e
             -> SomeBrains e
             -> m ()

refChanWorker env brains = do

  penv <- ask

  mergeQ <- newTQueueIO

  -- FIXME: resume-on-exception
  hw <- async (refChanHeadMon penv)

  -- FIXME: insist-more-during-download
  --  что-то частая ситуация, когда блоки
  --  с трудом докачиваются. надо бы
  --  разобраться. возможно переделать
  --  механизм скачивания блоков
  downloads <- async monitorHeadDownloads

  polls <- async refChanPoll

  wtrans <- async refChanWriter

  cleanup1 <- async cleanupRounds

  merge <- async (logMergeProcess env mergeQ)

  sto <- getStorage

  liftIO $ refChanWorkerInitValidators env
  liftIO $ refChanWorkerInitNotifiers  env

  subscribe @e RefChanRequestEventKey $ \(RefChanRequestEvent chan val) -> do
    debug $ "RefChanRequestEvent" <+> pretty (AsBase58 chan) <+> pretty val

    h <- liftIO $ getRef sto (RefChanLogKey @s chan)

    -- игнорируем, если синхронно
    unless ((HashRef <$> h) == Just val) do

      refChanAddDownload env chan val $ \href -> do
        debug $ "BLOCK DOWNLOADED" <+> pretty href
        atomically $ writeTQueue mergeQ (chan, href)

    atomically $ writeTQueue mergeQ (chan, val)

  forever do
   pause @'Seconds 10
   debug "I'm refchan worker"

  mapM_ waitCatch [hw,downloads,polls,wtrans,merge,cleanup1]

  where

    cleanupRounds = do

      rounds <- newTVarIO HashSet.empty

      subscribe @e RefChanRoundEventKey $ \(RefChanRoundEvent rcrk) -> do
        atomically $ modifyTVar rounds (HashSet.insert rcrk)
        debug $ "ON ROUND STARTED" <+> pretty rcrk

      forever do
        -- FIXME: use-polling-function-and-respect-wait
        pause @'Seconds 10

        now <- getTimeCoarse
        xs <- readTVarIO rounds <&> HashSet.toList

        forM_ xs $ \x -> do

          void $ runMaybeT do
            se <- MaybeT $ find @e x id

            closed <- readTVarIO (view refChanRoundClosed se)
            trans <- atomically $ readTVar (view refChanRoundTrans se) <&> HashSet.toList

            let ttl = view refChanRoundTTL se

            when (closed || ttl <= now) do
              lift $ expire x

              when closed  do
                forM_ trans $ \t -> do
                 debug $ "WRITING TRANS" <+> pretty t
                 lift $ refChanWriteTranFn env t

              atomically $ modifyTVar rounds (HashSet.delete x)
              debug $ "CLEANUP ROUND" <+> pretty x

    refChanWriter = do
      sto <- getStorage
      forever do
        pause @'Seconds 1

        _ <- atomically $ peekTQueue (view refChanWorkerEnvWriteQ env)

        htrans <- liftIO $ atomically $ flushTQueue (view refChanWorkerEnvWriteQ env)

        trans <- forM htrans $ \h -> runMaybeT do
          blk <- MaybeT $ liftIO (getBlock sto (fromHashRef h))
          upd <- MaybeT $ pure $ deserialiseOrFail @(RefChanUpdate e) blk  & either (const Nothing) Just

          case upd of
            Propose chan _ -> pure (RefChanLogKey @(Encryption e) chan, h)
            Accept chan _  -> pure (RefChanLogKey @(Encryption e) chan, h)

        let byChan = HashMap.fromListWith (<>) [ (x, [y]) | (x,y) <- catMaybes trans ]

        -- FIXME: process-in-parallel
        forM_ (HashMap.toList byChan) $ \(c,new) -> do
          mbLog <- liftIO $ getRef sto c

          hashes <- maybe1 mbLog (pure mempty) $ readLog (getBlock sto) . HashRef

          -- FIXME: might-be-problems-on-large-logs
          let hashesNew = HashSet.fromList (hashes <> new) & HashSet.toList

          -- -- FIXME: remove-chunk-num-hardcode
          let pt = toPTree (MaxSize 256) (MaxNum 256) hashesNew

          nref <- makeMerkle 0 pt $ \(_,_,bss) -> void $ liftIO $ putBlock sto bss
          liftIO $ updateRef sto c nref

          debug $ "REFCHANLOG UPDATED:" <+> pretty c <+> pretty nref

    refChanPoll = do

      let listRefs = listPolledRefs @e brains "refchan" <&> fmap (over _2 ( (*60) . fromIntegral) )

      polling (Polling 5 5) listRefs $ \ref -> do
        debug $ "POLLING REFCHAN" <+> pretty (AsBase58 ref)
        broadCastMessage (RefChanGetHead @e ref)
        broadCastMessage (RefChanRequest @e ref)

    monitorHeadDownloads = forever do
      pause @'Seconds 1
      all <- atomically $ readTVar (view refChanWorkerEnvDownload env) <&> HashMap.toList

      now <- getTimeCoarse

      -- FIXME: change-to-polling-functions
      -- FIXME: consider-timeouts-or-leak-is-possible
      rest <- forM all $ \(r,item@(chan,(t,onComplete))) -> do
                here <- checkDownloaded r
                if here then do
                  liftIO $ onComplete r
                  -- refChanOnHeadFn env chan (RefChanHeadBlockTran r)
                  pure mempty
                else do
                  -- FIXME: fix-timeout-hardcode
                  let expired = realToFrac (toNanoSecs $ now - t) / 1e9 > 600
                  if expired then pure mempty else pure [(r,item)]

      atomically $ writeTVar (view refChanWorkerEnvDownload env) (HashMap.fromList (mconcat rest))

    -- FIXME: in-parallel?
    refChanHeadMon pe = liftIO $ withPeerM pe do

      forever do
        (chan, RefChanHeadBlockTran hr) <- atomically $ readTQueue (view refChanWorkerEnvHeadQ env)

        here <- checkDownloaded hr

        if not here then do
          refChanAddDownload env chan hr (withPeerM pe . refChanOnHeadFn env chan . RefChanHeadBlockTran)
          trace $ "BLOCK IS NOT HERE" <+> pretty hr
        else do
          sto <- getStorage
          trace $ "BLOCK IS HERE" <+> pretty hr
          -- читаем блок
          lbs <- readBlobFromTree (getBlock sto) hr <&> fromMaybe mempty
          let what = unboxSignedBox @(RefChanHeadBlock e) @e lbs

          notify <- atomically $ do
                        no <- readTVar (_refChanWorkerEnvNotify env) <&> HashMap.member chan
                        modifyTVar (_refChanWorkerEnvNotify env) (HashMap.delete chan)
                        pure no

          case what of
            Nothing  -> err $ "malformed head block" <+> pretty hr

            Just (pk,blk) | pk == chan ->  do
              let rkey = RefChanHeadKey @s pk

              debug $ "Good head block" <+> pretty hr <+> "processing..."

              ourVersion <- runMaybeT do

                cur <- MaybeT $ liftIO $ getRef sto rkey

                lbss <- MaybeT $ readBlobFromTree (getBlock sto) (HashRef cur)

                (_, blkOur) <- MaybeT $ pure $ unboxSignedBox @(RefChanHeadBlock e) @e lbss

                pure $ view refChanHeadVersion blkOur

              let v0 = fromMaybe 0 ourVersion
              let v1 = view refChanHeadVersion blk

              if v1 > v0 then do
                debug $ "UPDATING HEAD BLOCK" <+> pretty (v1, v0)
                liftIO $ updateRef sto rkey (fromHashRef hr)
                -- если это мы сами его обновили - то неплохо бы
                -- всем разослать уведомление. А как?
                --
                -- TODO: update-acl-here

                forM_ (HashMap.keys $ view refChanHeadPeers blk) $ \pip -> do
                  debug $ "ADD PEER ACL" <+> pretty (AsBase58 chan) <+> pretty(AsBase58 pip)

                forM_ (view refChanHeadAuthors blk) $ \au -> do
                  debug $ "ADD AUTHOR ACL" <+> pretty (AsBase58 chan) <+> pretty(AsBase58 au)

                when notify do
                  debug $ "NOTIFY-ALL-HEAD-UPDATED" <+> pretty (AsBase58 pk) <+> pretty hr
                  broadCastMessage (RefChanHead @e pk (RefChanHeadBlockTran hr))

              else do
                debug $ "LEAVING HEAD BLOCK" <+> pretty (v1, v0)

            _ -> debug "not subscribed to this refchan"

          pure ()
          -- распаковываем блок
          -- вытаскиваем ключ из блока?

        pure ()

data MergeEnv e =
  MergeEnv
  { mergeSto   :: AnyStorage
  , mergeHeads :: TVar (HashMap HashRef (RefChanHeadBlock e) )
  }


-- FIXME: possible-performance-issues
--   Выглядит довольно медленно. Вероятно,
--   можно быстрее.
--   В частности, кэшировать уже обработанные логи
logMergeProcess :: forall e s m . ( MonadUnliftIO m
                                  , MyPeer e
                                  , ForRefChans e
                                  , HasStorage m
                                  , Signatures s
                                  , Pretty (AsBase58 (PubKey 'Sign s))
                                  , s ~ Encryption e
                                  , m ~ PeerM e IO
                                  )
                => RefChanWorkerEnv e
                -> TQueue (RefChanId e, HashRef)
                -> m ()

logMergeProcess env q = do

  sto <- getStorage

  menv <- MergeEnv sto <$> newTVarIO mempty

  forever do
    -- FIXME: fix-hardcoded-timeout
    pause @'Seconds 1
    _ <- atomically $ peekTQueue q
    logs <- liftIO $ atomically $ flushTQueue q

    let byChan = HashMap.fromListWith (<>) [ (x,[y]) | (x,y) <- logs ]
                     & HashMap.toList
                     & fmap (over _2 List.nub)

    -- FIXME: in-parallel
    mapM_ (logMergeChan menv sto) byChan

  where

    getHead :: MergeEnv e
            -> HashRef
            -> m (Maybe (RefChanHeadBlock e))

    getHead e h = do

      let sto = mergeSto e
      hd <- readTVarIO (mergeHeads e) <&> HashMap.lookup h

      here <- liftIO $ hasBlock sto (fromHashRef h) <&> isJust

      unless here do
        warn $ "refchan. head is missed:" <+> pretty h
        pure ()

      case hd of
        Just x -> pure (Just x)
        Nothing -> runMaybeT do
          hdblob <- MaybeT $ readBlobFromTree ( liftIO . getBlock sto ) h
          (_, headblk) <- MaybeT $ pure $ unboxSignedBox @(RefChanHeadBlock e) @e hdblob
          atomically $ modifyTVar (mergeHeads e) (HashMap.insert h headblk)
          pure headblk

    downloadMissedHead :: AnyStorage -> RefChanId e -> HashRef -> m ()
    downloadMissedHead sto chan headRef = do
      penv <- ask
      here <- liftIO $ hasBlock sto (fromHashRef headRef) <&> isJust
      unless here do
        refChanAddDownload env chan headRef (withPeerM penv . refChanOnHeadFn env chan . RefChanHeadBlockTran)

    logMergeChan menv sto (chan, logs) = do

      penv <- ask

      let readFn = getBlock sto

      void $ runMaybeT do

        let chanKey = RefChanLogKey @s chan

        -- FIXME: wont-work-if-no-reference-yet
        --   не сработает если ссылка новая
        (mergeSet, merge) <- liftIO (getRef sto chanKey) >>= \case
                             Nothing -> do
                               new <- mconcat <$> mapM (lift . readLog readFn) logs
                               pure (HashSet.fromList new, not (List.null new))

                             Just h -> do
                               current <- lift $ readLog readFn (HashRef h) <&> HashSet.fromList
                               new <- mconcat <$> mapM (lift . readLog readFn) (filter (/= HashRef h) logs)
                               let mergeSet = HashSet.fromList new <> current
                               pure (mergeSet, not (List.null new))
        guard merge

        -- итак, тут приехал весь лог, который есть у пира
        -- логично искать подтверждения только в нём. если
        -- пир принял транзы без достаточного количества
        -- подтверждений, то он сам лошара.
        -- каждую транзу рассматриваем один раз, если
        -- она смержена.
        -- если она не смержена --- может быть, надо её
        -- в какой-то reject список заносить

        -- распаковать, отсортировать по головам сначала
        -- потом бежим по головам, достаём головы
        -- проверяем acl-ы на соответствие историческим головам
        -- потом связываем каждый accept с соответствующим propose
        -- потом считаем количество accept для каждого propose
        -- потом, если всё ок -- пишем accept-ы и propose-ы у которых
        -- больше quorum подтверждений для актуальной головы

        let mergeList = HashSet.toList mergeSet

        -- если какие-то транзакции отсутствуют - пытаемся их скачать
        -- и надеемся на лучшее (лог сойдется в следующий раз)
        forM_ mergeList $ \href -> do
           mblk <- liftIO $ getBlock sto (fromHashRef href)
           maybe1 mblk (lift $ refChanAddDownload env chan href dontHandle) dontHandle

        r <- forM mergeList $ \href -> runMaybeT do

               blk <- MaybeT $ liftIO $ getBlock sto (fromHashRef href)

               tran <- MaybeT $ pure $ deserialiseOrFail @(RefChanUpdate e) blk
                                        & either (const Nothing) Just

               case tran of
                  Propose _ box -> do
                    (pk, ProposeTran headRef box) <- MaybeT $ pure $ unboxSignedBox0 box
                    (ak, _) <- MaybeT $ pure $ unboxSignedBox0 box

                    lift $ lift $ downloadMissedHead sto chan headRef

                    hd <- MaybeT $ lift $ getHead menv headRef

                    let quo = view refChanHeadQuorum hd & fromIntegral
                    guard $ checkACL hd (Just pk) ak
                    pure [(href, (quo,mempty))]

                  Accept  _ box -> do
                    (pk, AcceptTran headRef hashRef) <- MaybeT $ pure $ unboxSignedBox0 box

                    lift $ lift $ downloadMissedHead sto chan headRef

                    hd <- MaybeT $ lift $ getHead menv headRef
                    let quo = view refChanHeadQuorum hd & fromIntegral
                    guard $ HashMap.member pk (view refChanHeadPeers hd)
                    pure [(hashRef, (quo,[href]))]

        let merge1 (q1, hs1) (q2, hs2) = (max q1 q2, List.nub (hs1 <> hs2) )

        let permitted = HashMap.fromListWith merge1 (mconcat (catMaybes r))
                            & HashMap.toList

        new <- S.toList_ do
                  forM_ permitted $ \(prop, (qx, accs)) -> do
                    when (length accs >= qx) do
                      S.yield prop
                      S.each accs

        debug $ "new trans to merge" <+> pretty (AsBase58 chan) <+> pretty (length new)

        forM_ new $ \tnew -> do
          debug $ "TRANS TO MERGE" <+> pretty tnew

        let merged = HashSet.fromList new & HashSet.toList

        let pt = toPTree (MaxSize 256) (MaxNum 256) merged

        unless (List.null merged) do
          liftIO do
            nref <- makeMerkle 0 pt $ \(_,_,bss) -> do
              void $ putBlock sto bss

            debug $ "NEW REFCHAN" <+> pretty chanKey <+> pretty  nref

            updateRef sto chanKey nref


