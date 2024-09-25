{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
module HBS2.Net.Proto.Notify where

import HBS2.Actors.Peer
import HBS2.Clock
import HBS2.Net.Proto.Types
import HBS2.Prelude.Plated
import HBS2.Net.Messaging.Unix (UNIX)

import HBS2.System.Logger.Simple

import Data.Function
import Codec.Serialise
import Control.Monad
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Kind
import Data.List qualified as List
import Data.Word
import Control.Concurrent.STM (flushTQueue)
import Data.Maybe
import Data.Either
import UnliftIO


instance (HasProtocol UNIX (NotifyProto ev0 UNIX)) => HasTimeLimits UNIX (NotifyProto ev0 UNIX) IO where
  tryLockForPeriod _ _ = pure True

-- вряд ли у нас будут причины
-- иметь разные типы для NotifyHandle
type NotifyHandle = Word64

type ForNotify ev = ( Serialise (NotifyKey ev)
                    , Serialise (NotifyData ev)
                    , Serialise NotifyHandle
                    , Hashable  (NotifyKey ev)
                    , Eq (NotifyKey ev)
                    )

data family NotifyKey ev    :: Type
data family NotifyData ev   :: Type

class (ForNotify ev) => NotifySource ev source where

  startNotify :: forall m0 . MonadIO m0
              => source
              -> NotifyKey ev
              -> ( forall m . MonadIO m => NotifyHandle -> NotifyData ev -> m () )
              -> m0 NotifyHandle

  stopNotify  :: forall m . MonadIO m => source -> NotifyHandle -> m ()

-- NOTE: on-notify-proto
--  можно было бы разнести на несколько (два) разных
--  протокола, но тогда у нас будет
--  2*makeServer и 2*makeClient
--  так как каждый ev - это и есть отдельный протокол?
--  Или нет? Наверное, да, так как для каждого типа
--  эвента свой какой-то код их генерирует
data NotifyProto ev e =
    NotifyPing
  | NotifyPong
  | NotifyWant   Word64 (NotifyKey ev)
  | NotifyGiven  Word64 NotifyHandle
  | NotifyAlive  NotifyHandle
  | Notify       NotifyHandle (NotifyEvent ev)
  | NotifyBye    NotifyHandle
  | NotifyError  NotifyErr
  deriving stock (Generic)

data NotifyEvent ev =
  NotifyEvent
  { notifyKey  :: NotifyKey ev
  , notifyData :: NotifyData ev
  }
  deriving stock Generic

data NotifyErr = NotifyErrUnexpected
                 deriving stock (Generic, Show, Typeable)

instance Serialise NotifyErr

instance ForNotify ev => Serialise (NotifyEvent ev)

instance ForNotify ev => Serialise (NotifyProto ev e)

data NotifyEnv ev src e =
  NotifyEnv
  { notifySource :: src
  , notifyAlive  :: TVar (HashMap NotifyHandle TimeSpec)
  , notifyWho    :: TVar (HashMap NotifyHandle (Peer e))
  , notifyQ      :: TQueue (NotifyHandle, Peer e, NotifyEvent ev)
  }

newNotifyEnvServer :: ( NotifySource ev src
                      , MonadIO m
                      )
                   => src
                   -> m (NotifyEnv ev src e)
newNotifyEnvServer src = NotifyEnv src <$> newTVarIO mempty
                                       <*> newTVarIO mempty
                                       <*> newTQueueIO

makeNotifyServer :: forall ev src e m proto . ( MonadIO m
                                              , Response e proto m
                                              , NotifySource ev src
                                              , HasDeferred proto e m
                                              , Pretty (Peer e)
                                              , proto ~ NotifyProto ev e
                                              )
                => NotifyEnv ev src e
                -> NotifyProto ev e
                -> m ()

makeNotifyServer (NotifyEnv{..}) what = do

  case what of
    NotifyWant rn key -> deferred @proto do

      debug "SERVER: NotifyWant"

      who <- thatPeer @(NotifyProto ev e)

      hndl <- startNotify @ev @src @m notifySource key $ \ha d -> do
               atomically $ writeTQueue notifyQ (ha, who, NotifyEvent key d)

      now <- getTimeCoarse

      atomically $ do
        modifyTVar notifyAlive (HashMap.insert hndl now)
        modifyTVar notifyWho (HashMap.insert hndl who)

      debug $ "SEND GIVEN TO" <+> viaShow hndl <+> pretty who
      response (NotifyGiven @ev @e rn hndl)

    NotifyBye ha -> do
      trace $ "SERVER: NotifyBye" <+> pretty ha
      atomically $ modifyTVar notifyAlive (HashMap.insert ha 0)

    NotifyAlive ha -> do
      now <- getTimeCoarse
      atomically $ modifyTVar notifyAlive (HashMap.insert ha now)

    NotifyError{} -> pure ()

    NotifyPing{} -> do
      response (NotifyPong @ev @e)

    _ -> response (NotifyError @ev @e NotifyErrUnexpected)


runNotifyWorkerServer :: forall ev src e m . ( Request e (NotifyProto ev e) m
                                       , ForNotify ev
                                       , NotifySource ev src
                                       , Pretty (Peer e)
                                       , MonadUnliftIO m
                                       )
                => NotifyEnv ev src e
                -> m ()

runNotifyWorkerServer env = do

  -- FIXNE: timeout-hardcode
  let tmo = 60
  let tnano = round $ realToFrac tmo * (1e9 :: Double)

  cleanup <- async $ forever do

               trace "SERVER: CLEANUP"

               now <- getTimeCoarse

               losers <- atomically do
                  current <- readTVar (notifyAlive env)

                  pips    <- readTVar (notifyWho env) <&> HashMap.toList

                  let alive = HashMap.filter (\t -> toNanoSeconds (TimeoutTS (now - t)) < tnano ) current

                  let (winners, losers) = List.partition (\(h, _) -> HashMap.member h alive) pips

                  writeTVar (notifyAlive env) alive
                  writeTVar (notifyWho env) (HashMap.fromList winners)

                  pure losers

               trace $ "SERVER" <+> "losers" <+> pretty (length losers)

               forM_ losers $ \(h,p) -> do
                stopNotify @ev (notifySource env) h
                trace $ "SERVER: stopNotify" <+> pretty h
                request p (NotifyBye @ev @e h)

               pause @'Seconds tmo

  work <- async $ forever do
    -- TODO: several-threads
    (ha, pip, ev) <- atomically $ readTQueue (notifyQ env)
    debug $ "SENT-NOTIFY-TO" <+> pretty pip
    request pip (Notify @ev @e ha ev)

  mapM_ link  [cleanup, work]

  void $ waitAnyCancel [work, cleanup]


data NotifySinkTask ev e =
    NotifySinkSubscribe Word64 (NotifyKey ev) (TQueue NotifyHandle)
  | NotifySinkAlive     NotifyHandle
  | NotifySinkBye       NotifyHandle
  | NotifySinkPing

data NotifySink ev e =
  NotifySink
  { sinkPipeline :: TQueue (NotifySinkTask ev e)
  , sinkPong     :: TQueue ()
  , sinkPongNum  :: TVar Int
  , sinkNotify   :: TVar (HashMap NotifyHandle (TQueue (Maybe (NotifyData ev))))
  , sinkWaiters  :: TVar (HashMap Word64 (TQueue NotifyHandle))
  , sinkRnum     :: TVar Word64
  }

newNotifySink :: MonadIO m => m (NotifySink ev e)
newNotifySink = NotifySink <$> newTQueueIO
                           <*> newTQueueIO
                           <*> newTVarIO 0
                           <*> newTVarIO mempty
                           <*> newTVarIO mempty
                           <*> newTVarIO 1


runNotifyWorkerClient :: forall ev e m  . ( MonadUnliftIO m
                                          , Request e (NotifyProto ev e) m
                                          , HasOwnPeer e m
                                          , Pretty (Peer e)
                                          )
                => NotifySink ev e
                -> m ()

runNotifyWorkerClient sink = do
  let waiters = sinkWaiters sink
  pip <- ownPeer

  atomically $ writeTVar (sinkPongNum sink) 0

  fix \next -> do
    request @e pip (NotifyPing @ev @e)
    what <- race (pause @'Seconds 2) do
      debug "runNotifyWorkerClient.sendPing"
      atomically $ readTQueue (sinkPong sink)
    either (const next) (const none) what
    atomically $ modifyTVar (sinkPongNum sink) succ

  debug "runNotifyWorkerClient.run"

  forever do
    atomically (readTQueue (sinkPipeline sink)) >>= \case

      NotifySinkSubscribe r k w -> do
        atomically $ modifyTVar waiters (HashMap.insert r w)

        void $ asyncLinked $ void $ try @_ @SomeException $ do
          -- если ничего не произошло, через минуту удаляем
          pause @'Seconds 60
          atomically $ modifyTVar waiters (HashMap.delete r)

        trace $ "CLIENT:" <+> "SEND NotifySinkSubscribe" <+> pretty r
        request @e pip (NotifyWant @ev @e r k)

      NotifySinkAlive h ->
        request @e pip (NotifyAlive @ev @e h)

      NotifySinkPing -> do
        request @e pip (NotifyPing @ev @e)

      NotifySinkBye h -> do
        trace $ "CLIENT:" <+> "NotifySinkBye" <+> viaShow h
        request @e pip (NotifyBye @ev @e h)
        atomically $ modifyTVar (sinkNotify sink) (HashMap.delete h)

makeNotifyClient :: forall ev e m  . ( MonadUnliftIO m
                                     , Request e (NotifyProto ev e) m
                                     , Response e (NotifyProto ev e) m
                                     , HasOwnPeer e m
                                     , Pretty (Peer e)
                                     )
                => NotifySink ev e
                -> NotifyProto ev e
                -> m ()

makeNotifyClient sink what = do

  pip <- ownPeer
  let waiters = sinkWaiters sink

  case what of
    Notify ha (NotifyEvent _ kd) -> do
      trace $ "CLIENT: GOT NOTIFY!" <+> pretty ha
      mq <- readTVarIO (sinkNotify sink) <&> HashMap.lookup ha

      forM_ mq $ \q -> do
        r <- try @_ @SomeException $ atomically $ writeTQueue q (Just kd)

        when (isLeft r) do
          debug "LEFT writeTQueue"

        let unsbscribe _ = do
              debug "UNSUBSCRIBE!"
              -- на том конце очередь сдохла? удаляем
              request @e pip (NotifyBye @ev @e ha)
              atomically (modifyTVar (sinkNotify sink) (HashMap.delete ha))
              pure  ()

        either unsbscribe (const none) r

    NotifyGiven rn ha -> do
      trace $ "CLIENT: GOT NOTIFY GIVEN!" <+> pretty ha
      waiter <- atomically $ do
                  w <- readTVar waiters <&> HashMap.lookup rn
                  modifyTVar waiters (HashMap.delete rn)
                  pure w

      forM_ waiter $ \wa -> do
        _r  <- try @_ @SomeException $ atomically $ writeTQueue wa ha
        debug $ "NOTIFY CLIENT SUBSCRIBED" <+> viaShow rn

    NotifyBye ha  -> do
      mq <- readTVarIO (sinkNotify sink) <&> HashMap.lookup ha
      forM_ mq $ \q -> do
        void $ try @_ @SomeException $ atomically $ writeTQueue q Nothing

      void $ atomically $ modifyTVar (sinkNotify sink) $ HashMap.delete ha

    NotifyPong{} -> do
      void $ atomically $ writeTQueue (sinkPong sink) ()

    NotifyError e -> do
      err $ "*** makeNotifyClient:" <+> viaShow e

    _             -> pure ()


nextRNum :: MonadIO m => NotifySink ev e -> m Word64
nextRNum NotifySink{..} = do
  atomically $ stateTVar sinkRnum (\s -> (s, succ s))

runNotifySink :: forall ev e m . MonadUnliftIO m
              => NotifySink ev e
              -> NotifyKey ev
              -> ( NotifyData ev -> m () )
              -> m ()

runNotifySink sink k action = do

  fix \next -> do
    snum <- readTVarIO (sinkPongNum sink)
    if snum > 0 then
      none
    else do
      pause @'Seconds 0.5
      next

  ha <- fix \next -> do

          r <- race (pause @'Seconds 1) do
                  my <- nextRNum sink

                  answ <- newTQueueIO

                  atomically $ writeTQueue (sinkPipeline sink) (NotifySinkSubscribe my k answ)

                  -- ждём первый ответ, потом бы дропнуть или ЗАКРЫТЬ очередь
                  atomically $ do
                    r <- readTQueue answ
                    flushTQueue answ
                    pure r

          case r of
            Right x -> pure x
            Left{}  -> do
              debug "Retry subscribing..."
              pause @'Seconds 1
              next

  myQ <- newTQueueIO
  atomically $ modifyTVar (sinkNotify sink) (HashMap.insert ha myQ)

  w <- async $ forever do
    atomically $ writeTQueue (sinkPipeline sink) (NotifySinkAlive ha)
    pause @'Seconds 30

  -- NOTE: run-notify-sink-cleanup
  --  если нас пристрелили --- попрощаться с NotifySink хотя бы
  let cleanup = do
        trace $ "CLIENT: cleanup and exit" <+> pretty ha
        atomically $ writeTQueue (sinkPipeline sink) (NotifySinkBye ha)
        atomically $ modifyTVar (sinkNotify sink) (HashMap.delete ha)
        cancel w

  bracket_  none cleanup do
    fix $ \next -> do
      atomically (readTQueue myQ) >>= \case
        Just ev -> action ev >> next
        Nothing -> pure ()


data SomeCallback ev =
  SomeCallback { callback :: forall m . MonadIO m => NotifyHandle -> NotifyData ev -> m () }

data SomeNotifySource ev =
  SomeNotifySource
  { handleCount :: TVar NotifyHandle
  , listeners   :: TVar (HashMap (NotifyKey ev) [(NotifyHandle, SomeCallback ev)])
  }

newSomeNotifySource :: forall ev m . (MonadIO m, ForNotify ev)
                    => m (SomeNotifySource ev)

newSomeNotifySource = SomeNotifySource @ev <$> newTVarIO 1
                                           <*> newTVarIO mempty

instance ForNotify ev => NotifySource ev (SomeNotifySource ev) where

  startNotify src key fn = do
    ha <- atomically $ stateTVar (handleCount src) $ \s -> (s, succ s)
    debug $ "Start notify!"
    atomically $ modifyTVar (listeners src) (HashMap.insertWith (<>) key [(ha, SomeCallback @ev fn)])
    pure ha

  stopNotify src ha = do
    atomically do
      modifyTVar (listeners src) (HashMap.map (filter ((/= ha) . fst )))
      modifyTVar (listeners src) (HashMap.filter (not . null))

emitNotify :: forall ev m . (ForNotify ev, MonadIO m)
           => SomeNotifySource ev
           -> (NotifyKey ev, NotifyData ev)
          -> m ()

emitNotify src (k,d) = do
  who <- readTVarIO (listeners src) <&> HashMap.lookup k <&> fromMaybe mempty
  for_ who $ \(h, SomeCallback cb) -> cb h d

