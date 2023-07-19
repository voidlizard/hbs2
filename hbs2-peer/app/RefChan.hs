{-# Language AllowAmbiguousTypes #-}
{-# Language TemplateHaskell #-}
module RefChan (
    RefChanWorkerEnv(..)
  , refChanWorkerEnvHeadQ
  , refChanWorkerEnvDownload
  , refChanOnHeadFn
  , refChanWriteTranFn
  , refChanWorker
  , refChanWorkerEnv
  , refChanNotifyOnUpdated
  ) where

import HBS2.Prelude.Plated

import HBS2.Actors.Peer
import HBS2.Base58
import HBS2.Merkle
import HBS2.Clock
import HBS2.Events
import HBS2.Net.Proto.Peer
import HBS2.Net.Proto.Sessions
import HBS2.Data.Detect
import HBS2.Data.Types.Refs
import HBS2.Net.Auth.Credentials
import HBS2.Net.Proto
import HBS2.Net.Proto.RefChan
import HBS2.Net.Proto.Definition()
import HBS2.Merkle
import HBS2.Storage

import HBS2.System.Logger.Simple

import PeerTypes
import PeerConfig
import BlockDownload
import Brains

import Control.Exception ()
import Control.Monad.Except (throwError, runExceptT)
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Control.Concurrent.STM (flushTQueue)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.List qualified as List
import Data.Maybe
import Lens.Micro.Platform
-- import Data.Heap qualified as Heap
import Data.Heap ()
import Codec.Serialise
import UnliftIO

import Streaming.Prelude qualified as S
import Streaming()

{- HLINT ignore "Use newtype instead of data" -}

data DataNotReady = DataNotReady deriving (Show)

instance Exception DataNotReady

type OnDownloadComplete = HashRef -> IO ()

data RefChanWorkerEnv e =
  RefChanWorkerEnv
  { _refChanWorkerEnvDEnv     :: DownloadEnv e
  , _refChanWorkerEnvHeadQ    :: TQueue (RefChanId e, RefChanHeadBlockTran e)
  , _refChanWorkerEnvDownload :: TVar (HashMap HashRef (RefChanId e, (TimeSpec, OnDownloadComplete)))
  , _refChanWorkerEnvNotify   :: TVar (HashMap (RefChanId e) ())
  , _refChanWorkerEnvWriteQ   :: TQueue HashRef
  }

makeLenses 'RefChanWorkerEnv

refChanWorkerEnv :: forall m e . (MonadIO m, ForRefChans e)
                 => PeerConfig
                 -> DownloadEnv e
                 -> m (RefChanWorkerEnv e)

refChanWorkerEnv _ de = liftIO $ RefChanWorkerEnv @e de <$> newTQueueIO
                                                        <*> newTVarIO mempty
                                                        <*> newTVarIO mempty
                                                        <*> newTQueueIO


refChanOnHeadFn :: MonadIO m => RefChanWorkerEnv e -> RefChanId e -> RefChanHeadBlockTran e -> m ()
refChanOnHeadFn env chan tran = do
  atomically $ writeTQueue (view refChanWorkerEnvHeadQ env) (chan, tran)


refChanWriteTranFn :: MonadIO m => RefChanWorkerEnv e -> HashRef -> m ()
refChanWriteTranFn env href = do
  atomically $ writeTQueue (view refChanWorkerEnvWriteQ env) href

-- FIXME: leak-when-block-never-really-updated
refChanNotifyOnUpdated :: (MonadIO m, ForRefChans e) => RefChanWorkerEnv e -> RefChanId e -> m ()
refChanNotifyOnUpdated env chan = do
  atomically $ modifyTVar (_refChanWorkerEnvNotify env) (HashMap.insert chan ())

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

-- FIXME: slow-deep-scan-exception-seems-not-working
checkDownloaded :: forall m . (MonadIO m, HasStorage m) => HashRef -> m Bool
checkDownloaded hr = do
  sto <- getStorage
  let readBlock h = liftIO $ getBlock sto h

  result <- S.toList_ $
              deepScan ScanDeep (const $ S.yield Nothing) (fromHashRef hr) readBlock $ \ha -> do
                unless (fromHashRef hr == ha) do
                  here <- liftIO $ hasBlock sto ha
                  S.yield here

  pure $ maybe False (not . List.null)  $ sequence result


readLog :: forall m . ( MonadUnliftIO m )
        => AnyStorage
        -> HashRef
        -> m [HashRef]
readLog sto (HashRef h)  =
  S.toList_ $ do
    walkMerkle h (liftIO . getBlock sto) $ \hr -> do
      case hr of
        Left{} -> pure ()
        Right (hrr :: [HashRef]) -> S.each hrr

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
                                , Pretty (AsBase58 (PubKey 'Sign s))
                                , ForRefChans e
                                , EventListener e (RefChanRound e) m
                                , EventListener e (RefChanRequest e) m
                                , Sessions e (RefChanRound e) m
                                , m ~ PeerM e IO
                                )
             => RefChanWorkerEnv e
             -> SomeBrains e
             -> m ()

refChanWorker env brains = do

  penv <- ask

  mergeQ <- newTQueueIO

  -- FIXME: resume-on-exception
  hw <- async (refChanHeadMon penv)

  downloads <- async monitorHeadDownloads

  polls <- async refChanPoll

  wtrans <- async refChanWriter

  cleanup1 <- async cleanupRounds

  merge <- async (logMergeProcess env mergeQ)

  sto <- getStorage

  subscribe @e RefChanRequestEventKey $ \(RefChanRequestEvent chan val) -> do
    debug $ "RefChanRequestEvent" <+> pretty (AsBase58 chan) <+> pretty val

    h <- liftIO $ getRef sto (RefChanLogKey @s chan)

    -- игнорируем, если синхронно
    -- unless ((HashRef <$> h) == Just val) do

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
        pause @'Seconds 30

        now <- getTimeCoarse
        xs <- readTVarIO rounds <&> HashSet.toList

        forM_ xs $ \x -> do

          void $ runMaybeT do
            se <- MaybeT $ find @e x id

            closed <- readTVarIO (view refChanRoundClosed se)
            let ttl = view refChanRoundTTL se

            when (closed || ttl <= now) do
              lift $ expire x
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

          hashes <- maybe1 mbLog (pure mempty) $ readLog sto . HashRef

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
                                  , s ~ Encryption e
                                  )
                => RefChanWorkerEnv e
                -> TQueue (RefChanId e, HashRef)
                -> m ()

logMergeProcess _ q = do

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

      case hd of
        Just x -> pure (Just x)
        Nothing -> runMaybeT do
          hdblob <- MaybeT $ readBlobFromTree ( liftIO . getBlock sto ) h
          (_, headblk) <- MaybeT $ pure $ unboxSignedBox @(RefChanHeadBlock e) @e hdblob
          atomically $ modifyTVar (mergeHeads e) (HashMap.insert h headblk)
          pure headblk

    logMergeChan menv sto (chan, logs) = void $ runMaybeT do

      let chanKey = RefChanLogKey @s chan

      h <- MaybeT $ liftIO $ getRef sto chanKey

      current <- lift $ readLog sto (HashRef h) <&> HashSet.fromList

      trans <- filter (not . flip HashSet.member current) . mconcat <$> mapM (lift . readLog sto) logs
      -- trans <- mconcat <$> mapM (lift . readLog sto) logs

      guard (not $ List.null trans)

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

      r <- forM (trans <> HashSet.toList current) $ \href -> runMaybeT do

             blk <- MaybeT $ liftIO $ getBlock sto (fromHashRef href)

             tran <- MaybeT $ pure $ deserialiseOrFail @(RefChanUpdate e) blk
                                      & either (const Nothing) Just

             case tran of
                Propose _ box -> do
                  (pk, ProposeTran headRef box) <- MaybeT $ pure $ unboxSignedBox0 box
                  (ak, _) <- MaybeT $ pure $ unboxSignedBox0 box
                  hd <- MaybeT $ lift $ getHead menv headRef
                  let quo = view refChanHeadQuorum hd & fromIntegral
                  guard $ checkACL hd pk ak
                  pure [(href, (quo,mempty))]

                Accept  _ box -> do
                  (pk, AcceptTran headRef hashRef) <- MaybeT $ pure $ unboxSignedBox0 box
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

      let merged = (current <> HashSet.fromList new) & HashSet.toList

      let pt = toPTree (MaxSize 256) (MaxNum 256) merged

      unless (List.null merged) do
        liftIO do
          nref <- makeMerkle 0 pt $ \(_,_,bss) -> do
            void $ putBlock sto bss

          debug $ "NEW REFCHAN" <+> pretty chanKey <+> pretty  nref

          updateRef sto chanKey nref


