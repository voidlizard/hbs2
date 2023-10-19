{-# Language TemplateHaskell #-}
{-# Language AllowAmbiguousTypes #-}
{-# Language UndecidableInstances #-}
{-# Language MultiWayIf #-}
module HBS2.Net.Proto.QBLF where

import HBS2.Prelude.Plated
import HBS2.System.Logger.Simple
import HBS2.Clock
import HBS2.Hash

import Control.Applicative
import Control.Concurrent.STM (flushTQueue)
import Control.Monad
import Control.Monad.Trans.Maybe
import Data.Either
import Data.Function
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.Kind
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Maybe
import Data.Ord
import Data.Tuple (swap)
import Lens.Micro.Platform
import System.Random (randomRIO)
import Data.Time.Clock.POSIX
import Codec.Serialise()
import Data.Fixed
import UnliftIO

{- HLINT ignore "Use newtype instead of data" -}

newtype QBLFTimeStamp =
  QBLFTimeStamp { fromQBLFTimeStamp :: Fixed E12 }
  deriving stock (Generic)
  deriving newtype (Eq,Ord,Num,Real,Fractional,Show)

instance Serialise QBLFTimeStamp

data QBLFMessage w =
    QBLFMsgAnn       (QBLFActor w)  (QBLFAnnounce w)
  | QBLFMsgMerge     (QBLFActor w ) (QBLFMerge w)
  | QBLFMsgCommit    (QBLFActor w)  (QBLFCommit w)
  | QBLFMsgHeartBeat (QBLFActor w)  QBLFStateN (QBLFState w) QBLFTimeStamp
  deriving stock Generic

data QBLFAnnounce w =
  QBLFAnnounce (QBLFState w) (QBLFState w)
  deriving stock (Generic)


data QBLFMerge w =
  QBLFMerge (QBLFState w) (QBLFState w)
  deriving stock Generic

data QBLFCommit w =
  QBLFCommit (QBLFState w) (QBLFState w)
  deriving stock Generic


data QBLFStateN =
    QWait
  | QAnnounce
  | QMerge
  | QCommit
  deriving stock (Eq,Ord,Enum,Show,Generic)

instance Serialise QBLFStateN

type ForQBLF w = ( Hashed HbSync (QBLFState w)
                 , Hashable (QBLFState w)
                 , Hashable (QBLFTransaction w)
                 , Hashable (QBLFActor w)
                 , Hashable (QBLFAnnounce w)
                 , Hashable (QBLFMerge w)
                 , Eq (QBLFState w)
                 )

deriving instance ForQBLF w => Eq (QBLFAnnounce w)
deriving instance ForQBLF w => Eq (QBLFMerge w)

instance ForQBLF w => Hashable (QBLFAnnounce w)

instance ForQBLF w => Hashable (QBLFMerge w)

class (Monad m, ForQBLF w) => IsQBLF w m where
  type QBLFActor w       :: Type
  type QBLFTransaction w :: Type
  type QBLFState w       :: Type

  qblfNewState    :: QBLFState w -> [QBLFTransaction w] -> m (QBLFState w)
  qblfBroadCast   :: QBLFMessage w -> m ()
  qblfMerge       :: QBLFState w -> [QBLFState w] -> m (QBLFState w)
  qblfCommit      :: QBLFState w -> QBLFState w -> m ()

  qblfMoveForward :: QBLFState w -> QBLFState w -> m Bool
  qblfMoveForward _ _ = pure True

data QBLF w =
  QBLF
  { _qblfSelf          :: QBLFActor w
  , _qblfAllActors     :: HashSet (QBLFActor w)
  , _qblfState         :: QBLFStateN
  , _qblfCurrent       :: QBLFState w
  , _qblfWaitAnnounce  :: Timeout 'Seconds
  , _qblfCommitsFrom   :: HashSet (QBLFActor w)
  , _qblfTranQ         :: TVar (HashSet (QBLFTransaction w))
  , _qblfAlive         :: TVar (HashMap (QBLFActor w) (QBLFStateN, QBLFState w, TimeSpec))
  , _qblfStateTime     :: TVar TimeSpec
  , _qblfLastHeartBeat :: TVar TimeSpec
  , _qblfAnnounces     :: TVar (HashMap (QBLFActor w, QBLFAnnounce  w)  TimeSpec)
  , _qblfMerges        :: TVar (HashMap (QBLFActor w, QBLFMerge  w)  TimeSpec)
  }

makeLenses ''QBLF


qblfGetActor :: (ForQBLF w) => QBLFMessage w -> QBLFActor w
qblfGetActor = \case
  QBLFMsgAnn a _   -> a
  QBLFMsgMerge a _   -> a
  QBLFMsgCommit a _ -> a
  QBLFMsgHeartBeat a _ _ _ -> a

qblfEnqueue :: (ForQBLF w, MonadIO m) => QBLF w -> QBLFTransaction w -> m ()
qblfEnqueue me tran = do
  -- synced <- qblfIsSynced me
  -- when synced do
  atomically $ modifyTVar (view qblfTranQ me) (HashSet.insert tran)



qblfAcceptMessage :: (ForQBLF w, MonadIO m) => QBLF w -> QBLFMessage w -> m ()
qblfAcceptMessage me msg = do

  -- FIXME: drop-premature-announces

  let actor = qblfGetActor msg

  when (actor `HashSet.member` view qblfAllActors me) do
    now <- getTimeCoarse

    let mine = view qblfCurrent me
    let add = const True -- not $ HashSet.member x (view qblfIgnore me)

    case msg of
      QBLFMsgAnn a ann@(QBLFAnnounce s0 _) | add s0 -> do
        atomically $ modifyTVar (view qblfAnnounces me) (HashMap.insert (a, ann) now)

      QBLFMsgMerge a m@(QBLFMerge s0 _) | add s0 -> do
        atomically $ modifyTVar (view qblfMerges me) (HashMap.insert (a, m) now)

      QBLFMsgCommit a (QBLFCommit s0 s) | add s0 -> do
        atomically $ modifyTVar (view qblfAlive me) (HashMap.insert a (QWait,s,now))

      QBLFMsgHeartBeat a t s _ -> do
        -- debug $ "heartbeat" <+> pretty (view qblfSelf me) <+> pretty (a, s)
        atomically $ modifyTVar (view qblfAlive me) (HashMap.insert a (t,s,now))

      _ -> pure ()

qblfQuorum :: forall w a m . (ForQBLF w, IsQBLF w m, MonadUnliftIO m, Integral a)
           => QBLF w
           -> m a

qblfQuorum me = do
  -- n <- qblfLastAlive me
  -- pure $ fromIntegral $ 1 + (n `div` 2)
  let aliveSz = view qblfAllActors me & List.length
  pure $ max 1 $ round $ realToFrac (aliveSz + 1) / 2

qblfLastAlive :: (ForQBLF w, IsQBLF w m, MonadUnliftIO m) => QBLF w -> m Int
qblfLastAlive me = pure 0
  -- q <- qblfQuorum me
  -- n <- atomically $ readTVar (view qblfAlive me) <&> HashMap.toList <&> length
  -- if n > 0 then
  --   pure n
  -- else
  --   pure q


qblfInit :: forall w m . (ForQBLF w, IsQBLF w m, MonadUnliftIO m)
         => QBLFActor w      -- ^ self
         -> [QBLFActor w]    -- ^ all actors
         -> QBLFState w      -- ^ initial state
         -> Timeout 'Seconds -- ^ timeout
         -> m (QBLF w)

qblfInit self actors s0 w =
  QBLF self
       (HashSet.fromList actors)
       QWait
       s0
       w
       mempty
       <$> newTVarIO mempty
       <*> newTVarIO mempty
       <*> (newTVarIO =<< now)
       <*> newTVarIO 0
       <*> newTVarIO mempty
       <*> newTVarIO mempty

  where
    now = getTimeCoarse

qblfNextCommitTime :: (MonadIO f, Real a) => a -> f TimeSpec
qblfNextCommitTime ww = do
  let wt = realToFrac ww
  t0 <- getTimeCoarse
  dt <- liftIO $ randomRIO (wt/2, wt)
  pure $ fromNanoSecs $ toNanoSecs t0 + round (realToFrac dt * 1e9)

-- qblfGetState :: (ForQBLF w, MonadUnliftIO m) => QBLF w -> m QBLFStateN
-- qblfGetState q = readTVarIO (view qblfState q)



qblfRun :: forall w m . ( Pretty (QBLFActor w)
                        , Pretty (QBLFState w)
                        , ForQBLF w
                        , IsQBLF w m
                        , MonadUnliftIO m
                        ) => QBLF w -> m ()
qblfRun me = do

  forever $ do
    void $ qblfTo me QWait
    warn "QUIT!!!"

  -- mapM_ wait [a1,hb]
  -- mapM_ wait [a1]

  -- waitAnyCatchCancel []

  where

    tAlive = 5

    minHeartBeat = round 5e9

    sendHeartBeat :: IsQBLF w m => QBLF w -> m ()
    sendHeartBeat s = do
      ts <- liftIO getPOSIXTime <&> realToFrac
      now <- getTimeCoarse
      sent <- readTVarIO (_qblfLastHeartBeat s)
      when (toNanoSecs (now - sent) > minHeartBeat) do
        qblfBroadCast @w $ QBLFMsgHeartBeat (view qblfSelf s) (view qblfState s) (view qblfCurrent s) ts
        atomically $ writeTVar (_qblfLastHeartBeat s) now

    sendAnnounce :: IsQBLF w m => QBLF w -> QBLFState w -> m ()
    sendAnnounce s sx = do
      qblfBroadCast @w (QBLFMsgAnn self (QBLFAnnounce current sx))
      where
        self  = view qblfSelf s
        current = view qblfCurrent s

    sendMerge :: IsQBLF w m => QBLF w -> QBLFState w -> m ()
    sendMerge s sx = do
      qblfBroadCast @w (QBLFMsgMerge self (QBLFMerge current sx))
      where
        self  = view qblfSelf s
        current = view qblfCurrent s

    sendCommit :: IsQBLF w m => QBLF w -> QBLFState w -> m ()
    sendCommit s sx = do
      qblfBroadCast @w (QBLFMsgCommit self (QBLFCommit current sx))
      where
        self  = view qblfSelf s
        current = view qblfCurrent s


    nap = pause @'Seconds 0.25

    getAlive :: IsQBLF w m => QBLF w -> m [(QBLFActor w, QBLFStateN, QBLFState w) ]
    getAlive s = do
      now <- getTimeCoarse
      states <- readTVarIO (view qblfAlive s) <&> HashMap.toList
      pure [ (a,n,sx) | (a, (n,sx,t)) <- states, toNanoSecs (now - t) < round (2 * tAlive * 1e9) ]

    sweepMerges :: IsQBLF w m => QBLF w -> QBLFState w -> m ()
    sweepMerges qblf s = do
      atomically do
        old <- readTVar (view qblfMerges qblf) <&> HashMap.toList
        let new = [ ((a, m), t)  | ((a, m@(QBLFMerge s0 _)), t) <- old, s0 /= s ]
        writeTVar (view qblfMerges me) (HashMap.fromList new)

    sweepAnnounces :: IsQBLF w m => QBLF w -> m ()
    sweepAnnounces qblf = do
      -- FIXME: fix-magic-number
      let wnano = fromIntegral $ toNanoSeconds $ max 300 $ 10 * view qblfWaitAnnounce qblf

      now <- getTimeCoarse

      swept <- atomically do
        old <- readTVar (view qblfAnnounces qblf) <&> HashMap.toList
        let new = [ ((a, m), t)
                  | ((a, m@(QBLFAnnounce _ _)), t) <- old
                  , toNanoSecs (now - t) < wnano
                  ]
        writeTVar (view qblfAnnounces me) (HashMap.fromList new)
        pure $ length old - length new

      when (swept > 0) do
        debug $ "sweepAnnounces" <+> pretty swept

    qblfTo s n = do
      now <- getTimeCoarse
      let ns = s { _qblfState = n }
      atomically $ writeTVar (_qblfStateTime ns) now
      case n of
        QWait     -> qblfToWait ns
        QAnnounce -> qblfToAnnounce ns
        QMerge    -> qblfToMerge ns
        QCommit   -> qblfToCommit ns

    qblfToWait s = do
      let w = view qblfWaitAnnounce s
      q <- qblfQuorum s

      let trsh = max 2 (q `div` 2)

      newAnn <- newTVarIO mempty

      fix \next -> do

        sendHeartBeat s

        sweepAnnounces s

        now <- getTimeCoarse
        alive  <- getAlive s
        let wn = sum [ 1 | (_, QWait, _) <- alive ]
        -- debug $ logActor s <+> "wait" <+> pretty wn

        t0 <- readTVarIO (view qblfStateTime s)
        let elapsed = toNanoSeconds $ TimeoutTS (now - t0)

        their <- selectState s (fmap (view _3) alive)
        let mine = view qblfCurrent s

        ann0 <- atomically $ readTVar (view qblfAnnounces s) <&> HashMap.keys

        let alst = [ s1 | (_, QBLFAnnounce s0 s1) <- ann0, s0 == mine, s1 /= s0 ]

        old <- readTVarIO newAnn
        let an = HashSet.size $ HashSet.fromList alst `HashSet.difference` old

        atomically $ writeTVar newAnn (HashSet.fromList alst)

        let g = if | their /= Just mine -> do

                     case their of
                      Nothing -> pure $ nap >> next
                      Just th -> do

                     -- FIXME: what-if-failed
                       forwarded <- qblfMoveForward @w mine th

                       if forwarded then do
                         debug $ logActor s <+> "DO FAST-FORWARD" <+> pretty th
                         qblfCommit @w mine th
                         pure $ qblfTo (set qblfCurrent th s) QAnnounce
                       else do
                         pure $ nap >> next

                   | wn >= q && (elapsed > toNanoSeconds w || an >= trsh) && Just mine == their -> do
                      let el = elapsed > toNanoSeconds w
                      let aa = an >= trsh
                      debug $ logActor s <+> "ready" <+> pretty their <+> pretty el <+> pretty aa <+> pretty an
                      pure $ qblfTo s QAnnounce

                   | otherwise -> pure $ nap >> next

        join g

    qblfToAnnounce s = do

      let mine = view qblfCurrent s
      q <- qblfQuorum s
      let wa = view qblfWaitAnnounce s

      sweepMerges s mine

      -- TODO: extract-method
      txs <- atomically do
                tx <- readTVar (view qblfTranQ s) <&> HashSet.toList
                writeTVar (view qblfTranQ s) mempty
                pure tx

      hx <- qblfNewState @w mine txs

      sendAnnounce s hx

      pause (0.1 * wa)

      g <- race ( pause wa ) do

        fix \next -> do

          ann0 <- atomically $ readTVar (view qblfAnnounces s) <&> HashMap.keys

          let ann = [ s1 | (_, QBLFAnnounce s0 s1) <- ann0, s0 == mine ]

          if length ann >= q then do
            debug $ logActor s <+> "announce/ready-to-merge" <+> pretty (view qblfCurrent s) <+> pretty (length ann)
            pure $ qblfTo s QMerge
          else do
            trace $ logActor s <+> "announce/wait" <+> pretty (view qblfCurrent s) <+> pretty (length ann)
            nap >> next

      case g of
        Left{}  -> qblfTo s QWait
        Right n -> n

    qblfToMerge s = do
      let mine = view qblfCurrent s
      q <- qblfQuorum s

      let wa = view qblfWaitAnnounce s
      -- pause @'Seconds wa
      -- debug $ logActor s <+> "merge"

      ann0 <- atomically $ readTVar (view qblfAnnounces s) <&> HashMap.keys
      let aann = [ (a, s1) | (a, QBLFAnnounce s0 s1) <- ann0, s0 == mine ]

      let ann = fmap snd aann
      let actors = fmap fst aann & HashSet.fromList

      let g = case ann of
            [] -> do
              debug $ "MERGE: not found SHIT!" <+> pretty (length aann)
              pure $ qblfTo s QWait

            (_:_) -> do
              new <- qblfMerge @w mine ann
              sendMerge s new
              pure $ qblfTo (set qblfCommitsFrom actors s) QCommit

      join g

    qblfToCommit s = do
      let mine = view qblfCurrent s
      let authors = view qblfCommitsFrom s

      -- pause @'Seconds 2
      debug $ logActor s <+> "qblfToCommit"

      -- FIXME: timeout-and-rollback-to-wait

      let wa = view qblfWaitAnnounce s

      r <- race ( pause wa ) do
              fix \next -> do

                merges0 <- atomically $ readTVar (view qblfMerges s) <&> HashMap.keys
                let merges = [ (s1, a, m)
                             | (a, m@(QBLFMerge s0 s1)) <- merges0, s0 == mine
                             , a `HashSet.member` authors
                             ]

                mbNew <- selectState s (fmap (view _1) merges)

                trace $ "#### COMMIT NEW:"
                  <+> pretty mine
                  <+> pretty mbNew
                  -- <+> pretty (fmap (view _1) merges)

                case mbNew of
                  Just new -> do
                    when (new /= mine) do
                      debug $ logActor s <+> "commit: " <+> pretty new
                      sendCommit s new
                      qblfCommit @w mine new
                      -- sweepAnnounces s mine

                    pure $ qblfTo ( set qblfCurrent new s
                                  ) QWait

                  Nothing -> do
                    -- debug $ logActor s <+> "commit: " <+> "fail"
                    nap >> next

      case r of
        Left{} -> qblfTo s QWait
        Right n -> n

    selectState :: IsQBLF w m => QBLF w -> [QBLFState w] -> m (Maybe (QBLFState w))
    selectState s sx = do
      q <- qblfQuorum s
      let mbs = fmap (,1) sx  & HashMap.fromListWith (+)
                              & HashMap.toList
                              & fmap (over _2 List.singleton . swap)
                              & Map.fromListWith (<>)
                              & Map.toDescList
                              & headMay
      runMaybeT do
        ss <- MaybeT $ pure mbs
        let sss = over _2 (List.sortOn (Down . hashObject @HbSync)) ss :: (Integer, [QBLFState w])

        if fst sss >= q then do
          MaybeT $ pure $ headMay (snd sss)
        else
          mzero

    logActor s = "ACTOR" <> parens (pretty (view qblfSelf s))

