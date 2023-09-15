-- {-# Language AllowAmbiguousTypes #-}
-- {-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StrictData #-}
module RaftAlgo.Proto where

import HBS2.Prelude.Plated

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Cont
import Control.Monad.Error.Class
import Control.Monad.State.Strict
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Writer as W
import Data.Either
import Data.Foldable qualified as F
import Data.Function
import Data.Generics.Labels
import Data.Generics.Product
import Data.Generics.Sum
import Data.Heap (Heap)
import Data.Heap qualified as Heap
import Data.Kind
import Data.List qualified as List
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Ord
import Data.Sequence (Seq(..))
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Time
import Data.Time.Calendar.OrdinalDate
import Data.Void
import Lens.Micro.Platform as Lens
import Numeric.Natural
import Streaming
import Streaming.Prelude qualified as Streaming
import System.Random.MWC as MWC
import System.Random.Stateful


newtype Term = Term { unTerm :: Int }
  deriving (Eq, Ord, Num, Enum, Show)

-- | `p` - identity of a node
--   `a` - payload (reflog hash for example)
data LogEntry p a = LogEntry
  { entryTerm :: Term
  , entryStateMachineCommand :: StateMachineCommand p a
  }
  deriving (Generic, Show)

data StateMachineCommand p a
  = SMCLoad a
  -- | SMAddNode p
  -- | SMDropNode p
  deriving (Generic, Show)

-- Должно сохраняться надёжно:
-- currentTerm
-- votedFor
-- log[]
data PersMethods p a m = PersMethods
  { getCurrentTerm :: m Term
  , setCurrentTerm :: Term -> m ()
  --
  , getVotedFor :: m (Maybe p)
  , setVotedFor :: Maybe p -> m ()
  --
  -- log[] ..
  , getLogEntry :: Int -> m (Maybe (LogEntry p a))
  , setLogEntry :: Int -> LogEntry p a -> m ()
  , getLogEntriesFrom :: Int -> m [LogEntry p a]
  , getLastLogIndex :: m Int
  --
  , getOurID :: m p
  }

getLastLogIndexNTerm :: Monad m => PersMethods p a m -> m (Int, Term)
getLastLogIndexNTerm PersMethods{..} = do
    lastLogIndex <- getLastLogIndex
    (lastLogIndex, ) <$>
        if lastLogIndex > 0
          then maybe 0 entryTerm <$> getLogEntry lastLogIndex
          else pure 0

hoistMethods ::
       (forall a. m a -> n a)
    -> PersMethods p a m
    -> PersMethods p a n
hoistMethods nt h = PersMethods
  { getCurrentTerm = nt $ getCurrentTerm h
  , setCurrentTerm = nt . setCurrentTerm h
  --
  , getVotedFor = nt $ getVotedFor h
  , setVotedFor = nt . setVotedFor h
  --
  , getLogEntry = nt . getLogEntry h
  , setLogEntry = \j le -> nt $ setLogEntry h j le
  , getLogEntriesFrom = nt . getLogEntriesFrom h
  , getLastLogIndex = nt $ getLastLogIndex h
  --
  , getOurID = nt $ getOurID h
  }

data RaftState p = RaftState
  { commitIndex :: Int
  , lastAppliedIndex :: Int
  , actorState :: ActorState p
  , cluster :: Set p
  }
  deriving (Generic, Show)

data ActorState p
  = StateFollower
  | StateCandidate
    { votesCollected :: Set p
    }
  | StateLeader
    { nextIndex :: Map p Int
    , matchIndex :: Map p Int
    }
  deriving (Generic, Eq, Ord, Show)

---

data RequestVote p = RequestVote
  { requestVoteTerm :: Term
  , requestVoteCandidateID :: p
  , requestVoteCandidateLastLogIndex :: Int
  , requestVoteCandidateLastLogTerm :: Term
  }
  deriving (Generic, Show)

data AppendEntries p a = AppendEntries
  { appendEntriesTerm :: Term
  , appendEntriesLeaderID :: p
  , appendEntriesPrevLogIndex :: Int
  , appendEntriesPrevLogTerm :: Term
  , appendEntriesES :: [LogEntry p a]
  , appendEntriesLeaderCommitIndex :: Int
  }
  deriving (Generic, Show)

data ProcCall p a
  = CallRequestVote (RequestVote p)
  | RequestVoteReply
  { requestVoteReplyTerm :: Term
  , requestVoteReplyGranted :: Bool
  , requestVoteReplyFromID :: p
  }
  | CallAppendEntries (AppendEntries p a)
  | AppendEntriesReply
  { appendEntriesReplyTerm :: Term
  , appendEntriesReplySuccess :: Bool
  , appendEntriesReplyFromID :: p
  }
  deriving (Generic, Show)


data NodeEvent p a
  = ElectionTimeoutElapses
  | LeaderHeartbeat
  | GotProcCall (ProcCall p a)
  | GotClientCall (StateMachineCommand p a)
  deriving (Show)

data NodeAction p a
  = ResetElectionTimer
  | ResetLeaderHeartbeat

  | UpdateState (RaftState p -> RaftState p)
  | SwitchRole (ActorState p)

  | LogMessage Text
  | CallProc p (ProcCall p a)
  | ReplyProc p (ProcCall p a)

  | ReplyToClientWhoIsLeader (Maybe p)

---
-- Test cluster runner

data TestLogState p a = TestLogState
  { testLogStateCurrentTerm :: Term
  , testLogStateVotedFor :: Maybe p
  , testLogStateLog :: Seq (LogEntry p a)
  , testLogStateRaftState :: RaftState p
  , testLogStateNodeID :: p
  }
  deriving (Generic, Show)

newtype NodeID a = NodeID a
  deriving newtype (Show, Eq, Ord, Num, Enum)
type NodeIDInt = NodeID Int

newtype TestData = TestData Text
  deriving newtype (Show, Eq, Ord, IsString)

data TestEvent t = TestEvent
  { testEventTime :: t
  , testEvent :: NodeEvent NodeIDInt TestData
  }
  deriving (Generic)

data TestClusterState = TestClusterState
  { testClusterNodes :: Map NodeIDInt (TestLogState NodeIDInt TestData)
  , testClusterEvents :: Heap (Heap.Entry UTCTime (
        Either
          (NodeEvent NodeIDInt TestData, [TestEvent UTCTime])
          (NodeIDInt, NodeEvent NodeIDInt TestData)
      ))
  , testClusterElectionTimeouts :: Map NodeIDInt Int
  }
  deriving (Generic)

devTest :: IO ()
devTest = do
    evalTestCluster 5 commands $ Streaming.take 142 >>> Streaming.mapM display >>> Streaming.effects
  where
    display = List.uncons >>> mapM_ \(x,xs') -> liftIO do
          putStrLn (T.unpack x)
          forM_ xs' \x' -> do
              putStr "    "
              putStrLn (T.unpack x')
          putStrLn ""

    commands :: [TestEvent NominalDiffTime]
    commands = [
        TestEvent 9 (GotClientCall (SMCLoad "Tx1"))
      , TestEvent 1 (GotClientCall (SMCLoad "Tx2"))
      , TestEvent 3 (GotClientCall (SMCLoad "Tx3"))
      , TestEvent 2 (GotClientCall (SMCLoad "Tx4"))
      ]

evalTestCluster ::
    ( Monad m
    , m' ~ StateT TestClusterState (StateT StdGen m)
    )
   => Int
   -> [TestEvent NominalDiffTime]
   -> (Stream (Of [Text]) m' () -> m' ())
   -> m ()
evalTestCluster clusterSize cevs sf =
    runStateGenT_ (mkStdGen randomSeed) \g -> do

        initialNodeTimeouts <- forM nodeIDs \p -> do
            dt <- realToFrac @Double <$> MWC.uniformRM (minElectionTimeout, maxElectionTimeout) g
            pure $ Heap.Entry (addUTCTime dt timeBegin) (Right (p, ElectionTimeoutElapses))

        evalStateT (
            sf $
                  fix \go ->
                        lift (clusterStep g) >>= either
                            (\e -> Streaming.each [[e]])
                            (\es -> do
                                  Streaming.each [es]
                                  go
                            )
                  )
            TestClusterState
              { testClusterNodes = Map.fromList $ nodeIDs <&> (id &&& mkTestNodeState (Set.fromList nodeIDs))
              , testClusterEvents = Heap.fromList $ tcevs <> initialNodeTimeouts
              , testClusterElectionTimeouts = mempty
              }

  where

    randomSeed = 1

    minElectionTimeout :: Double
    minElectionTimeout = 5
    maxElectionTimeout = minElectionTimeout * 2

    heartbeatPeriod :: NominalDiffTime
    heartbeatPeriod = realToFrac (minElectionTimeout * 3 / 4)

    nodeIDs = fromIntegral <$> [1..clusterSize]

    timeBegin :: UTCTime
    timeBegin = UTCTime (YearDay 2000 1) 0

    tcevs = case integrateTestEventTimes timeBegin cevs of
        [] -> []
        (TestEvent t1 ev1):clientEvents ->
            [Heap.Entry t1 (Left (ev1, clientEvents))]

    integrateTestEventTimes :: UTCTime -> [TestEvent NominalDiffTime] -> [TestEvent UTCTime]
    integrateTestEventTimes t0 = flip evalState t0 . mapM \ev -> do
        upd <- addUTCTime (ev ^. #testEventTime) <$> get
        put upd
        pure $ ev & #testEventTime .~ upd

    mkTestNodeState :: Set NodeIDInt -> NodeIDInt -> TestLogState NodeIDInt TestData
    mkTestNodeState allNodeIDs nodeID = TestLogState
        { testLogStateCurrentTerm = 0
        , testLogStateVotedFor = Nothing
        , testLogStateLog = mempty
        , testLogStateRaftState =
            RaftState
              { commitIndex = 0
              , lastAppliedIndex = 0
              , actorState = StateFollower
              , cluster = allNodeIDs
              }
        , testLogStateNodeID = nodeID
        }

    clusterStep :: (Monad m, StatefulGen (StateGenM StdGen) m)
          => StateGenM StdGen -> StateT TestClusterState m (Either Text [Text])
    clusterStep g = runExceptT do
      (Heap.Entry tnow tev, heapRest) <- justOrThrowError "no events"
            $ gets (Heap.uncons . view #testClusterEvents)
      #testClusterEvents .= heapRest

      case tev of
        -- Запрос от клиента
        Left (ev, tevs) -> do
            case tevs of
                (TestEvent tnext evnext):tevs' -> do
                    #testClusterEvents %= mappend do
                        Heap.singleton (Heap.Entry tnext (Left (evnext, tevs')))
                [] -> pure ()
            clusterSize <- gets (Map.size . view #testClusterNodes)
            targetNodeID <- lift . lift $ fromIntegral <$> MWC.uniformRM (1, clusterSize) g
            runEvent tnow targetNodeID ev

        -- Событие от ноды для ноды nodeID
        Right (nodeID, ev :: NodeEvent NodeIDInt TestData) ->
            runEvent tnow nodeID ev


      where
        runEvent tnow nodeID ev = fromMaybe [] <$> runMaybeT do

            case ev of
                  ElectionTimeoutElapses -> do

                      x <- #testClusterElectionTimeouts . Lens.at nodeID . non 1 <%= pred
                      when (x > 0) do
                          fail "old timeout droped"

                  _ -> pure ()

            lift do
                nodeState :: TestLogState NodeIDInt TestData
                    <- justOrThrowError ("no state for node " <> showt nodeID)
                      $ gets $ preview (#testClusterNodes . ix nodeID)

                let testactions :: [TestNodeAction]
                    (testactions, nodeState') = flip runState nodeState (nodeTestStep ev)

                #testClusterNodes . ix nodeID .= nodeState'

                -- pure (Just nodeID, ev, mempty, testactions)

                -- [(NominalDiffTime, (NodeIDInt, NodeEvent NodeIDInt TestData))]
                (newevents, log :: [Text]) <- W.runWriterT $ catMaybes <$> forM testactions \case

                    TestResetElectionTimer -> do
                        dt <- lift . lift . lift $ realToFrac @Double
                            <$> MWC.uniformRM (minElectionTimeout, maxElectionTimeout) g
                        -- Как сбрасывать предыдущие таймеры?
                        -- В словарике testClusterElectionTimeouts по ключам
                        -- будем считать сколько добавлено новых
                        -- TestResetElectionTimer Событие кидать только если
                        -- оно соответствует единственной оставшейся записи.
                        x <- #testClusterElectionTimeouts . Lens.at nodeID . non 1 <<%= succ
                        -- W.tell [showt x]
                        pure $ Just $
                            (dt, (nodeID, ElectionTimeoutElapses))

                    TestResetLeaderHeartbeat ->
                        pure $ Just $
                            (heartbeatPeriod, (nodeID, LeaderHeartbeat))

                    TestApplyLogEntryToSM t -> pure Nothing

                    TestLogMessage t -> pure Nothing

                    TestCallProc nodeID pcall -> do
                        dt <- lift . lift . lift $ realToFrac @Double
                            <$> MWC.uniformRM (0.01, 0.1) g
                        pure $ Just $
                            (dt, (nodeID, GotProcCall pcall))

                    TestReplyProc nodeID pcall -> do
                        dt <- lift . lift . lift $ realToFrac @Double
                            <$> MWC.uniformRM (0.01, 0.1) g
                        pure $ Just $
                            (dt, (nodeID, GotProcCall pcall))

                    TestReplyToClientWhoIsLeader mnodeID -> forM mnodeID \nodeID -> do
                        dt <- lift . lift . lift $ realToFrac @Double
                            <$> MWC.uniformRM (0.3, 1) g
                        pure (dt, (nodeID, ev))

                #testClusterEvents %= mappend do
                    Heap.fromList (newevents <&> \(t',ev') -> Heap.Entry (addUTCTime t' tnow) (Right ev'))

                nodeStates <- gets (view #testClusterNodes)

                -- pure [showt (tnow, nodeID, ev, "->", testactions ^.. traverse . _Ctor @"TestLogMessage")]
                pure $ ([showt $ (tnow, nodeID, ev)]
                      <> (showt <$> (testactions ^.. traverse))
                      <> log
                      <> ["states:"]
                      <> (showt <$> Map.elems (nodeStates))
                        )

showt :: Show a => a -> Text
showt = T.pack . show

data TestNodeAction
  = TestResetElectionTimer
  | TestResetLeaderHeartbeat
  | TestApplyLogEntryToSM TestData
  | TestLogMessage Text
  | TestCallProc NodeIDInt (ProcCall NodeIDInt TestData)
  | TestReplyProc NodeIDInt (ProcCall NodeIDInt TestData)
  | TestReplyToClientWhoIsLeader (Maybe NodeIDInt)
  deriving (Generic, Show)

nodeTestStep :: forall m. (Monad m)
    => NodeEvent NodeIDInt TestData
    -> StateT (TestLogState NodeIDInt TestData) m [TestNodeAction]
nodeTestStep ev = do

    let h :: PersMethods NodeIDInt TestData (StateT (TestLogState NodeIDInt TestData) m)
        h = PersMethods
          { getCurrentTerm = gets (view #testLogStateCurrentTerm)
          , setCurrentTerm = (#testLogStateCurrentTerm .=)
          --
          , getVotedFor = gets (view #testLogStateVotedFor)
          , setVotedFor = (#testLogStateVotedFor .=)
          --
          , getLogEntry = \j -> gets $ Seq.lookup (j-1) . view #testLogStateLog
          , setLogEntry = \j le -> #testLogStateLog %= \s -> do
                when (Seq.length s < (j-1)) do
                    error "Raft algo error. Trying to set log element after missed elements"
                Seq.take (j-1) s Seq.|> le

          , getLogEntriesFrom = \j -> gets (F.toList . Seq.drop (j-1) . (view #testLogStateLog))
          , getLastLogIndex = gets (Seq.length . view #testLogStateLog)
          --
          , getOurID = gets (view #testLogStateNodeID)
          }

    rstate <- gets (view #testLogStateRaftState)
    (rstate', actions) <- do
        flip runStateT mempty do
            flip execStateT rstate do
                flip runContT absurd do
                    actions :: [TestNodeAction] <- testNodeReact (hoistMethods (lift . lift) h) ev
                    lift . lift $ modify (actions <>)
                    -- lift . lift . modify . (<>) =<< testNodeReact (hoistMethods (lift . lift) h) ev
                    ContT \k -> pure ()
    #testLogStateRaftState .= rstate'
    pure (reverse actions)

  where
        seqLast :: Seq a -> a
        seqLast = \case
            (Seq.viewr -> _ Seq.:> x) -> x
            _ -> error "no last element in empty sequence"

testNodeReact :: (MonadState (RaftState p) m, p ~ NodeIDInt, a ~ TestData)
    => PersMethods p a m -> NodeEvent p a -> ContT () m [TestNodeAction]
testNodeReact h ev = do
    x <- nodeReact h ev =<< get
    ContT \k -> do
        case x of
            ResetElectionTimer -> k [TestResetElectionTimer]
            ResetLeaderHeartbeat -> k [TestResetLeaderHeartbeat]

            UpdateState f -> id %= f
            SwitchRole st -> do
                #actorState .= st
                k [TestLogMessage ("Switch role to " <> showt st)]

            LogMessage msg -> k [TestLogMessage msg]
            CallProc p proc -> k [TestCallProc p proc]
            ReplyProc p proc -> k [TestReplyProc p proc]

            ReplyToClientWhoIsLeader mp -> k [TestReplyToClientWhoIsLeader mp]

        cix <- (gets (view #commitIndex))
        (gets (view #lastAppliedIndex)) >>= fix \go lastApplied -> do
            when (cix > lastApplied) do
                -- increment lastApplied, apply log[lastApplied] to stateMachine
                lastApplied' <- #lastAppliedIndex <%= succ
                (getLogEntry h) lastApplied' >>= mapM_ \le -> do
                    case entryStateMachineCommand le of
                        SMCLoad a -> do
                            k [TestApplyLogEntryToSM a]
                        -- | SMAddNode p
                        -- | SMDropNode p
                go lastApplied'

nodeReact :: forall m p a. (Monad m, Ord p, Show p, Show a)
    => PersMethods p a m
    -> NodeEvent p a
    -> RaftState p
    -> ContT () m (NodeAction p a)
nodeReact h@PersMethods{..} ev RaftState{..} = do
    ourID <- lift getOurID
    let otherNodes = cluster `Set.difference` Set.singleton ourID
    ContT \k -> case actorState of

        StateFollower -> do
            -- * respond to rpcs from candidates and leaders
            -- * if election timeout elapses without receiving AppendEntries from
            --   current leader or asking vote from candidates
            --   then convert to candidate
            case ev of
                ElectionTimeoutElapses -> do
                    k (SwitchRole (StateCandidate (Set.singleton ourID)))
                    startNewElection k

                LeaderHeartbeat -> do
                    pure ()

                GotProcCall proc -> case proc of

                    CallRequestVote (req@RequestVote {..}) -> do
                        updateCurrentTerm_ k requestVoteTerm
                        granted <- replyAfterRequestVote req k
                        k (LogMessage $ "granted: " <> showt granted)
                        when granted do
                            k ResetElectionTimer

                    CallAppendEntries req@AppendEntries {..} -> do
                        updateCurrentTerm_ k appendEntriesTerm
                        replyAfterAppendEntries req k do
                            k ResetElectionTimer

                    RequestVoteReply {..} -> do
                        k (LogMessage "Follower Got RequestVoteReply. Why ???")
                        void $ updateCurrentTerm_ k requestVoteReplyTerm

                    AppendEntriesReply {..} -> do
                        void $ updateCurrentTerm_ k appendEntriesReplyTerm

                GotClientCall {} -> do
                    votedFor <- getVotedFor
                    k (ReplyToClientWhoIsLeader votedFor)

        -- * if votes received from majority of servers: become leader
        -- * if AppendEntries received from new leader: become follower
        -- * if election timeout elapses: start new election
        StateCandidate{..} -> do
            case ev of
                ElectionTimeoutElapses -> do
                    startNewElection k

                LeaderHeartbeat -> do
                    pure ()

                GotProcCall proc -> case proc of

                    CallRequestVote req@RequestVote {..} -> do
                        upd <- updateCurrentTerm k requestVoteTerm
                        granted <- replyAfterRequestVote req k
                        when (granted && not upd) do
                            k ResetElectionTimer

                    CallAppendEntries req@AppendEntries {..} -> do
                        updateCurrentTerm_ k appendEntriesTerm
                        replyAfterAppendEntries req k do
                            k ResetElectionTimer
                            k (SwitchRole StateFollower)

                    RequestVoteReply {..} -> do
                        upd <- updateCurrentTerm k requestVoteReplyTerm
                        when (requestVoteReplyGranted && not upd) do
                            let votesCollected' = Set.insert requestVoteReplyFromID votesCollected
                            k (UpdateState (#actorState . _Ctor @"StateCandidate" .~ votesCollected'))
                            k (LogMessage ("Votes collected " <> showt votesCollected'))
                            when (Set.size votesCollected' > Set.size cluster `div` 2) do
                                (lastLogIndex, lastLogEntryTerm) <- getLastLogIndexNTerm h
                                k (SwitchRole (StateLeader
                                      { nextIndex = Map.fromList $ (Set.toList otherNodes) <&> (, succ lastLogIndex)
                                      , matchIndex = Map.fromList $ (Set.toList otherNodes) <&> (, defMatchIndex)
                                      }))
                                k ResetLeaderHeartbeat
                                forM_ otherNodes \p -> do
                                    term <- getCurrentTerm
                                    k (CallProc p (CallAppendEntries (
                                          AppendEntries
                                              { appendEntriesTerm = term
                                              , appendEntriesLeaderID = ourID
                                              , appendEntriesPrevLogIndex = lastLogIndex
                                              , appendEntriesPrevLogTerm = lastLogEntryTerm
                                              , appendEntriesES = mempty
                                              , appendEntriesLeaderCommitIndex = commitIndex
                                              })))

                    AppendEntriesReply {..} -> do
                        void $ updateCurrentTerm k appendEntriesReplyTerm

                GotClientCall {} -> do
                    votedFor <- getVotedFor
                    k (ReplyToClientWhoIsLeader votedFor)

        StateLeader{..} -> do
            let
                leaderCallAppendEntries p = do
                        ourLastLogIndex <- getLastLogIndex

                        let pPrevIndex = maybe ourLastLogIndex pred (nextIndex ^? ix p)
                        (pPrevEntryTerm, entries') <-
                            if pPrevIndex > 0
                              then
                                  maybe (error "Bug in algorithm") (over _1 entryTerm) . List.uncons
                                      <$> getLogEntriesFrom pPrevIndex
                              else
                                  (0 ,) <$> getLogEntriesFrom 1

                        term <- getCurrentTerm
                        k (CallProc p (CallAppendEntries (
                              AppendEntries
                                  { appendEntriesTerm = term
                                  , appendEntriesLeaderID = ourID
                                  , appendEntriesPrevLogIndex = pPrevIndex
                                  , appendEntriesPrevLogTerm = pPrevEntryTerm
                                  , appendEntriesES = entries'
                                  , appendEntriesLeaderCommitIndex = commitIndex
                                  })))

            case ev of

                ElectionTimeoutElapses ->
                    pure ()

                LeaderHeartbeat -> do
                    mapM_ leaderCallAppendEntries otherNodes
                    k ResetLeaderHeartbeat

                GotProcCall proc -> case proc of

                    CallRequestVote req@RequestVote {..} -> do
                        updateCurrentTerm k requestVoteTerm
                        replyAfterRequestVote req k
                        pure ()

                    CallAppendEntries req@AppendEntries {..} -> do
                        updateCurrentTerm k appendEntriesTerm
                        replyAfterAppendEntries req k do
                            k ResetElectionTimer
                            k (SwitchRole StateFollower)
                        pure ()

                    RequestVoteReply {..} -> do
                        updateCurrentTerm k requestVoteReplyTerm
                        pure ()

                    AppendEntriesReply {..} -> do
                        -- Нужно ли здесь учитывать appendEntriesReplyTerm ?
                        currentTerm <- getCurrentTerm
                        if (appendEntriesReplyTerm == currentTerm)
                          then do
                            if appendEntriesReplySuccess
                              then do
                                  ourLastLogIndex <- getLastLogIndex
                                  let
                                      -- updMatchIndex :: Map p Int -> Map p Int
                                      updMatchIndex = Lens.at appendEntriesReplyFromID . non defMatchIndex .~ ourLastLogIndex
                                  k (UpdateState ((
                                        #actorState . _Ctor @"StateLeader"
                                            . _1  -- . #nextIndex
                                            . ix appendEntriesReplyFromID .~ succ ourLastLogIndex
                                    ) . (
                                        #actorState . _Ctor @"StateLeader"
                                            . _2  -- . #matchIndex
                                            %~ updMatchIndex
                                    )))
                                  let newCommitIndex = matchIndex & updMatchIndex & Map.elems
                                          & Heap.fromList
                                          & Heap.drop ((Set.size cluster `div` 2 - 1))
                                          & Heap.uncons
                                          & maybe defMatchIndex fst
                                  when (newCommitIndex > commitIndex) do
                                      getLogEntry newCommitIndex >>= mapM_ do
                                          entryTerm >>> \term ->
                                              when (term == currentTerm) do
                                                  setCurrentTerm term
                                                  k (UpdateState (#commitIndex .~ newCommitIndex))
                              else do
                                  k (UpdateState (#actorState . _Ctor @"StateLeader"
                                        . _1  -- . #nextIndex
                                        . ix appendEntriesReplyFromID %~ pred
                                    ))
                                  leaderCallAppendEntries appendEntriesReplyFromID
                          else
                              void $ updateCurrentTerm k appendEntriesReplyTerm

                GotClientCall cmd -> do
                    j <- getLastLogIndex
                    term <- getCurrentTerm
                    setLogEntry (succ j)
                        (LogEntry
                          { entryTerm = term
                          , entryStateMachineCommand = cmd
                          })
                    -- TODO "respond after entry applied to state machine"

  where

    defMatchIndex = 0

    startNewElection k = do

        ourID <- getOurID
        let otherNodes = cluster `Set.difference` Set.singleton ourID

        setCurrentTerm . succ =<< getCurrentTerm
        setVotedFor . Just =<< getOurID
        k ResetElectionTimer

        term <- getCurrentTerm
        (lastLogIndex, lastLogEntryTerm) <- getLastLogIndexNTerm h
        forM_ otherNodes \p -> do
            k (CallProc p (CallRequestVote (
                  RequestVote
                      { requestVoteTerm = term
                      , requestVoteCandidateID = ourID
                      , requestVoteCandidateLastLogIndex = lastLogIndex
                      , requestVoteCandidateLastLogTerm = lastLogEntryTerm
                      })))

    updateCurrentTerm_ k term = do
        updateCurrentTerm' k term (pure ())

    updateCurrentTerm k term = do
        updateCurrentTerm' k term do
            k (SwitchRole StateFollower)
            k ResetElectionTimer

    updateCurrentTerm' k term onUpdate = do
        currentTerm <- getCurrentTerm
        k (LogMessage ("updateCurrentTerm ? our:" <> showt currentTerm <> " -> new:" <> showt term))
        if (currentTerm < term) then do
            setCurrentTerm term
            onUpdate
            setVotedFor Nothing
            pure True
          else pure False

    replyAfterRequestVote RequestVote {..} k = do
        -- 1. if term < currentTerm: reply false
        -- 2. else if voteFor is null or candidateID,
        --    and candidate's log is at least up-to-date as our log: grant vote
        currentTerm <- getCurrentTerm
        granted <- (either (\e -> k (LogMessage e) >> pure False) (const (pure True))) =<< runExceptT do

            when (requestVoteTerm < currentTerm) do
                throwError "requestVoteTerm < currentTerm"

            lift getVotedFor >>= mapM_ \ourVoteFor -> do
                when (ourVoteFor /= requestVoteCandidateID) do
                    throwError "already voted for another candidate"

        setVotedFor (Just requestVoteCandidateID)
        ourID <- getOurID
        k $ ReplyProc requestVoteCandidateID
          $ RequestVoteReply
                { requestVoteReplyTerm = currentTerm
                , requestVoteReplyGranted = granted
                , requestVoteReplyFromID = ourID
                }
        pure granted

    replyAfterAppendEntries AppendEntries {..} k onSuccess = do
        -- 1. if term < currentTerm: reply false
        -- 2. reply false if log doesn't contain an entry at prevLogIndex whose
        --    term matches prevLogTerm
        -- 3. if an existing entry conflicts with a new one (same index but
        --    different terms), delete the existing entry and all that follow it
        -- 4. append any new entries not already in the log
        -- 5. if leaderCommit > commitIndex,
        --     set commitIndex = min(leaderCommit, index of last new entry)

        currentTerm <- getCurrentTerm
        k (LogMessage ("replyAfterAppendEntries our:" <> showt currentTerm <> " <? new:" <> showt appendEntriesTerm))
        success <- (either (\e -> k (LogMessage e) >> pure False) (const (pure True))) =<< runExceptT do

            when (appendEntriesTerm < currentTerm) do
                throwError "appendEntriesTerm < currentTerm"

            when (appendEntriesPrevLogIndex > 0) do
                le' <- justOrThrowError ("No log entry " <> showt appendEntriesPrevLogIndex)
                    $ getLogEntry appendEntriesPrevLogIndex
                when (entryTerm le' /= appendEntriesPrevLogTerm) do
                    throwError "entryTerm at appendEntriesPrevLogIndex /= appendEntriesPrevLogTerm"

            justOrThrowError "?-?" $ flip fix (zip [succ appendEntriesPrevLogIndex..] appendEntriesES) $
                \go -> \case
                    [] -> pure (Just ())
                    (j, le):es ->
                        getLogEntry j >>= \case

                            Nothing -> do
                                setLogEntry j le
                                mapM_ (uncurry setLogEntry) es
                                pure (Just ())

                            Just cle -> do
                              if (entryTerm cle == entryTerm le)
                                then go es
                                else do
                                  setLogEntry j le
                                  mapM_ (uncurry setLogEntry) es
                                  pure (Just ())

            when (appendEntriesLeaderCommitIndex > commitIndex) do
                lift do
                    j <- getLastLogIndex
                    k (UpdateState (#commitIndex .~ min appendEntriesLeaderCommitIndex j))

        when success onSuccess

        ourID <- getOurID
        k $ ReplyProc appendEntriesLeaderID
          $ AppendEntriesReply
                { appendEntriesReplyTerm = currentTerm
                , appendEntriesReplySuccess = success
                , appendEntriesReplyFromID = ourID
                }

justOrThrowError :: Functor m => e -> m (Maybe a) -> ExceptT e m a
justOrThrowError e = ExceptT . fmap (maybe (Left e) Right)
