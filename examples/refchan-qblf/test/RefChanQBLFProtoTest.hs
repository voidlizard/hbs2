{-# Language AllowAmbiguousTypes #-}
module Main where

import HBS2.Prelude.Plated
import HBS2.System.Logger.Simple
import HBS2.Clock
import HBS2.Hash
import HBS2.Base58

import QBLF.Proto

import Data.Ord
import Data.Functor
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.List qualified as List
import Control.Monad.Reader
import System.Random
import Data.Hashable hiding (Hashed)
import Data.Word
import Lens.Micro.Platform
import Data.Fixed
import Options.Applicative as O
import Data.Cache (Cache)
import Data.Cache qualified as Cache
import Data.Maybe
import Data.Graph
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Prettyprinter.Render.Terminal
import GHC.TypeLits

import Codec.Serialise

import UnliftIO

type MyHash = Hash HbSync

{- HLINT ignore "Use newtype instead of data" -}

newtype Actor = Actor { fromActor :: Int }
                deriving stock (Eq,Ord,Show,Generic)
                deriving newtype (Num,Enum,Hashable,Pretty)

newtype Tran = OddEven { fromTran :: Integer }
               deriving stock (Eq,Ord,Show,Generic)
               deriving newtype (Hashable)


instance Serialise Tran

newtype MyState = MyState { fromMyState :: MyHash } -- [Tran]
                  deriving stock (Eq,Ord,Show,Generic)
                  deriving newtype (Hashable,Pretty)

instance Hashed HbSync MyState where
  hashObject (MyState x) = x

data MyConsensus = MyConsensus

data MyEnv =
  MyEnv
  { mySelf    :: Actor
  , myActors  :: HashMap Actor (TQueue (QBLFMessage MyConsensus))
  , myState   :: TVar MyState
  , myStorage :: TVar (HashMap MyHash (HashSet Tran))
  , myCommits :: TVar (HashSet (MyHash, MyHash))
  }



newtype RunM m a = RunM { fromRunM :: ReaderT MyEnv m a }
                   deriving newtype ( Applicative
                                    , Functor
                                    , Monad
                                    , MonadIO
                                    , MonadUnliftIO
                                    , MonadReader MyEnv
                                    )

runM :: MyEnv -> RunM m a -> m a
runM env f = runReaderT (fromRunM f) env


instance MonadIO m => IsQBLF MyConsensus (RunM m) where
  type QBLFActor MyConsensus = Actor
  type QBLFTransaction MyConsensus = Tran
  type QBLFState MyConsensus = MyState

  qblfNewState _ txs = do
    let tx1 = HashSet.fromList txs
    let h1 = hashObject (serialise tx1)
    let ms1 = MyState h1
    ca <- asks myStorage
    atomically $ modifyTVar ca $ HashMap.insert h1 tx1
    pure ms1

  qblfCommit s0 s1 = do
    mst <- asks myState
    me  <- asks mySelf
    co  <- asks myCommits

    atomically $ do
      writeTVar mst s1
      modifyTVar co (HashSet.insert (fromMyState s1, fromMyState s0))

    trace $ "COMMIT" <> braces (viaShow me)
              <+> ":"
              <+> pretty s0
              <+> "->"
              <+> pretty s1

  qblfBroadCast msg = do
    actors <- ask <&> myActors <&> HashMap.toList
    forM_ actors $ \(a,q) -> do
      -- debug $ "qblfBroadCast to" <+> pretty a
      atomically $ writeTQueue q msg

  qblfMerge m mss = do

    cache <- asks myStorage

    txs' <- forM (m:mss) $ \(MyState h) -> do
               readTVarIO cache <&> HashMap.lookup h <&> fromMaybe mempty

    let txs = HashSet.unions txs'

    let tx3 = HashSet.filter evenOnly txs
--       -- List.nub $ List.filter evenOnly $  List.sort txs
    let h3 = hashObject (serialise tx3)

    let ms3 = MyState h3
    atomically $ modifyTVar cache $ HashMap.insert h3 tx3
--
    pure ms3

    where
      evenOnly (OddEven x) = even x

tracePrefix :: SetLoggerEntry
tracePrefix  = logPrefix "[trace] "

debugPrefix :: SetLoggerEntry
debugPrefix  = logPrefix "[debug] "

errorPrefix :: SetLoggerEntry
errorPrefix  = logPrefix "[error] "

warnPrefix :: SetLoggerEntry
warnPrefix   = logPrefix "[warn] "

noticePrefix :: SetLoggerEntry
noticePrefix = logPrefix "[notice] "

data Opts =
  Opts
  { cliActorsNum         :: Int
  , cliWaitAnnounceTime  :: Double
  , cliPd                :: Double
  , cliPb                :: Double
  , cliPt                :: Double
  , cliWt                :: Double
  , cliTrace             :: Bool
  }

cliOptions :: Parser Opts
cliOptions = Opts
  <$> option auto
      ( long "actors-num"
     <> short 'n'
     <> help "number of actors"
     <> showDefault
     <> value 2 )
  <*> option auto
      ( long "wait-announce-time"
     <> short 'w'
     <> help "wait-announce-time"
     <> showDefault
     <> value 10 )
  <*> option auto
      ( long "pD"
     <> help "p(disaster)"
     <> showDefault
     <> value 0.000001 )
  <*> option auto
      ( long "pB"
     <> help "p(Bad)"
     <> showDefault
     <> value 0.05 )
  <*> option auto
      ( long "pT"
     <> help "p(transaction)"
     <> showDefault
     <> value 0.10 )
  <*> option auto
      ( long "wT"
     <> help "Period(transaction)"
     <> showDefault
     <> value 1.00 )
  <*> option auto
      ( long "trace"
     <> help "enable trace"
     <> showDefault
     <> value False )



main :: IO ()
main = do

  opts <- execParser $ O.info (cliOptions <**> helper)
      (  fullDesc
      <> progDesc "refchan-qblf-proto-test"
      )

  when (cliTrace opts) do
    setLogging @TRACE  tracePrefix

  setLogging @DEBUG  debugPrefix
  setLogging @INFO   defLog
  setLogging @ERROR  errorPrefix
  setLogging @WARN   warnPrefix
  setLogging @NOTICE noticePrefix

  cache <- newTVarIO mempty
  let nada = hashObject  (serialise (mempty :: HashSet Tran))
  let s0 = MyState nada
  atomically $ modifyTVar cache $ HashMap.insert nada mempty
  commits <- newTVarIO mempty

  let actors = [ 1 .. fromIntegral (cliActorsNum opts) ] :: [Actor]

  ee <- forM actors $ \a -> do
          (a,) <$> newTQueueIO

  tstates <- newTVarIO (mempty :: HashMap Actor MyState)

  async $ forever $ do
    let w = cliWaitAnnounceTime opts
    let n = cliActorsNum opts

    let q =  max 1 $ round $ realToFrac (n + 1) / 2

    pause @'Seconds (2 * realToFrac w)

    rs <- atomically $ readTVar tstates <&> HashMap.elems

    let votes = List.sortOn (Down . snd) $ HashMap.toList $ HashMap.fromListWith (+) [ (n,1) | n <- rs ]

    let dv = sum [ 1 | (_,x) <- votes, x < q ]

    case headMay votes of
      Just (s,l) | l >= q -> do
        z <- readTVarIO cache <&> HashMap.lookup (fromMyState s) <&> maybe 0 length
        notice $ annotate (color Green) $ "CONSENSUS" <+> pretty s <+> viaShow l <+> pretty z <+> viaShow dv

      _                   -> pure () -- err "NO CONSENSUS!"

    pure ()

  w1 <- forM actors $ \a -> async do

          mst <- newTVarIO s0

          runM (MyEnv a (HashMap.fromList ee) mst cache commits) do

            let pBad = cliPb opts
            let w = cliWaitAnnounceTime opts
            let pFuckup = cliPd opts
            let pT = cliPt opts
            let wT = cliWt opts

            qblf <- qblfInit @MyConsensus a actors s0 (realToFrac w)

            r1 <- async $ do
                    bad <- randomRIO (0.0, 1.00)

                    miniW <- randomRIO (0.001, 0.02)
                    pause @'Seconds (realToFrac miniW)

                    when (bad > (1 - pBad)) do
                      dt <- randomRIO (w/5, w*5)
                      pause @'Seconds (realToFrac dt)

                    qblfRun qblf

            a1 <- async $ forever do
                    pause @'Seconds (realToFrac wT)

                    pt <- liftIO $ randomRIO (0.00, 1.00 :: Double)

                    when (pt < pT) do
                      tran <- OddEven <$> liftIO randomIO
                      -- debug $ "I'm" <+> viaShow a <+> viaShow tran
                      qblfEnqueue qblf tran

            a2 <- async $ do
                    mbInbox <- asks myActors <&> HashMap.lookup a
                    maybe1 mbInbox (pure ()) $ \inbox -> forever do
                      atomically (readTQueue inbox) >>= qblfAcceptMessage qblf

                      let n = realToFrac (length actors)
                      fuckup <- liftIO $ randomRIO (0.00, 1.00) <&> (*n)

                      when (fuckup < pFuckup) do
                        debug $ "ACTOR" <> parens (pretty $ view qblfSelf qblf) <+> "DISASTER HAPPENED"
                        let wx = realToFrac $ view qblfWaitAnnounce qblf
                        dt <- liftIO $ randomRIO (wx*0.5, wx*3.5)
                        pause @'Seconds (realToFrac dt)

            a3 <- async $ forever do
                    pause @'Seconds (realToFrac (cliWaitAnnounceTime opts))
                    me <- ask
                    st@(MyState xs) <- asks myState >>= readTVarIO
                    n <- asks myStorage >>= liftIO . readTVarIO <&> HashMap.lookup xs <&> fromMaybe mempty <&> length

                    atomically $ modifyTVar tstates (HashMap.insert a st)

                    -- n <- qblfGetState qblf

                    -- -- h <- height
                    -- notice $ "ACTOR" <> parens (pretty (fromActor $ mySelf me))
                    --                  <> brackets (viaShow n)
                    --                  <> braces (pretty xs)


            mapM_ link [r1, a1, a2, a3]

            mapM_ wait [a1,a2,a3,r1]

  waitAnyCatchCancel w1

  putStrLn "WAT?"

