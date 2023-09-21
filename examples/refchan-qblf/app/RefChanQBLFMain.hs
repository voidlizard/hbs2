{-# Language TemplateHaskell #-}
{-# Language UndecidableInstances #-}
{-# Language AllowAmbiguousTypes #-}
module Main where

import HBS2.Prelude
import HBS2.Defaults
import HBS2.Merkle
import HBS2.Hash
import HBS2.Clock
import HBS2.Base58
import HBS2.OrDie
import HBS2.Data.Types.Refs
import HBS2.Net.Proto.Types
import HBS2.Actors.Peer
import HBS2.Net.Proto.RefChan
import HBS2.Net.Proto.AnyRef
import HBS2.Data.Types.SignedBox
import HBS2.Net.Messaging.Unix
import HBS2.Net.Proto.Definition
import HBS2.Data.Bundle
import HBS2.Net.Auth.Credentials
import HBS2.Data.Detect
import HBS2.Actors.Peer.Types()

import HBS2.Storage.Simple

import HBS2.System.Logger.Simple

import QBLF.Proto

import Demo.QBLF.Transactions
import Data.Config.Suckless
import Data.Config.Suckless.KeyValue

import Data.Ord
import Control.Monad.Trans.Maybe
import Codec.Serialise
import Control.Monad.Reader
import Data.ByteString(ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Functor
import Data.List qualified as List
import Lens.Micro.Platform hiding ((.=))
import Options.Applicative hiding (info)
import Options.Applicative qualified as O
import System.Directory
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Maybe
import Data.Word
import System.Random
import UnliftIO

import Data.Time.Clock.POSIX (getPOSIXTime)

import Data.Aeson hiding (json)
import Web.Scotty hiding (request,header)
import Web.Scotty qualified as Scotty
import Network.HTTP.Types.Status
import Data.Monoid (mconcat)
import Data.Cache (Cache)
import Data.Cache qualified as Cache

import Control.Monad.Except


import Streaming.Prelude qualified as S

{- HLINT ignore "Use newtype instead of data" -}

-- TODO: config
--  сделать конфиг, а то слишком много уже параметров в CLI

data HttpPortOpt
data RefChanOpt
data SocketOpt
data ActorOpt
data DefStateOpt
data StateRefOpt

data QBLFRefKey
type MyRefKey = AnyRefKey QBLFRefKey HBS2Basic

instance Monad m => HasCfgKey HttpPortOpt (Maybe Int) m where
  key = "http"


instance {-# OVERLAPPING #-} (HasConf m, HasCfgKey HttpPortOpt (Maybe Int) m) => HasCfgValue HttpPortOpt (Maybe Int) m where
  cfgValue  = val <$> getConf
    where
      val syn = lastMay [ fromIntegral e
                | ListVal @C (Key s [LitIntVal e]) <- syn, s == key @HttpPortOpt @(Maybe Int) @m
                ]

instance Monad m => HasCfgKey RefChanOpt (Maybe String) m where
  key = "refchan"

instance Monad m => HasCfgKey SocketOpt (Maybe String) m where
  key = "socket"

instance Monad m => HasCfgKey ActorOpt (Maybe String) m where
  key = "actor"

instance Monad m => HasCfgKey DefStateOpt (Maybe String) m where
  key = "default-state"

instance Monad m => HasCfgKey StateRefOpt (Maybe String) m where
  key = "state-ref"

class ToBalance e tx where
  toBalance :: tx -> [(Account e, Amount)]

tracePrefix :: SetLoggerEntry
tracePrefix  = toStderr . logPrefix "[trace] "

debugPrefix :: SetLoggerEntry
debugPrefix  = toStderr . logPrefix "[debug] "

errorPrefix :: SetLoggerEntry
errorPrefix  = toStderr . logPrefix "[error] "

warnPrefix :: SetLoggerEntry
warnPrefix   = toStderr . logPrefix "[warn] "

noticePrefix :: SetLoggerEntry
noticePrefix = toStderr . logPrefix "[notice] "

infoPrefix :: SetLoggerEntry
infoPrefix = toStdout . logPrefix ""

silently :: MonadIO m => m a -> m ()
silently m = do
  setLoggingOff @DEBUG
  setLoggingOff @INFO
  setLoggingOff @ERROR
  setLoggingOff @WARN
  setLoggingOff @NOTICE
  void m

withLogging :: MonadIO m => m a -> m ()
withLogging m = do

  -- setLogging @TRACE  tracePrefix
  setLogging @DEBUG  debugPrefix
  setLogging @INFO   infoPrefix
  setLogging @ERROR  errorPrefix
  setLogging @WARN   warnPrefix
  setLogging @NOTICE noticePrefix

  m

  setLoggingOff @DEBUG
  setLoggingOff @INFO
  setLoggingOff @ERROR
  setLoggingOff @WARN
  setLoggingOff @NOTICE

data MyEnv =
  MyEnv
  { mySelf  :: Peer UNIX
  , myFab   :: Fabriq UNIX
  , myChan  :: RefChanId UNIX
  , myRef   :: MyRefKey
  , mySto   :: AnyStorage
  , myCred  :: PeerCredentials HBS2Basic
  , myHttpPort :: Int
  , myFetch :: Cache HashRef ()
  }

newtype App m a = App { fromApp :: ReaderT MyEnv m a }
                  deriving newtype ( Functor
                                   , Applicative
                                   , Monad
                                   , MonadIO
                                   , MonadUnliftIO
                                   , MonadReader MyEnv
                                   , MonadTrans
                                   )

runApp :: (MonadIO m, PeerMessaging UNIX) => MyEnv -> App m a -> m a
runApp env m = runReaderT (fromApp  m) env

instance Monad m => HasFabriq UNIX (App m) where
  getFabriq = asks myFab

instance Monad m => HasOwnPeer UNIX (App m) where
  ownPeer = asks mySelf

instance Monad m => HasStorage (App m) where
  getStorage = asks mySto

data ConsensusQBLF

data StateQBLF = StateQBLF { fromStateQBLF :: HashRef }

data MyError =
  DeserializationError | SignatureError | TxUnsupported | SomeOtherError
  deriving stock (Eq,Ord,Show)

check :: MonadIO m => MyError -> Either e a -> ExceptT MyError m a
check w  = \case
  Right x -> ExceptT $ pure (Right x)
  Left{}  -> ExceptT $ pure (Left w)

fiasco :: MonadIO m => MyError -> ExceptT MyError m a
fiasco x = ExceptT $ pure $ Left x

ok :: MonadIO m => a -> ExceptT MyError m a
ok x = ExceptT $ pure $ Right x


type ForConsensus m = (MonadIO m, Serialise (QBLFMessage ConsensusQBLF))

instance Serialise (QBLFMerge ConsensusQBLF)
instance Serialise (QBLFMessage ConsensusQBLF)
instance Serialise (QBLFAnnounce ConsensusQBLF)
instance Serialise (QBLFCommit ConsensusQBLF)

instance Monad m => HasTimeLimits UNIX (RefChanNotify UNIX) (App m) where
  tryLockForPeriod _ _ = pure True

instance (ForConsensus m)  => IsQBLF ConsensusQBLF (App m) where
  type QBLFActor ConsensusQBLF = Actor L4Proto
  type QBLFTransaction ConsensusQBLF = QBLFDemoToken L4Proto
  type QBLFState ConsensusQBLF = DAppState

  qblfMoveForward _ s1 = do
    env <- ask
    fetchMissed env s1
    pure True

  qblfNewState (DAppState h0) txs = do
    sto <- asks mySto
    chan <- asks myChan
    self <- asks mySelf
    creds <- asks myCred
    let sk = view peerSignSk creds
    let pk = view peerSignPk creds

    -- let block = serialise (HashSet.toList $ HashSet.fromList txs)

    hashes <- liftIO $ mapM (putBlock sto . serialise) txs <&> catMaybes

    -- пробуем разослать бандлы с транзакциями
    runMaybeT do
      ref <- MaybeT $ createBundle sto (fmap HashRef hashes)
      let refval = makeBundleRefValue @L4Proto pk sk (BundleRefSimple ref)
      r <- MaybeT $ liftIO $ putBlock sto (serialise refval)
      lift $ request self (ActionRequest @UNIX chan (RefChanAnnounceBlock (HashRef r)))

    current <- readLog (getBlock sto) h0

    -- основная проблема в том, что мы пересортировываем весь state
    -- однако, если считать его уже отсортированным, то, может быть,
    -- все будет не так уж плохо.
    -- так-то мы можем вообще его на диске держать
    let new = HashSet.fromList ( current <> fmap HashRef hashes )

    let pt = toPTree (MaxSize 256) (MaxNum 256) (HashSet.toList new)

    root <- if List.null hashes then do
              pure h0
            else do
              r <- makeMerkle 0 pt $ \(hx,_,bs) -> do
                th <- liftIO (enqueueBlock sto bs)
                debug $ "WRITE TX" <+> pretty hx

              request self (ActionRequest @UNIX chan (RefChanAnnounceBlock (HashRef r)))

              pure (HashRef r)

    debug $ "PROPOSED NEW STATE:" <+> pretty root
    pure $ DAppState root

  qblfCommit s0 s1 = do
    debug $ "COMMIT:" <+> pretty s0 <+> pretty s1
    sto <- asks mySto
    chan <- asks myChan
    ref  <- asks myRef

    debug $ "UPDATING REF" <+> pretty ref <+> pretty s1
    liftIO $ updateRef sto ref (fromHashRef (fromDAppState s1))
    pure ()

  qblfBroadCast msg = do
      self <- asks mySelf
      creds <- asks myCred
      chan <- asks myChan

      let sk = view peerSignSk creds
      let pk = view peerSignPk creds
      nonce <- randomIO @Word64 <&> serialise <&> LBS.toStrict
      let box = makeSignedBox @UNIX pk sk (LBS.toStrict (serialise msg) <> nonce)
      let notify = Notify @UNIX chan box
      request self notify

      case msg of
        QBLFMsgAnn _ (QBLFAnnounce _ _) -> do
          -- TODO: maybe-announce-new-state-here
          pure ()

        _ -> none

  -- TODO: optimize-qblf-merge
  --   будет нормально работать до десятков/сотен тысяч транз,
  --   а потом помрёт.
  --   варианты:
  --     1. перенести логику в БД
  --     2. кэшировать всё, что можно
  qblfMerge s0 s1 = do

    chan <- asks myChan

    debug $ "MERGE. Proposed state:" <+> pretty s1

    sto <- asks mySto
    let readFn = liftIO . getBlock sto

    tx0 <- readLog readFn (fromDAppState s0) <&> HashSet.fromList
    tx1 <- mapM (readLog readFn) (fmap fromDAppState s1) <&> mconcat

    debug $ "READ TXS" <+> pretty s1 <+> pretty (length tx1)

    r <- forM tx1 $ \t -> runMaybeT do

            -- игнорируем ранее добавленные транзакции
            guard (not (HashSet.member t tx0))

            bs <- MaybeT $ liftIO $ getBlock sto (fromHashRef t)

            tx <- MaybeT $ pure $ deserialiseOrFail @(QBLFDemoToken L4Proto) bs & either (const Nothing) Just

            case tx of
              Emit box -> do
                (pk, e@(EmitTx a q _)) <- MaybeT $ pure $ unboxSignedBox0 @(EmitTx L4Proto) box
                guard ( chan == pk )
                debug $ "VALID EMIT TRANSACTION" <+> pretty t <+> pretty (AsBase58 a) <+> pretty q
                pure ([(t,e)], mempty)

              (Move box) -> do
                (_, m@(MoveTx _ _ qty _)) <- MaybeT $ pure $ unboxSignedBox0 @(MoveTx L4Proto) box

                guard (qty > 0)
                debug $ "MOVE TRANSACTION" <+> pretty t
                pure (mempty, [(t,m)])

    let parsed = catMaybes r

    let emits = foldMap (view _1) parsed

    let moves = foldMap (view _2) parsed & List.sortOn fst

    bal0 <- balances (fromDAppState s0)

    -- баланс с учётом новых emit
    let balE = foldMap (toBalance @L4Proto . snd) emits
                      & HashMap.fromListWith (+)
                      & HashMap.unionWith (+) bal0

    let moves' = updBalances @L4Proto balE moves

    let merged = fmap fst emits <> fmap fst moves'

    let pt = toPTree (MaxSize 256) (MaxNum 256) (HashSet.toList (tx0 <> HashSet.fromList merged))

    root <- makeMerkle 0 pt $ \(_,_,bs) -> do
              void $ liftIO (putBlock sto bs)

    let new = DAppState (HashRef root)

    -- FIXME: garbage-collect-discarded-states

    debug $ "MERGED" <+> pretty new

    pure new


instance (HasConf (ReaderT Config IO)) where
  getConf = ask

instance HasStorage (ReaderT AnyStorage IO) where
  getStorage = ask



instance ToBalance e (EmitTx e) where
  toBalance (EmitTx a qty _) = [(a, qty)]

instance ToBalance e (MoveTx e) where
  toBalance (MoveTx a1 a2 qty _) = [(a1, -qty), (a2, qty)]

balances :: forall e m . ( e ~ L4Proto
                         , MonadIO m
                         , HasStorage m
                         , ToBalance L4Proto (EmitTx L4Proto)
                         , ToBalance L4Proto (MoveTx L4Proto)
                         )
         => HashRef
         -> m (HashMap (Account e) Amount)

balances root = do
  sto <- getStorage
  txs <- readLog (liftIO . getBlock sto) root

  r <- forM txs $ \h -> runMaybeT do
    blk <- MaybeT $ liftIO $ getBlock sto (fromHashRef h)
    tx  <- MaybeT $ pure $ deserialiseOrFail @(QBLFDemoToken L4Proto) blk & either (const Nothing) Just

    case tx of
      Emit box -> do
        (_, emit) <- MaybeT $ pure $  unboxSignedBox0 @(EmitTx L4Proto) box
        pure $ toBalance @e emit

      Move box -> do
        (_, move) <- MaybeT $ pure $  unboxSignedBox0 @(MoveTx L4Proto) box
        pure $ toBalance @e move

  pure $ catMaybes r & mconcat & HashMap.fromListWith (+)


-- TODO: optimize-upd-balances
--  можно сгруппировать по аккаунтам
--  и проверять только те транзакции, которые относятся
--  к связанной (транзакциями) группе аккаунтов.
--  то есть, разбить на кластеры, у которых отсутствуют пересечения по
--  аккаунтам и проверять независимо и параллельно, например
--  причем, прямо этой функцией
--
-- updBalances :: HashMap (Account L4Proto) Amount
--              -> [(tx, b)]
--              -> [(tx, b)]

updBalances :: forall e a tx . (ForRefChans e, ToBalance e tx)
            => HashMap (Account e) Amount
            -> [(a, tx)]
            -> [(a, tx)]

updBalances = go
  where
    go bal [] = empty

    go bal (t:rest) =

      if good then
        t : go nb rest
      else
        go bal rest

      where
        nb = HashMap.unionWith (+) bal (HashMap.fromList (toBalance @e (snd t)))
        good = HashMap.filter (<0) nb & HashMap.null


fetchMissed :: forall e w m . ( MonadIO m
                              , Request e (RefChanNotify e) m
                              , e ~ UNIX
                              , w ~ ConsensusQBLF
                              )
            => MyEnv
            -> QBLFState w
            -> m ()

fetchMissed env s = do
  let tube = mySelf env
  let chan = myChan  env
  let cache = myFetch env
  let sto = mySto env

  let href = fromDAppState s

  here <- liftIO $ hasBlock sto (fromHashRef href) <&> isJust
  wip <- liftIO $ Cache.lookup cache href <&> isJust

  when here do
    liftIO $ Cache.delete cache href

  unless (here && not wip) do
    debug $ "We might be need to fetch" <+> pretty s
    liftIO $ Cache.insert cache href ()
    request @UNIX tube (ActionRequest @UNIX chan (RefChanFetch (fromDAppState s)))

runMe :: ForConsensus IO => Config -> IO ()
runMe conf = withLogging $ flip runReaderT conf do

  debug $ "runMe" <+> pretty conf

  kr    <- cfgValue @ActorOpt @(Maybe String) `orDie` "actor's key not set"
  chan' <- cfgValue @RefChanOpt @(Maybe String) `orDie` "refchan not set"
  sa    <- cfgValue @SocketOpt @(Maybe String) `orDie` "socket not set"
  pno   <- cfgValue @HttpPortOpt @(Maybe Int) <&> fromMaybe 3011
  ds    <- cfgValue @DefStateOpt @(Maybe String)

  ref   <- ( cfgValue @StateRefOpt @(Maybe String)
                <&>  maybe Nothing fromStringMay
           ) `orDie` "state-ref not set"

  sc <- liftIO $ BS.readFile kr
  creds <- pure (parseCredentials @HBS2Basic (AsCredFile sc)) `orDie` "bad keyring file"

  chan  <- pure (fromStringMay @(RefChanId L4Proto) chan') `orDie` "invalid REFCHAN"

  here <- liftIO $ doesFileExist sa

  when here do
    liftIO $ removeFile sa

  server <- newMessagingUnix True 1.0 sa

  abus <- async $ runMessagingUnix server

  let tube = fromString sa

  -- FIXME: fix-default-storage
  xdg <- liftIO $ getXdgDirectory XdgData defStorePath <&> fromString
  sto' <- simpleStorageInit @HbSync [StoragePrefix xdg]

  let sto = AnyStorage sto'
  sw <- liftIO $ replicateM 4 $ async $ simpleStorageWorker sto'

  -- FIXME: fix-hardcoded-timeout
  fetches <- liftIO $ Cache.newCache (Just (toTimeSpec (TimeoutSec 30)))

  let myEnv = MyEnv tube
                    (Fabriq server)
                    chan
                    ref
                    sto
                    creds
                    pno
                    fetches

  let dss = ds >>= fromStringMay

  s0 <- readOrCreateStateRef dss sto ref

  debug $ "STATE0:" <+> pretty s0

  -- получить голову
  -- из головы получить акторов

  headBlk <- getRefChanHead @L4Proto sto (RefChanHeadKey chan) `orDie` "can't read head block"

  let self = view peerSignPk creds & Actor @L4Proto

  let actors = view refChanHeadAuthors headBlk
                  & HashSet.toList
                  & fmap (Actor @L4Proto)

  runApp myEnv do

    -- FIXME: timeout-hardcode
    let w = realToFrac 5

    -- FIXME: use-actors-asap
    qblf <- qblfInit @ConsensusQBLF self actors (DAppState (HashRef s0)) w

    consensus <- async do
                   pause @'Seconds 0.5
                   qblfRun qblf


    -- FIXME: web-port-to-config
    web <- async $ liftIO $ scotty (fromIntegral (myHttpPort myEnv)) $ do
             post "/tx" $ do

              r <- runExceptT do

                bin <- lift body
                let hBin = hashObject @HbSync bin

                debug $ "GOT TX" <+> pretty hBin

                tok <- check DeserializationError =<< pure (deserialiseOrFail @(QBLFDemoToken L4Proto) bin)

                tx <- case tok of
                        (Emit box) -> do
                          (sign, tx) <- maybe (ExceptT $ pure $ Left SignatureError) pure $  unboxSignedBox0 box

                          if sign == chan then
                            pure hBin
                          else
                            fiasco SignatureError

                        (Move box) -> do
                          (sign, tx) <- maybe (ExceptT $ pure $ Left SignatureError) pure $  unboxSignedBox0 box
                          pure hBin

                qblfEnqueue qblf tok
                pure hBin

              case r of
                Left SignatureError -> do
                  err $ viaShow SignatureError
                  status status401

                Left e -> do
                  err $ viaShow e
                  status status400

                Right tx -> do
                  debug $ "TX ENQUEUED OK" <+> pretty tx
                  status status200

    link web

    runProto $ List.singleton $ makeResponse (myProto myEnv qblf chan)

  void $ waitAnyCatchCancel $ [abus] <> sw

  where

    myProto :: forall e m . ( MonadIO m
                            , Request e (RefChanNotify e) m
                            , e ~ UNIX
                            )
                       => MyEnv
                       -> QBLF ConsensusQBLF
                       -> RefChanId e
                       -> RefChanNotify e
                       -> m ()

    myProto _ qblf _ (ActionRequest{}) = do
      pure ()

    myProto env qblf chan (Notify _ msg) = do

      let sto = mySto env
      let tube = mySelf env

      let coco = hashObject @HbSync $ serialise msg
      void $ runMaybeT do
        (_, wrapped) <- MaybeT $ pure $ unboxSignedBox0 @ByteString @UNIX msg
        qbmess <- MaybeT $ pure $ deserialiseOrFail @(QBLFMessage ConsensusQBLF) (LBS.fromStrict wrapped)
                                              & either (const Nothing) Just

        states <- case qbmess of
          QBLFMsgAnn _ (QBLFAnnounce s0 s1) -> do
            pure [s0, s1]

          QBLFMsgHeartBeat _ _ s0 _-> do
            pure  [s0]

          _ -> do
            pure mempty

        -- FIXME: full-download-guarantee
        lift $ forM_ states (fetchMissed env)

        qblfAcceptMessage qblf qbmess
        -- debug $ "RefChanQBLFMain(3)" <+> "got message" <+> pretty (AsBase58 chan) <+> pretty coco

    readOrCreateStateRef mbDs sto ref = do
      debug $ "MyRef:" <+> pretty (hashObject @HbSync ref)
      fix \spin -> do
                mbref <- liftIO $ getRef @_ @HbSync sto ref
                case mbref of
                  Nothing -> do
                    debug "STATE is empty"
                    maybe1 mbDs none $ \ds -> do
                      debug $ "UPDATE REF" <+> pretty (hashObject @HbSync ref) <+> pretty (HashRef ds)
                      liftIO $ updateRef sto ref ds

                    pause @'Seconds 0.25

                    spin

                  Just val -> do
                    pure val

type Config = [Syntax MegaParsec]

main :: IO ()
main = join . customExecParser (prefs showHelpOnError) $
  O.info (helper <*> globalOptions)
  (  fullDesc
  <> header "refchan-qblf-worker"
  <> progDesc "for test and demo purposed"
  )
  where

    globalOptions = applyConfig <$> commonOpts <*> cli

    applyConfig :: Maybe FilePath -> (Config -> IO ()) -> IO ()
    applyConfig config m = do
      maybe1 config (m mempty) $ \conf -> do
        top <- readFile conf <&> parseTop <&> either (pure mempty) id
        m top

    commonOpts = optional $ strOption (long "config" <> short 'c' <> help "Config file")

    cli = hsubparser (  command "run" (O.info pRun (progDesc "run qblf servant" ) )
                     <> command "gen" (O.info pGen (progDesc "generate transcation") )
                     <> command "post" (O.info pPostTx (progDesc "post transaction") )
                     <> command "check" (O.info pCheckTx (progDesc "check transaction") )
                     <> command "balances" (O.info pBalances (progDesc "show balances") )
                     )

    pRun = do
      pure runMe

    pGen = hsubparser
           (   command "tx-emit" ( O.info pGenEmit (progDesc "generate emit") )
            <> command "tx-move" ( O.info pGenMove (progDesc "generate move") )
           )

    pGenEmit = do
      kr    <- strOption   ( long "keyring"  <> short 'k' <> help "keyring file" )
      amnt <- option @Amount auto ( long "amount" <> short 'n' <> help "amount" )
      dest <- strArgument ( metavar "ADDRESS" )
      pure $ const $ silently do
        sc <- BS.readFile kr
        creds <- pure (parseCredentials @HBS2Basic (AsCredFile sc)) `orDie` "bad keyring file"
        let pk = view peerSignPk creds
        let sk = view peerSignSk creds
        acc <- pure (fromStringMay @(RefChanId L4Proto) dest) `orDie` "bad account address"
        tx <- makeEmitTx @L4Proto pk sk acc amnt
        LBS.putStr $ serialise tx

    pGenMove = do
      kr    <- strOption   ( long "wallet"  <> short 'w' <> help "wallet (keyring) file" )
      amnt <- option @Amount auto ( long "amount" <> short 'n' <> help "amount" )
      dest <- strArgument ( metavar "ADDRESS" )
      pure $ const $ silently do
        sc <- BS.readFile kr
        creds <- pure (parseCredentials @HBS2Basic (AsCredFile sc)) `orDie` "bad keyring file"
        let pk = view peerSignPk creds
        let sk = view peerSignSk creds
        acc <- pure (fromStringMay @(RefChanId L4Proto) dest) `orDie` "bad account address"
        tx <- makeMoveTx @L4Proto pk sk acc amnt
        LBS.putStr $ serialise tx

    pCheckTx = do
      kr    <- strOption   ( long "keyring"  <> short 'k' <> help "keyring file" )
      pure $ const do
        sc <- BS.readFile kr
        creds <- pure (parseCredentials @HBS2Basic (AsCredFile sc)) `orDie` "bad keyring file"
        let pk = view peerSignPk creds
        let sk = view peerSignSk creds

        tx <- LBS.getContents <&> deserialise @(QBLFDemoToken L4Proto)

        case tx of
          Emit box -> do
            void $ pure (unboxSignedBox0 @(EmitTx L4Proto) @L4Proto box) `orDie` "bad emit tx"

          Move box -> do
            void $ pure (unboxSignedBox0 @(MoveTx L4Proto) @L4Proto box) `orDie` "bad move tx"

        pure ()

    pPostTx  = pure $ const do
      error "not supported anymore / TODO via http"
      -- rc <- strArgument ( metavar "REFCHAN" )
      -- sa <- strArgument ( metavar "UNIX-SOCKET" ) <&> fromString
      -- pure $ withLogging do
      --   rchan <- pure (fromStringMay @(RefChanId L4Proto) rc) `orDie` "bad refchan"
      --   print "JOPA"
      --   -- FIXME: wrap-client-boilerplate
      --   inbox <- newMessagingUnix False 1.0 sa
      --   wInbox <- async $ runMessagingUnix inbox
      --   let env = MyEnv (fromString sa) (Fabriq inbox) rchan
      --   msg <- (LBS.getContents <&> deserialiseOrFail) `orDie` "transaction decode error"
      --   runApp env do
      --     request (mySelf env) (msg  :: QBLFDemoTran UNIX)
      --     pause @'Seconds 0.1
        -- cancel wInbox

    pBalances = do
      state <- strArgument ( metavar "STATE" )
      pure $ const $ withLogging do

        xdg <- liftIO $ getXdgDirectory XdgData defStorePath <&> fromString
        sto' <- simpleStorageInit @HbSync [StoragePrefix xdg]

        let sto = AnyStorage sto'
        sw <- liftIO $ replicateM 4 $ async $ simpleStorageWorker sto'

        root <- pure (fromStringMay @HashRef state) `orDie` "Bad STATE reference"

        flip runReaderT sto $ do
          debug $ "calculating balances for" <+> pretty root
          bal <- balances root

          forM_ (HashMap.toList bal) $ \(acc, qty) -> do
            liftIO $ print $ pretty (AsBase58 acc) <+> pretty qty

