{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language TemplateHaskell #-}
{-# Language UndecidableInstances #-}
{-# Language AllowAmbiguousTypes #-}
module RefChanQBLF.Impl where

import HBS2.Actors.Peer
import HBS2.Actors.Peer.Types()
import HBS2.Base58
import HBS2.Data.Bundle
import HBS2.Data.Detect
import HBS2.Data.Types.Refs
import HBS2.Data.Types.SignedBox
import HBS2.Hash
import HBS2.Merkle
import HBS2.Net.Auth.Credentials
import HBS2.Net.Messaging.Unix
import HBS2.Net.Proto.QBLF
import HBS2.Peer.Proto.AnyRef
import HBS2.Peer.Proto.RefChan
import HBS2.Prelude
import HBS2.Storage.Simple
import HBS2.System.Logger.Simple

import RefChanQBLF.Common
import RefChanQBLF.Transactions
import Data.Config.Suckless
import Control.Monad.Except
import Control.Monad.Trans.Maybe
import Codec.Serialise
import Control.Monad.Reader
import Data.ByteString.Lazy qualified as LBS
import Data.List qualified as List
import Lens.Micro.Platform hiding ((.=))
import Options.Applicative hiding (info)
import Data.HashSet qualified as HashSet
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Maybe
import Data.Word
import System.Random
import UnliftIO

import Data.Cache (Cache)
import Data.Cache qualified as Cache

{- HLINT ignore "Use newtype instead of data" -}

-- TODO: config
--  сделать конфиг, а то слишком много уже параметров в CLI

data AppSocketOpt
data RefChanOpt
data SocketOpt
data ActorOpt
data DefStateOpt
data StateRefOpt

data QBLFRefKey
type MyRefKey = AnyRefKey QBLFRefKey 'HBS2Basic

instance HasCfgKey AppSocketOpt (Maybe String) where
  key = "app-socket"

instance HasCfgKey RefChanOpt (Maybe String)  where
  key = "refchan"

instance HasCfgKey SocketOpt (Maybe String)  where
  key = "socket"

instance HasCfgKey ActorOpt (Maybe String)  where
  key = "actor"

instance HasCfgKey DefStateOpt (Maybe String) where
  key = "default-state"

instance HasCfgKey StateRefOpt (Maybe String) where
  key = "state-ref"

class ToBalance s tx where
  toBalance :: tx -> [(Account s, Amount)]

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
  , myCred  :: PeerCredentials 'HBS2Basic
  -- , myHttpPort :: Int
  , myFetch :: Cache HashRef ()
  }

newtype MyAppT m a = MyAppT { fromQAppT :: ReaderT MyEnv m a }
                  deriving newtype ( Functor
                                   , Applicative
                                   , Monad
                                   , MonadIO
                                   , MonadUnliftIO
                                   , MonadReader MyEnv
                                   , MonadTrans
                                   )

runMyAppT :: (MonadIO m, PeerMessaging UNIX) => MyEnv -> MyAppT m a -> m a
runMyAppT env m = runReaderT (fromQAppT m) env

instance Monad m => HasFabriq UNIX (MyAppT m) where
  getFabriq = asks myFab

instance Monad m => HasOwnPeer UNIX (MyAppT m) where
  ownPeer = asks mySelf

instance Monad m => HasStorage (MyAppT m) where
  getStorage = asks mySto

data ConsensusQBLF

data StateQBLF = StateQBLF { fromStateQBLF :: HashRef }

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

instance Monad m => HasTimeLimits UNIX (RefChanNotify UNIX) (MyAppT m) where
  tryLockForPeriod _ _ = pure True

instance (ForConsensus m, MonadUnliftIO m)  => IsQBLF ConsensusQBLF (MyAppT m) where
  type QBLFActor ConsensusQBLF = Actor 'HBS2Basic
  type QBLFTransaction ConsensusQBLF = QBLFDemoToken 'HBS2Basic
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

    -- основная проблема в том, что мы пересортировываем весь state
    -- однако, если считать его уже отсортированным, то, может быть,
    -- все будет не так уж плохо.
    -- так-то мы можем вообще его на диске держать

    root <- if List.null txs then do
              pure h0
            else do
              hashes <- liftIO $ mapM (putBlock sto . serialise) txs <&> catMaybes

              current <- readLogThrow (getBlock sto) h0

              let new = HashSet.fromList ( current <> fmap HashRef hashes )

              let pt = toPTree (MaxSize 256) (MaxNum 256) (HashSet.toList new)

              -- пробуем разослать бандлы с транзакциями
              runMaybeT do
                ref <- MaybeT $ createBundle sto (fmap HashRef hashes)
                let refval = makeBundleRefValue @'HBS2Basic pk sk (BundleRefSimple ref)
                r <- MaybeT $ liftIO $ putBlock sto (serialise refval)
                lift $ request self (ActionRequest @UNIX chan (RefChanAnnounceBlock (HashRef r)))

              r <- makeMerkle 0 pt $ \(hx,_,bs) -> do
                _th <- liftIO (enqueueBlock sto bs)
                debug $ "WRITE TX" <+> pretty hx

              request self (ActionRequest @UNIX chan (RefChanAnnounceBlock (HashRef r)))

              pure (HashRef r)

    debug $ "PROPOSED NEW STATE:" <+> pretty root
    pure $ DAppState root

  qblfCommit s0 s1 = do
    debug $ "COMMIT:" <+> pretty s0 <+> pretty s1
    sto <- asks mySto
    _chan <- asks myChan
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
      let box = makeSignedBox pk sk (LBS.toStrict (serialise msg) <> nonce)
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
    _self <- asks mySelf

    creds <- asks myCred
    let _sk = view peerSignSk creds
    let _pk = view peerSignPk creds

    debug $ "MERGE. Proposed state:" <+> pretty s1

    sto <- asks mySto
    let readFn = liftIO . getBlock sto

    tx1 <- mapM (readLogThrow readFn) (fmap fromDAppState s1) <&> mconcat
    tx0 <- readLogThrow readFn (fromDAppState s0) <&> HashSet.fromList

    let txNew = HashSet.fromList tx1 `HashSet.difference` tx0

    if List.null txNew then do
      pure s0
    else do
      debug $ "READ TXS" <+> pretty s1 <+> pretty (length tx1)

      r <- forM tx1 $ \t -> runMaybeT do

              -- игнорируем ранее добавленные транзакции
              guard (not (HashSet.member t tx0))

              bs <- MaybeT $ liftIO $ getBlock sto (fromHashRef t)

              tx <- MaybeT $ pure $ deserialiseOrFail @(QBLFDemoToken 'HBS2Basic) bs & either (const Nothing) Just

              case tx of
                Emit box -> do
                  (pk', e@(EmitTx a q _)) <- MaybeT $ pure $ unboxSignedBox0 @(EmitTx 'HBS2Basic) box
                  guard ( chan == pk' )
                  debug $ "VALID EMIT TRANSACTION" <+> pretty t <+> pretty (AsBase58 a) <+> pretty q
                  pure ([(t,e)], mempty)

                (Move box) -> do
                  (_, m@(MoveTx _ _ qty _)) <- MaybeT $ pure $ unboxSignedBox0 @(MoveTx 'HBS2Basic) box

                  guard (qty > 0)
                  debug $ "MOVE TRANSACTION" <+> pretty t
                  pure (mempty, [(t,m)])

      let parsed = catMaybes r

      let emits = foldMap (view _1) parsed

      let moves = foldMap (view _2) parsed & List.sortOn fst

      bal0 <- balances (fromDAppState s0)

      -- баланс с учётом новых emit
      let balE = foldMap (toBalance @'HBS2Basic. snd) emits
                        & HashMap.fromListWith (+)
                        & HashMap.unionWith (+) bal0

      let moves' = updBalances @L4Proto balE moves

      let merged = fmap fst emits <> fmap fst moves'

      let pt = toPTree (MaxSize 256) (MaxNum 256) (HashSet.toList (tx0 <> HashSet.fromList merged))

      root <- makeMerkle 0 pt $ \(_,_,bs) -> do
                void $ liftIO (putBlock sto bs)

      let new = DAppState (HashRef root)

      -- FIXME: garbage-collect-discarded-states

      async $ void $ balances (fromDAppState new)
      debug $ "MERGED" <+> pretty new

      pure new


instance HasStorage (ReaderT AnyStorage IO) where
  getStorage = ask



instance ToBalance e (EmitTx e) where
  toBalance (EmitTx a qty _) = [(a, qty)]

instance ToBalance e (MoveTx e) where
  toBalance (MoveTx a1 a2 qty _) = [(a1, -qty), (a2, qty)]

balances :: forall e s m . ( e ~ L4Proto
                           , MonadIO m
                           , HasStorage m
                           -- , FromStringMaybe (PubKey 'Sign s)
                           , s ~ Encryption e
                           , ToBalance s (EmitTx s)
                           , ToBalance s (MoveTx s)
                           , Pretty (AsBase58 (PubKey 'Sign s))
                           )
         => HashRef
         -> m (HashMap (Account s) Amount)

balances root = do
  sto <- getStorage

  let pk = SomeRefKey (HashRef "6ChGmfYkwM6646oKkj8r8MAjdViTsdtZSi6tgqk3tbh", root)

  cached <- runMaybeT do
    rval <- MaybeT $ liftIO $ getRef sto pk
    val  <- MaybeT $ liftIO $ getBlock sto rval
    MaybeT $ deserialiseOrFail @(HashMap (Account s) Amount) val
          & either (const $ pure Nothing) (pure . Just)

  case cached of
    Just bal -> pure bal
    Nothing -> do

      txs <- readLogThrow (liftIO . getBlock sto) root

      r <- forM txs $ \h -> runMaybeT do
        blk <- MaybeT $ liftIO $ getBlock sto (fromHashRef h)
        tx  <- MaybeT $ pure $ deserialiseOrFail @(QBLFDemoToken s) blk & either (const Nothing) Just

        case tx of
          Emit box -> do
            (_, emit) <- MaybeT $ pure $  unboxSignedBox0 @(EmitTx s) box
            pure $ toBalance @s emit

          Move box -> do
            (_, move) <- MaybeT $ pure $  unboxSignedBox0 @(MoveTx s) box
            pure $ toBalance @s move

      let val = catMaybes r & mconcat & HashMap.fromListWith (+)

      runMaybeT do
        checkComplete sto root >>= guard
        rv <- MaybeT $ liftIO $ putBlock sto (serialise val)
        liftIO $ updateRef sto pk rv

      pure val


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

updBalances :: forall e s a tx . (ForRefChans e, ToBalance s tx, s ~ Encryption e)
            => HashMap (Account s) Amount
            -> [(a, tx)]
            -> [(a, tx)]

updBalances = go
  where
    go _bal [] = empty

    go bal (t:rest) =

      if good then
        t : go nb rest
      else
        go bal rest

      where
        nb = HashMap.unionWith (+) bal (HashMap.fromList (toBalance @s (snd t)))
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

  unless (here || wip) do
    debug $ "We might be need to fetch" <+> pretty s
    liftIO $ Cache.insert cache href ()
    request @UNIX tube (ActionRequest @UNIX chan (RefChanFetch (fromDAppState s)))

