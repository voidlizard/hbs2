{-# Language UndecidableInstances #-}
{-# Language AllowAmbiguousTypes #-}
{-# Language TemplateHaskell #-}
{-# Language FunctionalDependencies #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
module HBS2.Peer.Proto.RefChan.RefChanUpdate where

import HBS2.Prelude.Plated
import HBS2.Hash
import HBS2.Clock
import HBS2.Net.Proto
import HBS2.Net.Auth.Credentials
import HBS2.Base58
import HBS2.Events
import HBS2.Peer.Proto.Peer
import HBS2.Peer.Proto.RefChan.Adapter
import HBS2.Net.Proto.Sessions
import HBS2.Data.Types.Refs
import HBS2.Data.Types.SignedBox
import HBS2.Actors.Peer.Types
import HBS2.Data.Types.Peer
import HBS2.Storage

import HBS2.Peer.Proto.RefChan.Types

import HBS2.System.Logger.Simple

import Codec.Serialise
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Identity
import Control.Monad.Trans.Maybe
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.Maybe
import Data.Word
import Data.Text qualified as Text
import Lens.Micro.Platform
import Data.Hashable hiding (Hashed)
import Type.Reflection (someTypeRep)
import Data.Time.Clock.POSIX (getPOSIXTime)

import UnliftIO

data ProposeTran e = ProposeTran HashRef (SignedBox ByteString (Encryption e)) -- произвольная бинарная транзакция,
                     deriving stock (Generic)                     -- подписанная ключом **АВТОРА**, который её рассылает

newtype AcceptTime = AcceptTime Word64
                     deriving stock (Eq,Ord,Data,Generic)
                     deriving newtype (Enum,Num,Real,Pretty,Integral)

instance Serialise AcceptTime

data AcceptTran e = AcceptTran1 HashRef HashRef -- ссылка на (ProposTran e)
                  | AcceptTran2 (Maybe AcceptTime) HashRef HashRef
                    deriving stock (Generic)

acceptTime :: SimpleGetter (AcceptTran e) (Maybe AcceptTime)
acceptTime = to getter
  where
    getter (AcceptTran1 _ _) = Nothing
    getter (AcceptTran2 a _ _) = a

unpackAcceptTran :: AcceptTran e -> (Maybe AcceptTime, HashRef, HashRef)
unpackAcceptTran (AcceptTran1 a b) = (Nothing, a, b)
unpackAcceptTran (AcceptTran2 t a b) = (t, a, b)

pattern AcceptTran :: Maybe AcceptTime -> HashRef -> HashRef -> AcceptTran e
pattern AcceptTran t a b <- (unpackAcceptTran -> (t, a, b))
  where
    AcceptTran Nothing a b = AcceptTran1 a b
    AcceptTran (Just t) a b = AcceptTran2 (Just t) a b
{-# COMPLETE AcceptTran #-}

instance ForRefChans e => Serialise (ProposeTran e)
instance ForRefChans e => Serialise (AcceptTran e)

data RefChanRound e =
  RefChanRound
  { _refChanRoundKey     :: HashRef -- ^ hash of the Propose transaction
  , _refChanHeadKey      :: RefChanHeadKey (Encryption e)
  , _refChanRoundTTL     :: TimeSpec
  , _refChanRoundClosed  :: TVar Bool
  , _refChanRoundPropose :: TVar (Maybe (ProposeTran e)) -- ^ propose transaction itself
  , _refChanRoundTrans   :: TVar (HashSet HashRef)
  , _refChanRoundAccepts :: TVar (HashMap (PubKey 'Sign (Encryption e)) ())
  }
  deriving stock (Typeable, Generic)

makeLenses 'RefChanRound

newtype instance SessionKey e (RefChanRound e) =
  RefChanRoundKey HashRef
  deriving stock (Generic, Eq, Typeable)
  deriving newtype (Pretty)

deriving newtype instance Hashable (SessionKey e (RefChanRound e))

type instance SessionData e (RefChanRound e) = RefChanRound e

instance Expires (SessionKey e (RefChanRound e)) where
  expiresIn _ = Just 300

data instance EventKey e (RefChanRound e) =
  RefChanRoundEventKey
  deriving (Generic,Typeable,Eq)

newtype instance Event e (RefChanRound e) =
  RefChanRoundEvent (SessionKey e (RefChanRound e))
  deriving (Typeable,Generic)
  deriving newtype (Pretty)

instance Typeable (RefChanRound e) => Hashable (EventKey e (RefChanRound e)) where
  hashWithSalt salt _ = hashWithSalt salt (someTypeRep p)
    where
      p = Proxy @(RefChanRound e)

instance EventType ( Event e (RefChanRound e) ) where
  isPersistent = True

instance Expires (EventKey e (RefChanRound e)) where
  expiresIn = const Nothing

-- TODO: find-out-sure-transaction-size
--   транзакция должна быть маленькая!
--   хочешь что-то большое просунуть -- шли хэши.
--   черт его знает, какой там останется пайлоад.
--   надо посмотреть. байт, небось, 400
data RefChanUpdate e =
    Propose (RefChanId e) (SignedBox (ProposeTran e) (Encryption e)) -- подписано ключом пира
  | Accept  (RefChanId e) (SignedBox (AcceptTran e) (Encryption e))  -- подписано ключом пира
  deriving stock (Generic)

instance ForRefChans e => Serialise (RefChanUpdate e)

data RefChanRequest e =
    RefChanRequest  (RefChanId e)
  | RefChanResponse (RefChanId e) HashRef
  deriving stock (Generic,Typeable)

instance ForRefChans e => Serialise (RefChanRequest e)

data instance EventKey e (RefChanRequest e) =
  RefChanRequestEventKey
  deriving (Generic,Typeable,Eq)

data instance Event e (RefChanRequest e) =
  RefChanRequestEvent (RefChanId e) HashRef
  deriving (Typeable,Generic)

instance EventType ( Event e (RefChanRequest e) ) where
  isPersistent = True

instance Expires (EventKey e (RefChanRequest e)) where
  expiresIn = const Nothing

instance Typeable (RefChanRequest e) => Hashable (EventKey e (RefChanRequest e)) where
  hashWithSalt salt _ = hashWithSalt salt (someTypeRep p)
    where
      p = Proxy @(RefChanRequest e)



type RefChanValidateNonce e = Nonce (RefChanValidate e)

data RefChanValidate e =
  RefChanValidate
  { rcvNonce :: Nonce (RefChanValidate e)
  , rcvChan  :: RefChanId e
  , rcvData  :: RefChanValidateData e
  }
  deriving stock (Generic)

data RefChanValidateData e =
    Validate HashRef
  | Accepted HashRef
  | Rejected HashRef
  | Poke
  deriving stock (Generic)

instance Serialise (RefChanValidateData e)

instance ( Serialise (PubKey 'Sign (Encryption e))
         , Serialise (Nonce (RefChanValidate e)) )
  => Serialise (RefChanValidate e)

instance ( ForRefChans e
         , Pretty (AsBase58 (Nonce (RefChanValidate e)))
         , Pretty (AsBase58 (PubKey 'Sign (Encryption e)))
         ) => Pretty (RefChanValidate e) where
  pretty (RefChanValidate n c d) = case d of
    Validate r -> pretty "validate" <+> pretty (AsBase58 n) <+> pretty (AsBase58 c) <+> pretty r
    Accepted r -> pretty "accepted" <+> pretty (AsBase58 n) <+> pretty (AsBase58 c) <+> pretty r
    Rejected r -> pretty "rejected" <+> pretty (AsBase58 n) <+> pretty (AsBase58 c) <+> pretty r
    Poke       -> pretty "poke"     <+> pretty (AsBase58 n) <+> pretty (AsBase58 c)



instance HasRefChanId e (RefChanUpdate e) where
  getRefChanId = \case
    Propose c _ -> c
    Accept  c _ -> c

instance HasRefChanId e (RefChanRequest e) where
  getRefChanId = \case
    RefChanRequest c -> c
    RefChanResponse c _ -> c


instance HasRefChanId e (RefChanValidate e) where
  getRefChanId = rcvChan


refChanUpdateProto :: forall e s m proto . ( MonadUnliftIO m
                                           , Request e proto m
                                           , Response e proto m
                                           , HasDeferred proto e m
                                           , IsPeerAddr e m
                                           , Pretty (Peer e)
                                           , Sessions e (KnownPeer e) m
                                           , Sessions e (RefChanHeadBlock e) m
                                           , Sessions e (RefChanRound e) m
                                           , EventEmitter e (RefChanRound e) m
                                           , HasStorage m
                                           , HasGossip e proto m
                                           , Signatures s
                                           , IsRefPubKey s
                                           , Pretty (AsBase58 (PubKey 'Sign s))
                                           , ForRefChans e
                                           , s ~ Encryption e
                                           , proto ~ RefChanUpdate e
                                           )
                   => Bool
                   -> PeerCredentials s
                   -> RefChanAdapter e m
                   -> RefChanUpdate e
                   -> m ()

refChanUpdateProto self pc adapter msg = flip withException (\e -> liftIO (print (e :: SomeException))) do
  -- авторизовать пира
  peer <- thatPeer @proto

  auth <- find (KnownPeerKey peer) id <&> isJust

  sto <- getStorage

  let pk = view peerSignPk pc
  let sk = view peerSignSk pc

  do

    guard' "auth || self" (auth || self)

    -- TODO: process-each-message-only-once
    -- где-то тут мы разбираемся, что такое сообщеине
    -- уже отправляли и больше одного раза не реагируем

    -- У нас тут получается раунд на каждый Propose
    -- Это может быть и хорошо и похо. Если очень
    -- много транзакций, это плохо. Если не очень
    -- то это нормально и можно обойтись без понятия
    -- "блок".
    -- так-то и количество proposers можно ограничить

    guard' "refChanSubscribed" =<< refChanSubscribed adapter (getRefChanId msg)

    let h0 = hashObject @HbSync (serialise msg)

    debug $ "RefchanUpdate: ALREADY" <+> pretty h0

    guard' ("has block " <> (Text.pack . show . pretty) h0) =<< liftIO (hasBlock sto h0 <&> isNothing)

    case msg of
     Propose chan box -> do

       debug $ "RefChanUpdate/Propose" <+> pretty h0

       deferred @proto do
       -- do

        -- проверили подпись пира
        (peerKey, ProposeTran headRef abox) <- unboxSignedBox0 box
            & justOrThrowIO "unbox signed box"

        -- проверили подпись автора
        (authorKey, _) <- unboxSignedBox0 abox
            & justOrThrowIO "unbox signed abox"

        -- итак, сначала достаём голову. как мы достаём голову?

        let refchanKey = RefChanHeadKey @s chan
        h <- liftIO (getRef sto refchanKey)
            & justMOrThrowIO "getref"
        -- смотрим, что у нас такая же голова.
        -- если нет -- значит, кто-то рассинхронизировался.
        -- может быть, потом попробуем головы запросить
        guard' "HashRef h == headRef" (HashRef h == headRef)

        debug $ "OMG! Got trans" <+> pretty (AsBase58 peerKey)  <+> pretty (AsBase58 authorKey)

        -- теперь достаём голову
        headBlock <- getActualRefChanHead @e refchanKey
            & justMOrThrowIO "getActualRefChanHead"

        let pips = view refChanHeadPeers headBlock

        guard' "checkACL" $ checkACL ACLUpdate headBlock (Just peerKey) authorKey

        debug $ "OMG!!! TRANS AUTHORIZED" <+> pretty (AsBase58 peerKey)  <+> pretty (AsBase58 authorKey)

        -- TODO: validate-transaction
        --  итак, как нам валидировать транзакцию?
        --  HTTP ?
        --  TCP ?
        --  UDP ? (кстати, годно и быстро)
        --  CLI ?
        --  получается, риалтайм: ждём не более X секунд валидации,
        --  иначе не валидируем.
        --  по хорошему,  не блокироваться бы в запросе.
        --  тут мы зависим от состояния конвейра, нас можно DDoS-ить
        --  большим количеством запросов и транзакции будут отклоняться
        --  при большом потоке.
        --  но решается это.. тадам! PoW! подбором красивых хэшей
        --  при увеличении нагрузки.
        --  тогда, правда, в открытой системе работает паттерн -- DDoS
        --  всех, кроме своих узлов, а свои узлы всё принимают.

        -- для начала: сделаем хук для валидации, которыйне будет делать ничего

        -- если не смогли сохранить транзу, то и Accept разослать
        -- не сможем
        -- это правильно, так как транза содержит ссылку на RefChanId
        -- следовательно, для другого рефчана будет другая транза

        hash <- liftIO (putBlock sto (serialise msg))
            & justMOrThrowIO "putBlock"

        ts <- liftIO getTimeCoarse

        let toWait = TimeoutSec (fromIntegral $ 2 * view refChanHeadWaitAccept headBlock)
        let ttl = ts + fromNanoSecs (fromIntegral $ toNanoSeconds toWait)

        let rcrk = RefChanRoundKey (HashRef hash)

        rndHere <- find rcrk id

        defRound <- RefChanRound @e (HashRef hash) refchanKey ttl
                       <$> newTVarIO False
                       <*> newTVarIO Nothing
                       <*> newTVarIO (HashSet.singleton (HashRef hash)) -- save propose
                       <*> newTVarIO (HashMap.singleton peerKey ())

        unless (isJust rndHere) do
          update defRound rcrk id
          emit @e RefChanRoundEventKey (RefChanRoundEvent rcrk)

        -- не обрабатывать propose, если он уже в процессе
        guard' "isNothing rndHere" (isNothing rndHere)

        -- FIXME: fixed-timeout-is-no-good
        validated <- either id id <$> ( race (pause @'Seconds 5 >> pure False)
                                               $ refChanValidatePropose adapter chan (HashRef hash)
                                           )
        -- почему так:
        --   мы можем тормозить в проверке транзакции,
        --   другие пиры могут работать быстрее и от них
        --   может прийти accept.
        --   так что раунд всё равно нужно завести,
        --   даже если транза не очень.

        unless validated do
          maybe1 rndHere none $ \rnd -> do
            atomically $ writeTVar (view refChanRoundClosed rnd) True
            liftIO $ delBlock sto hash

        guard' "validated" validated

        debug $ "TRANS VALIDATED" <+> pretty  (AsBase58 chan) <+> pretty hash

        gossip msg

        --   проверить, что мы вообще авторизованы
        --   рассылать ACCEPT

        guard' "pk in pips" ( pk `HashMap.member` pips )

        -- если нет - то и всё, просто перешлём
        -- по госсипу исходную транзу

        ts <- liftIO getPOSIXTime <&> round <&> Just
        let tran = AcceptTran ts headRef (HashRef hash)

        -- --  генерируем Accept
        let accept = Accept chan (makeSignedBox @s pk sk tran)

        -- -- и рассылаем всем
        debug "GOSSIP ACCEPT TRANSACTION"
        gossip accept

        -- --  рассылаем ли себе? что бы был хоть один accept
        refChanUpdateProto True pc adapter accept

     Accept chan box -> deferred @proto do
     -- Accept chan box -> do

       -- что если получили ACCEPT раньше PROPOSE ?
       -- что если PROPOSE еще обрабатывается?
       -- надо, короче, блокироваться и ждать тут Propose
       -- но если блокироваться --- то конвейр вообще
       -- может встать. что делать?
       --

       debug $ "RefChanUpdate/ACCEPT" <+> pretty h0

       (peerKey, headRef, hashRef) <- justOrThrowIO "accept unboxSignedBox0 box" do
          (peerKey, AcceptTran _ headRef hashRef) <- unboxSignedBox0 box
          Just (peerKey, headRef, hashRef)

       let refchanKey = RefChanHeadKey @s chan

       headBlock <- getActualRefChanHead @e refchanKey
          & justMOrThrowIO "getActualRefChanHead"

       h <- liftIO (getRef sto refchanKey)
          & justMOrThrowIO "getRef"

       guard' "HashRef h == headRef" (HashRef h == headRef)

       gossip msg

       -- тут может так случиться, что propose еще нет
       -- UDP вообще не гарантирует порядок доставки, а отправляем мы транзы
       -- почти одновременно. ну или не успело записаться. и что делать?

       -- вот прямо тут надо ждать, пока придёт / завершится Propose
       -- -- или до таймаута

       let afterPropose = do

             here <- fix \next -> do
                      blk <- liftIO (hasBlock sto (fromHashRef hashRef)) <&> isJust
                      if blk then
                        pure blk
                      else do
                        pause @'Seconds 0.25
                        next

             unless here do
              warn $ "No propose transaction saved yet!" <+> pretty hashRef

             tranBs <- liftIO (getBlock sto (fromHashRef hashRef))
                  & justMOrThrowIO "after propose getBlock"

             tran <- deserialiseOrFail @(RefChanUpdate e) tranBs & either (const Nothing) Just
                & justOrThrowIO "after propose deserialiseOrFail RefChanUpdate"


             proposed <- justOrThrowIO "after propose case tran" $
                case tran of
                            Propose _ pbox -> Just pbox
                            _              -> Nothing


             (_, ptran) <- unboxSignedBox0 @(ProposeTran e) @s proposed
                & justOrThrowIO "after propose unboxSignedBox0 proposed"

             debug $ "ACCEPT FROM:" <+> pretty (AsBase58 peerKey) <+> pretty h0

             -- compiler bug?
             let (ProposeTran _ pbox) = ptran

             (authorKey, _) <- unboxSignedBox0 pbox
                & justOrThrowIO "after propose unboxSignedBox0 pbox"

             -- может, и не надо второй раз проверять
             guard' "after propose checkACL" $
                checkACL ACLUpdate headBlock (Just peerKey) authorKey

             debug $ "JUST GOT TRANSACTION FROM STORAGE! ABOUT TO CHECK IT" <+> pretty hashRef

             rcRound <- find (RefChanRoundKey @e hashRef) id
                & justMOrThrowIO "after propose find RefChanRoundKey"

             atomically $ modifyTVar (view refChanRoundAccepts rcRound) (HashMap.insert peerKey ())

             -- TODO: garbage-collection-strongly-required
             ha <- liftIO (putBlock sto (serialise msg))
                & justMOrThrowIO "after propose putBlock"

             atomically $ modifyTVar (view refChanRoundTrans rcRound) (HashSet.insert (HashRef ha))
             -- atomically $ modifyTVar (view refChanRoundTrans rcRound) (HashSet.insert hashRef) -- propose just in case we missed it?

             accepts <- atomically $ readTVar (view refChanRoundAccepts rcRound) <&> HashMap.size

             -- FIXME: why-accepts-quorum-on-failed-proposal?

             debug $ "ACCEPTS:" <+> pretty accepts

             closed <- readTVarIO (view refChanRoundClosed rcRound)

             -- FIXME: round!
             when (fromIntegral accepts >= view refChanHeadQuorum headBlock && not closed) do
               debug $ "ROUND!" <+> pretty accepts <+> pretty hashRef

               trans <- atomically $ readTVar (view refChanRoundTrans rcRound) <&> HashSet.toList

               forM_ trans $ \t -> do
                refChanWriteTran adapter t
                debug $ "WRITING TRANS" <+> pretty t

               let pips  = view refChanHeadPeers headBlock & HashMap.keys & HashSet.fromList
               votes <- readTVarIO (view refChanRoundAccepts rcRound) <&> HashSet.fromList . HashMap.keys

               debug $ "PIPS" <+> pretty (HashSet.toList pips & fmap AsBase58)
               debug $ "VOTES" <+> pretty (HashSet.toList votes & fmap AsBase58)

               when (pips `HashSet.isSubsetOf` votes) do
                debug $ "CLOSING ROUND" <+> pretty hashRef <+> pretty (length trans)
                atomically $ writeTVar (view refChanRoundClosed rcRound) True

       -- мы не можем ждать / поллить в deferred потому,
       -- что мы так забьем конвейр - там сейчас всего 8
       -- воркеров, и 8 параллельных  ждущих запросов
       -- все остановят.

       let w = TimeoutSec (realToFrac $ view refChanHeadWaitAccept headBlock)
       void $ race ( pause (2 * w) ) afterPropose
  where
    guard' :: Text -> Bool -> m ()
    guard' msg p = unless p $ throwIO (RefchanUpdateProtoFailure msg)

    justOrThrowIO :: Text -> Maybe a -> m a
    justOrThrowIO msg = maybe (throwIO (RefchanUpdateProtoFailure msg)) pure

    justMOrThrowIO :: Text -> m (Maybe a) -> m a
    justMOrThrowIO msg = (justOrThrowIO msg =<<)

orThrowIO :: Monad m => m a -> m (Maybe a) -> m a
orThrowIO md = (maybe md pure =<<)

data RefchanUpdateProtoFailure = RefchanUpdateProtoFailure Text deriving (Show)
instance Exception RefchanUpdateProtoFailure

-- TODO: refchan-poll-proto
--   Запрашиваем refchan у всех.
--   Пишем в итоговый лог только такие
--   propose + accept у которых больше quorum accept
--   каждую транзу обрабатываем только один раз
--

refChanRequestProto :: forall e s m proto . ( MonadIO m
                                            , Request e proto m
                                            , Response e proto m
                                            , HasDeferred proto e m
                                            , IsPeerAddr e m
                                            , Pretty (Peer e)
                                            , Sessions e (KnownPeer e) m
                                            , Sessions e (RefChanHeadBlock e) m
                                            , EventEmitter e proto m
                                            , HasStorage m
                                            , Signatures s
                                            , IsRefPubKey s
                                            , ForRefChans e
                                            , s ~ Encryption e
                                            , proto ~  RefChanRequest e
                                            )
                   => Bool
                   -> RefChanAdapter e m
                   -> RefChanRequest e
                   -> m ()

refChanRequestProto self adapter msg = do

  peer <- thatPeer @proto

  auth' <- find (KnownPeerKey peer) id

  sto <- getStorage

  void $ runMaybeT do

    guard (self || isJust auth')

    auth <- MaybeT $ pure  auth'

    guard =<< lift (refChanSubscribed adapter (getRefChanId @e msg))

    case msg of

      RefChanRequest chan -> do
        rv <- MaybeT $ liftIO $ getRef sto (RefChanLogKey @s chan)
        lift $ response @e (RefChanResponse @e chan (HashRef rv))

      RefChanResponse chan val -> do
        hd <- MaybeT $ getActualRefChanHead @e (RefChanHeadKey @s chan)
        let ppk = view peerSignKey auth

        guard $ ppk `HashMap.member` view refChanHeadPeers hd

        lift $ emit RefChanRequestEventKey (RefChanRequestEvent @e chan val)
        debug $ "RefChanResponse" <+> pretty peer <+> pretty (AsBase58 chan) <+> pretty val

          -- case s of
          --   Accept{} -> pure ()
          --   Propose _ box -> do
          --     (_, ProposeTran _ pbox :: ProposeTran L4Proto) <- toMPlus $ unboxSignedBox0 box
          --     (_, bs2) <- toMPlus $ unboxSignedBox0 pbox
          --     liftIO $ BS.putStr bs2

readProposeTranMay :: forall p e s m . ( Monad m
                                       , ForRefChans e
                                       , Signatures (Encryption e)
                                       , s ~ Encryption e
                                       , Serialise p
                                       )
                => LBS.ByteString
                -> m (Maybe p)
readProposeTranMay lbs = runMaybeT do

  updTx <- deserialiseOrFail @(RefChanUpdate e) lbs & toMPlus

  box <- case updTx of
           Accept{} -> mzero
           Propose _ box -> pure box

  (_, ProposeTran _ pbox :: ProposeTran e) <- toMPlus $ unboxSignedBox0 @_ @s box
  (_, bs2) <- toMPlus $ unboxSignedBox0 pbox

  deserialiseOrFail @p (LBS.fromStrict bs2) & toMPlus

makeProposeTran :: forall e s m . ( MonadIO m
                                  , ForRefChans e
                                  , Signatures (Encryption e)
                                  , HasStorage m
                                  , s ~ Encryption e
                                  )
                => PeerCredentials s
                -> RefChanId e
                -> SignedBox ByteString s
                -> m (Maybe (SignedBox (ProposeTran e) s))

makeProposeTran creds chan box1 = do
  sto <- getStorage
  runMaybeT do
    h <- MaybeT $ liftIO $ getRef sto (RefChanHeadKey @s chan)
    let tran = ProposeTran @e (HashRef h) box1
    let pk = view peerSignPk creds
    let sk = view peerSignSk creds
    pure $ makeSignedBox @s pk sk tran

-- FIXME: reconnect-validator-client-after-restart
--  почему-то сейчас если рестартовать пира,
--  но не растартовать валидатор --- то не получится
--  повторно соединиться с валидатором.

