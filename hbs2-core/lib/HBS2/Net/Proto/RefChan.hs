{-# Language UndecidableInstances #-}
{-# Language AllowAmbiguousTypes #-}
{-# Language TemplateHaskell #-}
{-# Language FunctionalDependencies #-}
{-# LANGUAGE ImplicitParams #-}
module HBS2.Net.Proto.RefChan where

import HBS2.Prelude.Plated
import HBS2.Hash
import HBS2.Data.Detect
import HBS2.Clock
import HBS2.Net.Proto
import HBS2.Net.Auth.Credentials
import HBS2.Base58
import HBS2.Defaults
import HBS2.Events
import HBS2.Net.Proto.Peer
import HBS2.Net.Proto.BlockAnnounce
import HBS2.Net.Proto.Sessions
import HBS2.Data.Types.Refs
import HBS2.Data.Types.SignedBox
import HBS2.Actors.Peer.Types
import HBS2.Data.Types.Peer
import HBS2.Storage

import Data.Config.Suckless

import HBS2.System.Logger.Simple

import Codec.Serialise
import Control.Monad.Identity
import Control.Monad.Trans.Maybe
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Either
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.Maybe
import Data.Text qualified as Text
import Lens.Micro.Platform
import Data.Hashable hiding (Hashed)
import Type.Reflection (someTypeRep)

import UnliftIO

{- HLINT ignore "Use newtype instead of data" -}

type RefChanId e = PubKey 'Sign (Encryption e)
type RefChanOwner e = PubKey 'Sign (Encryption e)
type RefChanAuthor e = PubKey 'Sign (Encryption e)

type Weight = Integer

data RefChanHeadBlock e =
  RefChanHeadBlockSmall
  { _refChanHeadVersion     :: Integer
  , _refChanHeadQuorum      :: Integer
  , _refChanHeadWaitAccept  :: Integer
  , _refChanHeadPeers       :: HashMap (PubKey 'Sign (Encryption e)) Weight
  , _refChanHeadAuthors     :: HashSet (PubKey 'Sign (Encryption e))
  }
  deriving stock (Generic)

makeLenses 'RefChanHeadBlockSmall

type ForRefChans e = ( Serialise ( PubKey 'Sign (Encryption e))
                     , Pretty (AsBase58 (PubKey 'Sign (Encryption e)))
                     , FromStringMaybe (PubKey 'Sign (Encryption e))
                     , Signatures (Encryption e)
                     , Serialise (Signature (Encryption e))
                     , Hashable (PubKey 'Sign (Encryption e))
                     )

instance ForRefChans e => Serialise (RefChanHeadBlock e)

type instance SessionData e (RefChanHeadBlock e) = RefChanHeadBlock e

newtype instance SessionKey e  (RefChanHeadBlock e) =
  RefChanHeadBlockKey (RefChanHeadKey (Encryption e))

deriving newtype instance ForRefChans L4Proto
  => Hashable (SessionKey L4Proto (RefChanHeadBlock L4Proto))

deriving stock instance ForRefChans L4Proto
  => Eq (SessionKey L4Proto (RefChanHeadBlock L4Proto))

-- TODO: define-expiration-time
instance Expires (SessionKey L4Proto (RefChanHeadBlock L4Proto)) where
  expiresIn = const (Just defCookieTimeoutSec)

newtype RefChanHeadKey s = RefChanHeadKey (PubKey 'Sign s)

deriving stock instance IsRefPubKey s => Eq (RefChanHeadKey s)

instance IsRefPubKey s => Hashable (RefChanHeadKey s) where
  hashWithSalt s k = hashWithSalt s (hashObject @HbSync k)

instance IsRefPubKey s => Hashed HbSync (RefChanHeadKey s) where
  hashObject (RefChanHeadKey pk) = hashObject ("refchanhead|" <> serialise pk)

instance IsRefPubKey s => FromStringMaybe (RefChanHeadKey s) where
  fromStringMay s = RefChanHeadKey <$> fromStringMay s

instance IsRefPubKey s =>  IsString (RefChanHeadKey s) where
  fromString s = fromMaybe (error "bad public key base58") (fromStringMay s)

instance Pretty (AsBase58 (PubKey 'Sign s )) => Pretty (AsBase58 (RefChanHeadKey s)) where
  pretty (AsBase58 (RefChanHeadKey k)) = pretty (AsBase58 k)

instance Pretty (AsBase58 (PubKey 'Sign s )) => Pretty (RefChanHeadKey s) where
  pretty (RefChanHeadKey k) = pretty (AsBase58 k)


newtype RefChanLogKey s = RefChanLogKey (PubKey 'Sign s)

deriving stock instance IsRefPubKey s => Eq (RefChanLogKey s)

instance IsRefPubKey s => Hashable (RefChanLogKey s) where
  hashWithSalt s k = hashWithSalt s (hashObject @HbSync k)

instance IsRefPubKey s => Hashed HbSync (RefChanLogKey s) where
  hashObject (RefChanLogKey pk) = hashObject ("refchanlog|" <> serialise pk)

instance IsRefPubKey s => FromStringMaybe (RefChanLogKey s) where
  fromStringMay s = RefChanLogKey <$> fromStringMay s

instance IsRefPubKey s =>  IsString (RefChanLogKey s) where
  fromString s = fromMaybe (error "bad public key base58") (fromStringMay s)

instance Pretty (AsBase58 (PubKey 'Sign s )) => Pretty (AsBase58 (RefChanLogKey s)) where
  pretty (AsBase58 (RefChanLogKey k)) = pretty (AsBase58 k)

instance Pretty (AsBase58 (PubKey 'Sign s )) => Pretty (RefChanLogKey s) where
  pretty (RefChanLogKey k) = pretty (AsBase58 k)


-- блок головы может быть довольно большой.
-- поэтому посылаем его, как merkle tree
newtype RefChanHeadBlockTran e =
  RefChanHeadBlockTran { fromRefChanHeadBlockTran :: HashRef }
  deriving stock (Generic)

instance Serialise (RefChanHeadBlockTran e)

data RefChanHead e =
    RefChanHead (RefChanId e) (RefChanHeadBlockTran e)
  | RefChanGetHead (RefChanId e)
  deriving stock (Generic)

instance ForRefChans e => Serialise (RefChanHead e)


data ProposeTran e = ProposeTran HashRef (SignedBox ByteString e) -- произвольная бинарная транзакция,
                     deriving stock (Generic)                     -- подписанная ключом **АВТОРА**, который её рассылает


data AcceptTran e = AcceptTran HashRef HashRef -- ссылка на (ProposTran e)
                    deriving stock (Generic)

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
    Propose (RefChanId e) (SignedBox (ProposeTran e) e) -- подписано ключом пира
  | Accept  (RefChanId e) (SignedBox (AcceptTran e) e)  -- подписано ключом пира
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

-- принимается, только если соответствует текущему HEAD
-- не пишется в журнал
data RefChanNotify e =
  Notify (RefChanId e) (SignedBox ByteString e) -- подписано ключом автора
  deriving stock (Generic)

instance ForRefChans e => Serialise (RefChanNotify e)

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

instance (ForRefChans e, Pretty (AsBase58 (Nonce (RefChanValidate e)))) => Pretty (RefChanValidate e) where
  pretty (RefChanValidate n c d) = case d of
    Validate r -> pretty "validate" <+> pretty (AsBase58 n) <+> pretty (AsBase58 c) <+> pretty r
    Accepted r -> pretty "accepted" <+> pretty (AsBase58 n) <+> pretty (AsBase58 c) <+> pretty r
    Rejected r -> pretty "rejected" <+> pretty (AsBase58 n) <+> pretty (AsBase58 c) <+> pretty r
    Poke       -> pretty "poke"     <+> pretty (AsBase58 n) <+> pretty (AsBase58 c)

-- FIXME: rename
data RefChanAdapter e m =
  RefChanAdapter
  { refChanOnHead     :: RefChanId e -> RefChanHeadBlockTran e -> m ()
  , refChanSubscribed :: RefChanId e -> m Bool
  , refChanWriteTran  :: HashRef -> m ()
  , refChanValidatePropose :: RefChanId e -> HashRef -> m Bool
  }

class HasRefChanId e p | p -> e where
  getRefChanId :: p -> RefChanId e

instance HasRefChanId e (RefChanUpdate e) where
  getRefChanId = \case
    Propose c _ -> c
    Accept  c _ -> c

instance HasRefChanId e (RefChanRequest e) where
  getRefChanId = \case
    RefChanRequest c -> c
    RefChanResponse c _ -> c

instance HasRefChanId e (RefChanNotify e) where
  getRefChanId = \case
    Notify c _ -> c

instance HasRefChanId e (RefChanValidate e) where
  getRefChanId = rcvChan

refChanHeadProto :: forall e s m . ( MonadIO m
                                   , Request e (RefChanHead e) m
                                   , Request e (BlockAnnounce e) m
                                   , Response e (RefChanHead e) m
                                   , HasPeerNonce e m
                                   , HasDeferred e (RefChanHead e) m
                                   , IsPeerAddr e m
                                   , Pretty (Peer e)
                                   , Sessions e (KnownPeer e) m
                                   , HasStorage m
                                   , Signatures s
                                   , IsRefPubKey s
                                   , Pretty (AsBase58 (PubKey 'Sign s))
                                   , s ~ Encryption e
                                   )
                  => Bool
                  -> RefChanAdapter e m
                  -> RefChanHead e
                  -> m ()

refChanHeadProto self adapter msg = do
  -- авторизовать пира
  peer <- thatPeer proto

  auth <- find (KnownPeerKey peer) id <&> isJust

  no <- peerNonce @e

  void $ runMaybeT do

    guard (auth || self)

    case msg of
      RefChanHead chan pkt -> do
        guard =<< lift (refChanSubscribed adapter chan)
        trace $ "RefChanHead" <+> pretty self <+> pretty (AsBase58 chan)
        -- TODO: notify-others-for-new-head
        --   нужно ли уведомить остальных, что голова поменялась?
        --   всех, от кого мы еще не получали данное сообщение
        --   откуда мы знаем, от кого мы получали данное сообщение?
        lift $ refChanOnHead adapter chan pkt

      RefChanGetHead chan -> deferred proto do
        trace $ "RefChanGetHead" <+> pretty self <+> pretty (AsBase58 chan)

        sto <- getStorage
        ref <- MaybeT $ liftIO $ getRef sto (RefChanHeadKey @s chan)
        sz  <- MaybeT $ liftIO $ hasBlock sto ref

        let annInfo = BlockAnnounceInfo 0 NoBlockInfoMeta sz ref
        let announce = BlockAnnounce @e no annInfo
        lift $ request peer announce
        lift $ request peer (RefChanHead @e chan (RefChanHeadBlockTran (HashRef ref)))

  where
    proto = Proxy @(RefChanHead e)


refChanUpdateProto :: forall e s m . ( MonadIO m
                                     , MonadUnliftIO m
                                     , Request e (RefChanUpdate e) m
                                     , Response e (RefChanUpdate e) m
                                     , HasDeferred e (RefChanUpdate e) m
                                     , IsPeerAddr e m
                                     , Pretty (Peer e)
                                     , Sessions e (KnownPeer e) m
                                     , Sessions e (RefChanHeadBlock e) m
                                     , Sessions e (RefChanRound e) m
                                     , EventEmitter e (RefChanRound e) m
                                     , HasStorage m
                                     , HasGossip e (RefChanUpdate e) m
                                     , Signatures s
                                     , IsRefPubKey s
                                     , Pretty (AsBase58 (PubKey 'Sign s))
                                     -- , Serialise (Signature s)
                                     , ForRefChans e
                                     , s ~ Encryption e
                                     )
                   => Bool
                   -> PeerCredentials s
                   -> RefChanAdapter e m
                   -> RefChanUpdate e
                   -> m ()

refChanUpdateProto self pc adapter msg = do
  -- авторизовать пира
  peer <- thatPeer proto

  auth <- find (KnownPeerKey peer) id <&> isJust

  sto <- getStorage

  let pk = view peerSignPk pc
  let sk = view peerSignSk pc

  void $ runMaybeT do

    guard (auth || self)

    -- TODO: process-each-message-only-once
    -- где-то тут мы разбираемся, что такое сообщеине
    -- уже отправляли и больше одного раза не реагируем

    -- У нас тут получается раунд на каждый Propose
    -- Это может быть и хорошо и похо. Если очень
    -- много транзакций, это плохо. Если не очень
    -- то это нормально и можно обойтись без понятия
    -- "блок".
    -- так-то и количество proposers можно ограничить

    guard =<< lift (refChanSubscribed adapter (getRefChanId msg))

    let h0 = hashObject @HbSync (serialise msg)
    guard =<< liftIO (hasBlock sto h0 <&> isNothing)

    case msg of
     Propose chan box -> do

       debug $ "RefChanUpdate/Propose" <+> pretty h0

       deferred proto do

        -- проверили подпись пира
        (peerKey, ProposeTran headRef abox) <- MaybeT $ pure $ unboxSignedBox0 box

        -- проверили подпись автора
        (authorKey, _) <- MaybeT $ pure $ unboxSignedBox0 abox

        -- итак, сначала достаём голову. как мы достаём голову?

        let refchanKey = RefChanHeadKey @s chan
        h <- MaybeT $ liftIO $ getRef sto refchanKey
        -- смотрим, что у нас такая же голова.
        -- если нет -- значит, кто-то рассинхронизировался.
        -- может быть, потом попробуем головы запросить
        guard (HashRef h == headRef)

        debug $ "OMG! Got trans" <+> pretty (AsBase58 peerKey)  <+> pretty (AsBase58 authorKey)

        -- теперь достаём голову
        headBlock <- MaybeT $ getActualRefChanHead @e refchanKey

        let pips = view refChanHeadPeers headBlock

        guard $ checkACL headBlock (Just peerKey) authorKey

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

        hash <- MaybeT $ liftIO $ putBlock sto (serialise msg)

        ts <- liftIO getTimeCoarse

        let toWait = TimeoutSec (fromIntegral $ 2 * view refChanHeadWaitAccept headBlock)
        let ttl = ts + fromNanoSecs (fromIntegral $ toNanoSeconds toWait)

        let rcrk = RefChanRoundKey (HashRef hash)

        rndHere <- lift $ find rcrk id

        defRound <- RefChanRound @e (HashRef hash) refchanKey ttl
                       <$> newTVarIO False
                       <*> newTVarIO Nothing
                       <*> newTVarIO (HashSet.singleton (HashRef hash)) -- save propose
                       <*> newTVarIO (HashMap.singleton peerKey ())

        unless (isJust rndHere) do
          lift $ update defRound rcrk id
          lift $ emit @e RefChanRoundEventKey (RefChanRoundEvent rcrk)

        -- не обрабатывать propose, если он уже в процессе
        guard (isNothing rndHere)

        -- FIXME: fixed-timeout-is-no-good
        validated <- either id id <$> lift ( race (pause @'Seconds 5 >> pure False)
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

        guard validated

        debug $ "TRANS VALIDATED" <+> pretty  (AsBase58 chan) <+> pretty hash

        lift $ gossip msg

        --   проверить, что мы вообще авторизованы
        --   рассылать ACCEPT

        guard ( pk `HashMap.member` pips )

        -- если нет - то и всё, просто перешлём
        -- по госсипу исходную транзу

        let tran = AcceptTran headRef (HashRef hash)

        -- --  генерируем Accept
        let accept = Accept chan (makeSignedBox @e pk sk tran)

        -- -- и рассылаем всем
        debug "GOSSIP ACCEPT TRANSACTION"
        lift $ gossip accept

        -- --  рассылаем ли себе? что бы был хоть один accept
        lift $ refChanUpdateProto True pc adapter accept

     Accept chan box -> deferred proto do

       -- что если получили ACCEPT раньше PROPOSE ?
       -- что если PROPOSE еще обрабатывается?
       -- надо, короче, блокироваться и ждать тут Propose
       -- но если блокироваться --- то конвейр вообще
       -- может встать. что делать?
       --

       debug $ "RefChanUpdate/ACCEPT" <+> pretty h0

       (peerKey, AcceptTran headRef hashRef) <- MaybeT $ pure $ unboxSignedBox0 box

       let refchanKey = RefChanHeadKey @s chan

       headBlock <- MaybeT $ getActualRefChanHead @e refchanKey

       h <- MaybeT $ liftIO $ getRef sto refchanKey

       guard (HashRef h == headRef)

       lift $ gossip msg

       -- тут может так случиться, что propose еще нет
       -- UDP вообще не гарантирует порядок доставки, а отправляем мы транзы
       -- почти одновременно. ну или не успело записаться. и что делать?

       -- вот прямо тут надо ждать, пока придёт / завершится Propose
       -- -- или до таймаута

       let afterPropose = runMaybeT do

             here <- fix \next -> do
                      blk <- liftIO (hasBlock sto (fromHashRef hashRef)) <&> isJust
                      if blk then
                        pure blk
                      else do
                        pause @'Seconds 0.25
                        next

             unless here do
              warn $ "No propose transaction saved yet!" <+> pretty hashRef

             tranBs <- MaybeT $ liftIO $ getBlock sto (fromHashRef hashRef)

             tran <- MaybeT $ pure $ deserialiseOrFail @(RefChanUpdate e) tranBs & either (const Nothing) Just


             proposed <- MaybeT $ pure $ case tran of
                            Propose _ pbox -> Just pbox
                            _              -> Nothing


             (_, ptran) <- MaybeT $ pure $ unboxSignedBox0 @(ProposeTran e) @e proposed

             debug $ "ACCEPT FROM:" <+> pretty (AsBase58 peerKey) <+> pretty h0

             -- compiler bug?
             let (ProposeTran _ pbox) = ptran

             (authorKey, _) <- MaybeT $ pure $ unboxSignedBox0 pbox

             -- может, и не надо второй раз проверять
             guard $ checkACL headBlock (Just peerKey) authorKey

             debug $ "JUST GOT TRANSACTION FROM STORAGE! ABOUT TO CHECK IT" <+> pretty hashRef

             rcRound <- MaybeT $ find (RefChanRoundKey @e hashRef) id

             atomically $ modifyTVar (view refChanRoundAccepts rcRound) (HashMap.insert peerKey ())

             -- TODO: garbage-collection-strongly-required
             ha <- MaybeT $ liftIO $ putBlock sto (serialise msg)

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
                lift $ refChanWriteTran adapter t
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
       void $ lift $ race ( pause (2 * w) ) afterPropose

  where
    proto = Proxy @(RefChanUpdate e)


checkACL :: forall e s . (Encryption e ~ s, ForRefChans e)
         => RefChanHeadBlock e
         -> Maybe (PubKey 'Sign s)
         -> PubKey 'Sign s
         -> Bool

checkACL theHead mbPeerKey authorKey = match
  where
    pips = view refChanHeadPeers theHead
    aus  = view refChanHeadAuthors theHead
    match = maybe True (`HashMap.member` pips) mbPeerKey
           && authorKey `HashSet.member` aus

-- TODO: refchan-poll-proto
--   Запрашиваем refchan у всех.
--   Пишем в итоговый лог только такие
--   propose + accept у которых больше quorum accept
--   каждую транзу обрабатываем только один раз
--

refChanRequestProto :: forall e s m . ( MonadIO m
                                     , Request e (RefChanRequest e) m
                                     , Response e (RefChanRequest e) m
                                     , HasDeferred e (RefChanRequest e) m
                                     , IsPeerAddr e m
                                     , Pretty (Peer e)
                                     , Sessions e (KnownPeer e) m
                                     , Sessions e (RefChanHeadBlock e) m
                                     , EventEmitter e (RefChanRequest e) m
                                     , HasStorage m
                                     , Signatures s
                                     , IsRefPubKey s
                                     , Pretty (AsBase58 (PubKey 'Sign s))
                                     -- , Serialise (Signature s)
                                     , ForRefChans e
                                     , s ~ Encryption e
                                     )
                   => Bool
                   -> RefChanAdapter e m
                   -> RefChanRequest e
                   -> m ()

refChanRequestProto self adapter msg = do

  peer <- thatPeer proto

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

  where
    proto = Proxy @(RefChanRequest e)


refChanNotifyProto :: forall e s m . ( MonadIO m
                                     , Request e (RefChanNotify e) m
                                     , Response e (RefChanNotify e) m
                                     , HasRefChanId e (RefChanNotify e)
                                     , HasDeferred e (RefChanNotify e) m
                                     , HasGossip e (RefChanNotify e) m
                                     , IsPeerAddr e m
                                     , Pretty (Peer e)
                                     , Sessions e (RefChanHeadBlock e) m
                                     , Sessions e (KnownPeer e) m
                                     , HasStorage m
                                     , Signatures s
                                     , IsRefPubKey s
                                     , ForRefChans e
                                     , Pretty (AsBase58 (PubKey 'Sign s))
                                     , s ~ Encryption e
                                     )
                  => Bool
                  -> RefChanAdapter e m
                  -> RefChanNotify e
                  -> m ()

refChanNotifyProto self adapter msg@(Notify rchan box) = do
  -- аутентифицируем
  -- проверяем ACL
  -- пересылаем всем

  peer <- thatPeer proto

  auth <- find (KnownPeerKey peer) id <&> isJust

  void $ runMaybeT do

    guard =<< lift (refChanSubscribed adapter rchan)

    guard (self || auth)

    (authorKey, bs) <- MaybeT $ pure $ unboxSignedBox0 box

    let refchanKey = RefChanHeadKey @s rchan
    headBlock <- MaybeT $ getActualRefChanHead @e refchanKey

    guard $ checkACL headBlock Nothing authorKey

    -- теперь пересылаем по госсипу
    lift $ gossip msg

    trace $ "refChanNotifyProto" <+> pretty (BS.length bs)

    -- тут надо заслать во внешнее приложение,
    -- равно как и в остальных refchan-протоколах

  where
    proto = Proxy @(RefChanNotify e)


getActualRefChanHead :: forall e s m . ( MonadIO m
                                       , Sessions e (RefChanHeadBlock e) m
                                       , HasStorage m
                                       , Signatures s
                                       , IsRefPubKey s
                                       , Pretty (AsBase58 (PubKey 'Sign s))
                                     -- , Serialise (Signature s)
                                       , ForRefChans e
                                       , HasStorage m
                                       , s ~ Encryption e
                                       )
                   => RefChanHeadKey s
                   -> m (Maybe (RefChanHeadBlock e))

getActualRefChanHead key = do
  sto <- getStorage

  runMaybeT do
    mbHead <- do
      lift $ find @e (RefChanHeadBlockKey key) id

    case mbHead of
      Just hd -> do
        debug "HEAD DISCOVERED"
        pure hd

      Nothing -> do
        debug "ABOUT TO FIND HEAD"
        h <- MaybeT $ liftIO $ getRef sto key
        hdblob <- MaybeT $ readBlobFromTree ( getBlock sto ) (HashRef h)
        (_, headblk) <- MaybeT $ pure $ unboxSignedBox @(RefChanHeadBlock e) @e hdblob
        lift $ update headblk (RefChanHeadBlockKey key) id  -- set found head
        debug "HEAD FOUND"
        pure headblk

makeProposeTran :: forall e s m . ( MonadIO m
                                  , ForRefChans e
                                  , Signatures (Encryption e)
                                  , HasStorage m
                                  , s ~ Encryption e
                                  )
                => PeerCredentials s
                -> RefChanId e
                -> SignedBox ByteString e
                -> m (Maybe (SignedBox (ProposeTran e) e))

makeProposeTran creds chan box1 = do
  sto <- getStorage
  runMaybeT do
    h <- MaybeT $ liftIO $ getRef sto (RefChanHeadKey @s chan)
    let tran = ProposeTran @e (HashRef h) box1
    let pk = view peerSignPk creds
    let sk = view peerSignSk creds
    pure $ makeSignedBox @e pk sk tran


instance ForRefChans e => FromStringMaybe (RefChanHeadBlock e) where
  fromStringMay str = RefChanHeadBlockSmall <$> version
                                          <*> quorum
                                          <*> wait
                                          <*> pure (HashMap.fromList peers)
                                          <*> pure (HashSet.fromList authors)
    where
      parsed = parseTop str & fromRight mempty
      version = lastMay [ n | (ListVal [SymbolVal "version", LitIntVal n] ) <- parsed ]
      quorum  = lastMay [ n | (ListVal [SymbolVal "quorum", LitIntVal n] ) <- parsed ]
      wait    = lastMay [ n | (ListVal [SymbolVal "wait", LitIntVal n] ) <- parsed ]

      peers   = catMaybes [ (,) <$> fromStringMay (Text.unpack s) <*> pure w
                          | (ListVal [SymbolVal "peer", LitStrVal s, LitIntVal w] ) <- parsed
                          ]

      authors = catMaybes [ fromStringMay (Text.unpack s)
                          | (ListVal [SymbolVal "author", LitStrVal s] ) <- parsed
                          ]

instance ForRefChans e => Pretty (RefChanHeadBlock e) where
  pretty blk = parens ("version" <+> pretty (view refChanHeadVersion blk)) <> line
               <>
               parens ("quorum" <+> pretty (view refChanHeadQuorum blk)) <> line
               <>
               parens ("wait" <+> pretty (view refChanHeadWaitAccept blk)) <> line
               <>
               vcat (fmap peer (HashMap.toList $ view refChanHeadPeers blk)) <> line
               <>
               vcat (fmap author (HashSet.toList $ view refChanHeadAuthors blk)) <> line

    where
      peer (p,w) = parens ("peer" <+> dquotes (pretty (AsBase58 p)) <+> pretty w)
      author p   = parens ("author" <+> dquotes (pretty (AsBase58 p)))



-- FIXME: reconnect-validator-client-after-restart
--  почему-то сейчас если рестартовать пира,
--  но не растартовать валидатор --- то не получится
--  повторно соединиться с валидатором.

