{-# Language UndecidableInstances #-}
{-# Language AllowAmbiguousTypes #-}
{-# Language TemplateHaskell #-}
module HBS2.Net.Proto.RefChan where

import HBS2.Prelude.Plated
import HBS2.Hash
-- import HBS2.Clock
import HBS2.Net.Proto
import HBS2.Net.Auth.Credentials
import HBS2.Base58
-- import HBS2.Events
import HBS2.Net.Proto.Peer
import HBS2.Net.Proto.BlockAnnounce
import HBS2.Net.Proto.Sessions
import HBS2.Data.Types.Refs
import HBS2.Actors.Peer.Types
import HBS2.Storage

import Data.Config.Suckless

import HBS2.System.Logger.Simple

import Codec.Serialise
import Control.Monad.Identity
import Control.Monad.Trans.Maybe
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Either
import Data.Maybe
import Data.Text qualified as Text
import Lens.Micro.Platform

import Data.Hashable hiding (Hashed)

{- HLINT ignore "Use newtype instead of data" -}

type RefChanId e = PubKey 'Sign (Encryption e)
type RefChanOwner e = PubKey 'Sign (Encryption e)
type RefChanAuthor e = PubKey 'Sign (Encryption e)

data SignedBox p e =
  SignedBox (PubKey 'Sign (Encryption e)) ByteString (Signature (Encryption e))
  deriving stock (Generic)

type Weight = Integer

data RefChanHeadBlock e =
  RefChanHeadBlockSmall
  { _refChanHeadVersion     :: Integer
  , _refChanHeadQuorum      :: Integer
  , _refChanHeadWaitAccept  :: Integer
  , _refChanHeadPeers       :: [(PubKey 'Sign (Encryption e),Weight)]
  , _refChanHeadAuthors     :: [PubKey 'Sign (Encryption e)]
  }
  deriving stock (Generic)

makeLenses 'RefChanHeadBlockSmall

type ForRefChans e = ( Serialise ( PubKey 'Sign (Encryption e))
                     , Pretty (AsBase58 (PubKey 'Sign (Encryption e)))
                     , FromStringMaybe (PubKey 'Sign (Encryption e))
                     , Serialise (Signature (Encryption e))
                     )

instance ForRefChans e => Serialise (RefChanHeadBlock e)
instance ForRefChans e => Serialise (SignedBox p e)



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

data RefChanHeadAdapter e m =
  RefChanHeadAdapter
  { refChanHeadOnHead     :: RefChanId e -> RefChanHeadBlockTran e -> m ()
  , refChanHeadSubscribed :: RefChanId e -> m Bool
  }

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
                  -> RefChanHeadAdapter e m
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
        guard =<< lift (refChanHeadSubscribed adapter chan)
        trace $ "RefChanHead" <+> pretty self <+> pretty (AsBase58 chan)
        -- FIXME: check-chan-is-listened
        lift $ refChanHeadOnHead adapter chan pkt

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

makeSignedBox :: forall e p . (Serialise p, ForRefChans e, Signatures (Encryption e))
              => PubKey 'Sign (Encryption e)
              -> PrivKey 'Sign (Encryption e)
              -> p
              -> SignedBox p e

makeSignedBox pk sk msg = SignedBox @p @e pk bs sign
  where
    bs = LBS.toStrict (serialise msg)
    sign = makeSign @(Encryption e) sk bs

unboxSignedBox :: forall p e . (Serialise p, ForRefChans e, Signatures (Encryption e))
               => LBS.ByteString
               -> Maybe (PubKey 'Sign (Encryption e), p)

unboxSignedBox bs = runIdentity $ runMaybeT do

  (SignedBox pk bs sign) <- MaybeT $ pure
                                   $ deserialiseOrFail @(SignedBox p e) bs
                                       & either (pure Nothing) Just

  guard $ verifySign @(Encryption e) pk sign bs

  p <- MaybeT $ pure $ deserialiseOrFail @p (LBS.fromStrict bs) & either (const Nothing) Just

  pure (pk, p)

instance ForRefChans e => FromStringMaybe (RefChanHeadBlock e) where
  fromStringMay str = RefChanHeadBlockSmall <$> version
                                          <*> quorum
                                          <*> wait
                                          <*> pure peers
                                          <*> pure authors
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
               vcat (fmap peer (view refChanHeadPeers blk)) <> line
               <>
               vcat (fmap author (view refChanHeadAuthors blk)) <> line

    where
      peer (p,w) = parens ("peer" <+> dquotes (pretty (AsBase58 p)) <+> pretty w)
      author p   = parens ("author" <+> dquotes (pretty (AsBase58 p)))


-- TODO: implement-refchans-head
--   Сгенерировать транзакцию RefHead
--   Послать транзакцию RefHead
--   Принять транзакцию RefHead
--   Валидировать транзакцию RefHead
--   Ответить на запрос RefChanGetHead
--
--  Как послать:
--    надо сохранить и как-то передать в серверную часть пира
--    или просто как-то передать в серверную часть пира.
--    Блок может быть довольно большим (больше UDP) пакета
--
--  Вариант 1.
--    Сохраняем hbs2 и дальше оперируем уже хэшем
--    дерева.
--    Что если пир на другом хосте? Черт
--    его знает уже. Через HTTP API?
--
--  Вариант 2.
--    Можно тоже самое из пира, но тогда надо узнать
--    его сторейдж или всё-таки найти способ передать транзакцию
--    ему в контекст
--
--
--



