{-# Language UndecidableInstances #-}
{-# Language AllowAmbiguousTypes #-}
module HBS2.Net.Proto.RefChan.RefChanHead where

import HBS2.Prelude.Plated
import HBS2.Net.Proto
import HBS2.Net.Auth.Credentials
import HBS2.Base58
import HBS2.Net.Proto.Peer
import HBS2.Net.Proto.BlockAnnounce
import HBS2.Net.Proto.Sessions
import HBS2.Data.Types.Refs
import HBS2.Storage

import HBS2.Net.Proto.RefChan.Types

import HBS2.System.Logger.Simple

import Control.Monad.Trans.Maybe
import Data.Maybe


refChanHeadProto :: forall e s m proto . ( MonadIO m
                                         , Request e proto m
                                         , Response e proto m
                                         , Request e (BlockAnnounce e) m
                                         , HasPeerNonce e m
                                         , HasDeferred proto e m
                                         , IsPeerAddr e m
                                         , Pretty (Peer e)
                                         , Sessions e (KnownPeer e) m
                                         , HasStorage m
                                         , Signatures s
                                         , IsRefPubKey s
                                         , s ~ Encryption e
                                         , proto ~ RefChanHead e
                                         )
                  => Bool
                  -> RefChanAdapter e m
                  -> RefChanHead e
                  -> m ()

refChanHeadProto self adapter msg = do
  -- авторизовать пира
  peer <- thatPeer @proto

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

      RefChanGetHead chan -> deferred @proto do
        trace $ "RefChanGetHead" <+> pretty self <+> pretty (AsBase58 chan)

        sto <- getStorage
        ref <- MaybeT $ liftIO $ getRef sto (RefChanHeadKey @s chan)
        sz  <- MaybeT $ liftIO $ hasBlock sto ref

        let annInfo = BlockAnnounceInfo 0 NoBlockInfoMeta sz ref
        let announce = BlockAnnounce @e no annInfo
        lift $ request peer announce
        lift $ request peer (RefChanHead @e chan (RefChanHeadBlockTran (HashRef ref)))


