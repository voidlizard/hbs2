module HBS2.Peer.Proto.LWWRef.Internal
  ( module HBS2.Peer.Proto.LWWRef.Internal
  , module HBS2.Peer.Proto.LWWRef
  ) where

import HBS2.Prelude.Plated
import HBS2.Peer.Proto.LWWRef
import HBS2.Data.Types.SignedBox
import HBS2.Storage

import HBS2.Hash
import HBS2.Clock
import HBS2.Net.Proto
import HBS2.Net.Auth.Credentials
import HBS2.Base58
import HBS2.Events
import HBS2.Actors.Peer.Types
import HBS2.Peer.Proto.Peer
import HBS2.Net.Proto.Sessions
import HBS2.Data.Types.Refs
import HBS2.Misc.PrettyStuff
import HBS2.System.Logger.Simple

import Codec.Serialise
import Control.Monad
import Control.Monad.Trans.Maybe
import Data.Maybe

{- HLINT ignore "Functor law" -}


data LWWRefProtoAdapter e m =
  LWWRefProtoAdapter
  { lwwFetchBlock :: Hash HbSync -> m ()
  }

lwwRefProto :: forall e s m proto . ( MonadIO m
                                    , ForLWWRefProto e
                                    , Request e proto m
                                    , Response e proto m
                                    , HasDeferred proto e m
                                    , HasGossip e (LWWRefProto e) m
                                    , HasStorage m
                                    , IsPeerAddr e m
                                    , Pretty (Peer e)
                                    , Sessions e (KnownPeer e) m
                                    , Signatures s
                                    , Pretty (AsBase58 (PubKey 'Sign s))
                                    , s ~ Encryption e
                                    , proto ~ LWWRefProto e
                                    )
                  => LWWRefProtoAdapter e m
                  -> LWWRefProto e -> m ()

lwwRefProto adapter pkt@(LWWRefProto1 req) = do
  debug $ yellow "lwwRefProto"

  case req of
    LWWProtoGet key -> deferred @proto $ void $ runMaybeT do
      sto <- getStorage

      ref <- getRef sto key   >>= toMPlus

      box <- getBlock sto ref
                 >>= toMPlus
                 <&> deserialiseOrFail
                 >>= toMPlus

      lift $ response (LWWRefProto1 (LWWProtoSet @e key box))

    LWWProtoSet key box -> void $ runMaybeT do

      (puk, lww) <- MaybeT $ pure $ unboxSignedBox0 box

      guard ( puk == fromLwwRefKey key )

      deferred @proto do

        sto <- getStorage

        let bs = serialise box
        let h0  = hashObject @HbSync bs

        new <- hasBlock sto h0 <&> isNothing

        when new do
          lift $ gossip pkt

        lift $ lwwFetchBlock adapter (fromHashRef (lwwValue lww))

        getRef sto key >>= \case
          Nothing -> do
            h <- enqueueBlock sto bs >>= toMPlus
            updateRef sto key h

          Just rv -> do
            blk' <- getBlock sto rv
            maybe1 blk' (forcedUpdateLwwRef sto key bs) $ \blk -> do
              let seq0 = deserialiseOrFail @(SignedBox (LWWRef e) e) blk
                             & either (const Nothing) Just
                             >>= unboxSignedBox0
                             <&> snd
                             <&> lwwSeq

              when (Just (lwwSeq lww) > seq0) do
                forcedUpdateLwwRef sto key (serialise box)

    where
      forcedUpdateLwwRef sto key bs = do
        h' <- enqueueBlock sto bs
        forM_ h' $ updateRef sto key

