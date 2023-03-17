{-# LANGUAGE AllowAmbiguousTypes #-}
module HBS2.Refs.Linear where

import HBS2.Actors
import HBS2.Clock
import HBS2.Data.Types.Refs
import HBS2.Defaults
import HBS2.Events
import HBS2.Hash
import HBS2.Merkle
import HBS2.Net.Auth.Credentials
import HBS2.Net.Messaging
import HBS2.Net.PeerLocator
import HBS2.Net.PeerLocator.Static
import HBS2.Net.Proto
import HBS2.Net.Proto.Sessions
import HBS2.OrDie
import HBS2.Prelude.Plated
import HBS2.Storage

import Codec.Serialise (serialise, deserialiseOrFail)
import Control.Monad.Trans.Maybe
import Data.ByteString.Lazy qualified as LBS
import Data.Maybe
import Data.Set qualified as Set

modifyLinearRef :: forall e.
    ( Signatures e
    , Serialise (Signature e)
    , Serialise (PubKey 'Sign e)
    , Eq (PubKey 'Sign e)
    , Block LBS.ByteString ~ LBS.ByteString
    )
  => AnyStorage
  -> PeerCredentials e   -- owner keyring
  -> Hash HbSync         -- channel id
  -> (Maybe (Hash HbSync) -> IO (Hash HbSync))
  -> IO ()
modifyLinearRef ss kr chh modIO = do
    g :: RefGenesis e <- (((either (const Nothing) Just . deserialiseOrFail) =<<)
        <$> getBlock ss chh)
      `orDie` "can not read channel ref genesis"
    when (refOwner g /= _peerSignPk kr) do
      (pure Nothing) `orDie` "channel ref owner does not match genesis owner"
    mrefvalraw <- readLinkRaw ss chh
    lmr <- case mrefvalraw of
        Nothing -> do
            val <- modIO Nothing
            pure LinearMutableRef
                { lrefId = chh
                , lrefHeight = 0
                , lrefVal = val
                }
        Just refvalraw -> do
            -- FIXME: do not increment counter if value is the same
            LinearMutableRefSigned _ ref :: Signed SignaturePresent (MutableRef e 'LinearRef)
                <- pure ((either (const Nothing) Just . deserialiseOrFail) refvalraw)
                `orDie` "can not parse channel ref"
            -- guard $ lrefId ref == chh
            val <- modIO (Just (lrefVal ref))
            pure LinearMutableRef
                { lrefId = chh
                , lrefHeight = lrefHeight ref + 1
                , lrefVal = val
                }
    (writeLinkRaw ss chh . serialise) (signLinearMutableRef @e (_peerSignSk kr) lmr)
      `orDie` "can not write link"
    pure ()

signLinearMutableRef :: forall e. (Signatures e)
  => PrivKey 'Sign e -> MutableRef e 'LinearRef -> Signed 'SignaturePresent (MutableRef e 'LinearRef)
signLinearMutableRef sk lmr = LinearMutableRefSigned @e ((makeSign @e sk . LBS.toStrict . serialise) lmr) lmr

verifyLinearMutableRefSigned :: forall e. (Signatures e)
  => PubKey 'Sign e
  -> Signed SignaturePresent (MutableRef e 'LinearRef)
  -> Maybe (Signed SignatureVerified (MutableRef e 'LinearRef))
verifyLinearMutableRefSigned pk lref = do
  guard $ verifySign @e pk (lmrefSignature lref) dat
  pure (LinearMutableRefSignatureVerified (lmrefSignature lref) (lmrefSignedRef lref) pk)
  where
    dat = (LBS.toStrict . serialise) (lmrefSignedRef lref)

tryUpdateLinearRefSigned :: forall e.
    ( Signatures e
    , Serialise (Signature e)
    , Serialise (PubKey 'Sign e)
    , Eq (PubKey 'Sign e)
    , Block LBS.ByteString ~ LBS.ByteString
    )
  => AnyStorage
  -> Signed SignaturePresent (MutableRef e 'LinearRef)
  -> IO Bool
tryUpdateLinearRefSigned st slref = do
    (maybe (pure False) pure =<<) $ runMaybeT do
        g :: RefGenesis e <- MaybeT $
            (((either (const Nothing) Just . deserialiseOrFail) =<<)
              <$> getBlock st ((lrefId . lmrefSignedRef) slref))
        vlref <- MaybeT . pure $ (verifyLinearMutableRefSigned (refOwner g) slref)
        lift $ tryUpdateLinearRef st vlref

tryUpdateLinearRef :: forall e.
    ( Signatures e
    , Serialise (Signature e)
    , Serialise (PubKey 'Sign e)
    , Eq (PubKey 'Sign e)
    , Block LBS.ByteString ~ LBS.ByteString
    )
  => AnyStorage
  -> Signed SignatureVerified (MutableRef e 'LinearRef)
  -> IO Bool
tryUpdateLinearRef ss vlref = do
    let chh = lrefId . lmrefVSignedRef $ vlref
    g :: RefGenesis e <- (((either (const Nothing) Just . deserialiseOrFail) =<<)
        <$> getBlock ss chh)
      `orDie` "can not read channel ref genesis"
    when (refOwner g /= lmrefVSigner vlref) do
      (pure Nothing) `orDie` "channel ref signer does not match genesis owner"
    -- Достать наше текущее значение ссылки, сравнить счётчик
    mrefvalraw <- readLinkRaw ss chh
    allowUpdate <- case mrefvalraw of
        Nothing -> pure True
        Just refvalraw -> do
            LinearMutableRefSigned _ ref :: Signed SignaturePresent (MutableRef e 'LinearRef)
                <- pure ((either (const Nothing) Just . deserialiseOrFail) refvalraw)
                `orDie` "can not parse channel ref"
            -- Если новое значение больше, обновить его
            pure (lrefHeight ref < lrefHeight (lmrefVSignedRef vlref))
    if allowUpdate
      then do
          (writeLinkRaw ss chh . serialise)
              (LinearMutableRefSigned @e (lmrefVSignature vlref) (lmrefVSignedRef vlref))
              `orDie` "can not write link"
          pure True
      else (pure False)

modifyNodeLinearRefList :: forall e.
    ( Signatures e
    , Serialise (Signature e)
    , Serialise (PubKey 'Sign e)
    , Eq (PubKey 'Sign e)
    , Block LBS.ByteString ~ LBS.ByteString
    )
    => AnyStorage -> PeerCredentials e -> Hash HbSync -> ([Hash HbSync] -> [Hash HbSync]) -> IO ()
modifyNodeLinearRefList ss kr chh f =
    modifyLinearRef ss kr chh \mh -> do
        v <- case mh of
            Nothing -> pure mempty
            Just h -> fromMaybe mempty . ((either (const Nothing) Just . deserialiseOrFail) =<<)
                <$> getBlock ss h
        (putBlock ss . serialise) (f v)
            `orDie` "can not put new node channel list block"

readNodeLinearRefList :: forall e.
    ( Signatures e
    , Serialise (Signature e)
    , Serialise (PubKey 'Sign e)
    , Eq (PubKey 'Sign e)
    , Block LBS.ByteString ~ LBS.ByteString
    )
    => AnyStorage -> PubKey 'Sign e -> IO [Hash HbSync]
readNodeLinearRefList ss pk = do
    -- полученный хэш будет хэшем ссылки на список референсов ноды
    lrh :: h <- pure $ (hashObject . serialise) (nodeLinearRefsRef @e pk)
    readLinkRaw ss lrh >>= \case
      Nothing -> pure []
      Just refvalraw -> do
          LinearMutableRefSigned _ ref
              <- pure ((either (const Nothing) Just . deserialiseOrFail @(Signed SignaturePresent (MutableRef e 'LinearRef))) refvalraw)
              `orDie` "can not parse channel ref"
          fromMaybe mempty . ((either (const Nothing) Just . deserialiseOrFail) =<<)
              <$> getBlock ss (lrefVal ref)

nodeRefListNew :: forall e.
    ( Signatures e
    , Serialise (Signature e)
    , Serialise (PubKey 'Sign e)
    , Eq (PubKey 'Sign e)
    , Block LBS.ByteString ~ LBS.ByteString
    )
    => AnyStorage -> PeerCredentials e -> PubKey 'Sign e -> Text -> AnnMetaData -> IO (Hash HbSync)
nodeRefListNew st nodeCred ownerPk title meta = do
  -- полученный хэш будет хэшем ссылки на созданный канал владельца c ownerCred
  chh <- (putBlock st . serialise) (RefGenesis @e ownerPk title meta)
      `orDie` "can not put channel genesis block"
  nodeRefListAdd st nodeCred chh
  pure chh

nodeRefListAdd :: forall e.
    ( Signatures e
    , Serialise (Signature e)
    , Serialise (PubKey 'Sign e)
    , Eq (PubKey 'Sign e)
    , Block LBS.ByteString ~ LBS.ByteString
    )
    => AnyStorage -> PeerCredentials e -> Hash HbSync -> IO ()
nodeRefListAdd st nodeCred chh = do
  -- полученный хэш будет хэшем ссылки на список референсов ноды
  lrh <- (putBlock st . serialise) (nodeLinearRefsRef @e (_peerSignPk nodeCred))
      `orDie` "can not create node refs genesis"
  modifyNodeLinearRefList st nodeCred lrh $ Set.toList . Set.insert chh . Set.fromList

