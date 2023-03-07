module HBS2.Refs.Linear where

import HBS2.Actors
import HBS2.Clock
import HBS2.Data.Types.Refs
import HBS2.Defaults
import HBS2.Events
import HBS2.Hash
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
import Data.ByteString.Lazy qualified as LBS
import Data.Maybe
import Data.Set qualified as Set

modifyLinearRef :: forall e st block h.
    ( e ~ [h]
    , h ~ Hash HbSync
    , Signatures e
    , Serialise (Signature e)
    , Serialise (PubKey 'Sign e)
    , Eq (PubKey 'Sign e)
    , Block block ~ LBS.ByteString
    , Storage (st HbSync) HbSync block IO
    -- , IsKey HbSync, Key HbSync ~ h
    )
  => st HbSync
  -> PeerCredentials e   -- owner keyring
  -> h                   -- channel id
  -> (Maybe (h) -> IO (h))
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
    (writeLinkRaw ss chh . serialise)
      (LinearMutableRefSigned @e ((makeSign @e (_peerSignSk kr) . LBS.toStrict . serialise) lmr) lmr)
      `orDie` "can not write link"
    pure ()

verifyLinearMutableRefSigned :: forall e. (Signatures e)
  => PubKey 'Sign e
  -> Signed SignaturePresent (MutableRef e 'LinearRef)
  -> Maybe (Signed SignatureVerified (MutableRef e 'LinearRef))
verifyLinearMutableRefSigned pk lref = do
  guard $ verifySign @e pk (lmrefSignature lref) dat
  pure (LinearMutableRefSignatureVerified (lmrefSignature lref) (lmrefSignedRef lref) pk)
  where
    dat = (LBS.toStrict . serialise) (lmrefSignedRef lref)

tryUpdateLinearRef :: forall e st block h.
    ( e ~ [h]
    , h ~ Hash HbSync
    , Signatures e
    , Serialise (Signature e)
    , Serialise (PubKey 'Sign e)
    , Eq (PubKey 'Sign e)
    , Block block ~ LBS.ByteString
    , Storage (st HbSync) HbSync block IO
    -- , IsKey HbSync, Key HbSync ~ h
    )
  => st HbSync
  -> h                -- channel id
  -> Signed SignatureVerified (MutableRef e 'LinearRef)
  -> IO Bool
tryUpdateLinearRef ss chh vlref = do
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

modifyNodeLinearRefList :: forall e st block h.
    ( e ~ [h]
    , h ~ Hash HbSync
    , Signatures e
    , Serialise (Signature e)
    , Serialise (PubKey 'Sign e)
    , Eq (PubKey 'Sign e)
    , Block block ~ LBS.ByteString
    , Storage (st HbSync) HbSync block IO
    -- , IsKey HbSync, Key HbSync ~ h
    )
    => st HbSync -> PeerCredentials e -> h -> ([h] -> [h]) -> IO ()
modifyNodeLinearRefList ss kr chh f =
    modifyLinearRef ss kr chh \mh -> do
        v <- case mh of
            Nothing -> pure mempty
            Just h -> fromMaybe mempty . ((either (const Nothing) Just . deserialiseOrFail) =<<)
                <$> getBlock ss h
        (putBlock ss . serialise) (f v)
            `orDie` "can not put new node channel list block"

readNodeLinearRefList :: forall e st block h.
    ( e ~ [h]
    , h ~ Hash HbSync
    , Signatures e
    , Serialise (Signature e)
    , Serialise (PubKey 'Sign e)
    , Eq (PubKey 'Sign e)
    , Block block ~ LBS.ByteString
    , Storage (st HbSync) HbSync block IO
    -- , IsKey HbSync, Key HbSync ~ h
    )
    => st HbSync -> PubKey 'Sign e -> IO [h]
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

nodeRefListAdd :: forall e st block h.
    ( e ~ [h]
    , h ~ Hash HbSync
    , Signatures e
    , Serialise (Signature e)
    , Serialise (PubKey 'Sign e)
    , Eq (PubKey 'Sign e)
    , Block block ~ LBS.ByteString
    , Storage (st HbSync) HbSync block IO
    -- , IsKey HbSync, Key HbSync ~ h
    )
    => st HbSync -> PeerCredentials e -> h -> IO ()
nodeRefListAdd ss nodeCred chh = do
  -- полученный хэш будет хэшем ссылки на список референсов ноды
  lrh <- (putBlock ss . serialise) (nodeLinearRefsRef @e (_peerSignPk nodeCred))
      `orDie` "can not create node refs genesis"
  modifyNodeLinearRefList ss nodeCred lrh $ Set.toList . Set.insert chh . Set.fromList
