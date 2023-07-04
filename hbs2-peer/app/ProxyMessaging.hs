{-# Language TemplateHaskell #-}
module ProxyMessaging
  ( ProxyMessaging
  , PlainProxyMessaging(..)
  , newProxyMessaging
  , runProxyMessaging
  , proxyEncryptionKeys
  , sendToPlainProxyMessaging
  ) where

import HBS2.Prelude.Plated
import HBS2.Net.Messaging
import HBS2.Clock
import HBS2.Crypto
import HBS2.Net.Auth.Credentials
import HBS2.Net.Proto.Definition ()
import HBS2.Net.Proto.Peer
import HBS2.Net.Proto.Types
import HBS2.Net.Messaging.UDP
import HBS2.Net.Messaging.TCP

import HBS2.System.Logger.Simple

import Crypto.Saltine.Class as SCl
import Crypto.Saltine.Core.Box qualified as Encrypt

import Codec.Serialise
import Control.Applicative
import Control.Arrow hiding ((<+>))
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue
import Control.Monad.Trans.Maybe
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Maybe
import Data.String.Conversions (cs)
import Data.List qualified as L
import Data.Map (Map)
import Data.Map qualified as Map
import Lens.Micro.Platform as Lens
import Control.Monad

-- TODO: protocol-encryption-goes-here

data ProxyMessaging =
  ProxyMessaging
  { _proxyUDP :: MessagingUDP
  , _proxyTCP  :: Maybe MessagingTCP
  , _proxyAnswers :: TQueue (From L4Proto, LBS.ByteString)
  , _proxyEncryptionKeys :: TVar (Map (Peer L4Proto) (CommonSecret (Encryption L4Proto)))
  }

newtype PlainProxyMessaging = PlainProxyMessaging ProxyMessaging

-- 1 нода X создаёт себе Encrypt.Keypair
-- 2 подписывает из него публичный ключ ключом подписи ноды X и отправляет ноде Y
-- 3 нода Y получила Публичный ключ ноды X, создала симметричный Key,
--   зашифровала его для полученного Публичного ключа ноды X и отравила ей

makeLenses 'ProxyMessaging

newProxyMessaging :: forall m . MonadIO m
                  => MessagingUDP
                  -> Maybe MessagingTCP
                  -> m ProxyMessaging

newProxyMessaging u t = liftIO do
  ProxyMessaging u t
    <$> newTQueueIO
    <*> newTVarIO mempty

runProxyMessaging :: forall m . MonadIO  m
                  => ProxyMessaging
                  -> m ()

runProxyMessaging env = liftIO do

  let udp = view proxyUDP env
  let answ = view proxyAnswers env
  let udpPeer = getOwnPeer udp

  u <- async $ forever do
          msgs <- receive  udp (To udpPeer)
          atomically $ do
            forM_ msgs $ writeTQueue answ

  t <- async $ maybe1 (view proxyTCP env) none $ \tcp -> do
          forever do
            msgs <- receive  tcp (To $ view tcpOwnPeer tcp)
            atomically $ do
              forM_ msgs $ writeTQueue answ

  liftIO $ mapM_ waitCatch [u,t]

instance Messaging ProxyMessaging L4Proto LBS.ByteString where
  sendTo = sendToPlainProxyMessaging
  receive = receiveFromProxyMessaging

sendToPlainProxyMessaging :: (MonadIO m)
  => ProxyMessaging
  -> To L4Proto
  -> From L4Proto
  -> LBS.ByteString
  -> m ()
sendToPlainProxyMessaging bus t@(To whom) proto msg = do
    let udp = view proxyUDP bus
    case view sockType whom of
      UDP -> sendTo udp  t proto msg
      TCP -> maybe1 (view proxyTCP bus) none $ \tcp -> do
               sendTo tcp t proto msg

-- sendToProxyMessaging :: (MonadIO m)
--   => ProxyMessaging
--   -> To L4Proto
--   -> From L4Proto
--   -> LBS.ByteString
--   -> m ()
-- sendToProxyMessaging bus t@(To whom) proto msg = do
--     -- sendTo (view proxyUDP bus)  t proto msg
--     -- trace $ "PROXY: SEND" <+> pretty whom
--     encKey <- Map.lookup whom <$> (liftIO . readTVarIO) (view proxyEncryptionKeys bus)
--     cf <- case encKey of
--         Nothing -> do
--             trace $ "ENCRYPTION SEND: sending plain message to" <+> pretty whom
--             pure id
--         Just k -> do
--             trace $ "ENCRYPTION SEND: sending encrypted message to" <+> pretty whom <+> "with key" <+> viaShow k
--             boxAfterNMLazy k <$> liftIO Encrypt.newNonce
--     sendTo (PlainProxyMessaging bus) t proto (cf msg)

-- receiveFromProxyMessaging :: MonadIO m
--   => ProxyMessaging -> To L4Proto -> m [(From L4Proto, LBS.ByteString)]
-- receiveFromProxyMessaging bus _ = liftIO do
--     -- trace "PROXY: RECEIVE"
--     -- receive (view proxyUDP bus) w
--     let answ = view proxyAnswers bus
--     rs <- atomically $ liftM2 (:) (readTQueue answ) (flushTQueue answ)
--     fmap catMaybes $ forM rs \(w@(From whom), msg) -> do
--         encKeys <- (liftIO . readTVarIO) (view proxyEncryptionKeys bus)
--         fmap (w, ) <$> dfm whom (Map.lookup whom encKeys) msg

--     where
--       dfm :: Peer L4Proto -> Maybe Encrypt.CombinedKey -> LBS.ByteString -> IO (Maybe LBS.ByteString)
--       dfm = \whom mk msg -> case mk of
--           Nothing -> do
--               trace $ "ENCRYPTION RECEIVE: we do not have a key to decode" <+> pretty whom
--               pure (Just msg)
--           Just k -> runMaybeT $
--             -- А будем-ка мы просто передавать сообщение дальше как есть, если не смогли расшифровать
--             (<|> (do
--                 -- И сотрём ключ из памяти
--                 -- liftIO $ atomically $ modifyTVar' (view proxyEncryptionKeys bus) $ Lens.at whom .~ Nothing
--                 trace $ "ENCRYPTION RECEIVE: got plain message. clearing key of" <+> pretty whom
--                 pure msg
--                 )) $
--             do
--               trace $ "ENCRYPTION RECEIVE: we have a key to decode from" <+> pretty whom <+> ":" <+> viaShow k
--               case ((extractNonce . cs) msg) of
--                   Nothing -> do
--                       trace $ "ENCRYPTION RECEIVE: can not extract nonce from" <+> pretty whom <+> "message" <+> viaShow msg
--                       fail ""

--                   Just (nonce, msg') ->
--                       ((MaybeT . pure) (boxOpenAfterNMLazy k nonce msg')
--                           <* (trace $ "ENCRYPTION RECEIVE: message successfully decoded from" <+> pretty whom)
--                       )
--                     <|>
--                       (do
--                           (trace $ "ENCRYPTION RECEIVE: can not decode message from" <+> pretty whom)
--                           fail ""

--                           -- -- Попытаться десериализовать сообщение как PeerPing или PeerPingCrypted
--                           -- case deserialiseOrFail msg of
--                           --   Right (_ :: PeerHandshake L4Proto) -> do
--                           --       trace $ "ENCRYPTION RECEIVE: plain message decoded as PeerHandshake" <+> pretty whom
--                           --       fail ""
--                           --   Left _ -> do
--                           --       trace $ "ENCRYPTION RECEIVE: failed" <+> pretty whom
--                           --       mzero

--                       )
