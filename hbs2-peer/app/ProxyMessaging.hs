{-# Language TemplateHaskell #-}
module ProxyMessaging
  ( ProxyMessaging(..)
  , newProxyMessaging
  , runProxyMessaging
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

import PeerTypes

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

  , _proxy_getEncryptionKey :: Peer L4Proto -> IO (Maybe (CommonSecret (Encryption L4Proto)))
  , _proxy_clearEncryptionKey :: Peer L4Proto -> IO ()
  , _proxy_sendResetEncryptionKeys :: Peer L4Proto -> IO ()
  , _proxy_sendBeginEncryptionExchange :: Peer L4Proto -> IO ()
  }

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
    let _proxyUDP = u
    let _proxyTCP = t
    _proxyAnswers <- newTQueueIO

    let _proxy_getEncryptionKey = const (pure Nothing)
    let _proxy_clearEncryptionKey = const (pure ())
    let _proxy_sendResetEncryptionKeys = const (pure ())
    let _proxy_sendBeginEncryptionExchange = const (pure ())

    pure ProxyMessaging {..}

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

  sendTo = sendToProxyMessaging

  receive = receiveFromProxyMessaging

  -- receive bus _ = liftIO do
  --   -- trace "PROXY: RECEIVE"
  --   -- receive (view proxyUDP bus) w
  --   let answ = view proxyAnswers bus
  --   atomically $ do
  --     r <- readTQueue answ
  --     rs <- flushTQueue answ
  --     pure (r:rs)

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

sendToProxyMessaging :: (MonadIO m)
  => ProxyMessaging
  -> To L4Proto
  -> From L4Proto
  -> LBS.ByteString
  -> m ()
sendToProxyMessaging bus t@(To whom) proto msg = do
    -- sendTo (view proxyUDP bus)  t proto msg
    -- trace $ "PROXY: SEND" <+> pretty whom
    mencKey <- liftIO $ _proxy_getEncryptionKey bus whom
    cf <- case mencKey of
        Nothing -> do
            trace1 $ "ENCRYPTION SEND: sending plain message to" <+> pretty whom
            pure id
        Just k -> do
            trace1 $ "ENCRYPTION SEND: sending encrypted message to" <+> pretty whom <+> "with key" <+> viaShow k
            boxAfterNMLazy k <$> liftIO Encrypt.newNonce
    sendToPlainProxyMessaging bus t proto (cf msg)

receiveFromProxyMessaging :: MonadIO m
  => ProxyMessaging -> To L4Proto -> m [(From L4Proto, LBS.ByteString)]
receiveFromProxyMessaging bus _ = liftIO do
    -- trace "PROXY: RECEIVE"
    -- receive (view proxyUDP bus) w
    let answ = view proxyAnswers bus
    rs <- atomically $ liftM2 (:) (readTQueue answ) (flushTQueue answ)
    fmap catMaybes $ forM rs \(w@(From whom), msg) -> do
        fmap (w, ) <$> dfm whom msg

    -- Здесь:
    -- 1. У нас есть ключ сессии и мы не смогли расшифровать -> do
    --     удаляем у себя ключ
    --     отправляем sendBeginEncryptionExchange
    -- 2. У нас (до сих пор, даже если мы давно стартовали) нет ключа сессии -> do
    --     sendResetEncryptionKeys
    --     просто передаём сообщение как есть

    -- В протоколе пингов:
    -- 1. Если слишком долго нет ответа на ping, то удаляем у себя ключ, отправляем sendResetEncryptionKeys
    --       Выполняется в PeerInfo:
    --       emit PeerExpiredEventKey (PeerExpiredEvent @e p mpeerData)

    where
      dfm :: Peer L4Proto -> LBS.ByteString -> IO (Maybe LBS.ByteString)
      dfm = \whom msg -> liftIO $ _proxy_getEncryptionKey bus whom >>= \case

          Nothing -> do
              trace1 $ "ENCRYPTION RECEIVE: we do not have a key to decode" <+> pretty whom
              liftIO $ _proxy_sendBeginEncryptionExchange bus whom
              pure (Just msg)

          Just k -> runMaybeT $
            -- А будем-ка мы просто передавать сообщение дальше как есть, если не смогли расшифровать
            (<|> (do

                liftIO $ _proxy_clearEncryptionKey bus whom

                liftIO $ _proxy_sendResetEncryptionKeys bus whom

                trace1 $ "ENCRYPTION RECEIVE: got plain message. clearing key of" <+> pretty whom
                pure msg
                )) $
            do
              trace $ "ENCRYPTION RECEIVE: we have a key to decode from" <+> pretty whom <+> ":" <+> viaShow k
              case ((extractNonce . cs) msg) of
                  Nothing -> do
                      trace1 $ "ENCRYPTION RECEIVE: can not extract nonce from" <+> pretty whom <+> "message" <+> viaShow msg
                      fail ""

                  Just (nonce, msg') ->
                      ((MaybeT . pure) (boxOpenAfterNMLazy k nonce msg')
                          <* (trace1 $ "ENCRYPTION RECEIVE: message successfully decoded from" <+> pretty whom)
                      )
                    <|>
                      (do
                          (trace1 $ "ENCRYPTION RECEIVE: can not decode message from" <+> pretty whom)
                          fail ""

                          -- -- Попытаться десериализовать сообщение как PeerPing или PeerPingCrypted
                          -- case deserialiseOrFail msg of
                          --   Right (_ :: PeerHandshake L4Proto) -> do
                          --       trace $ "ENCRYPTION RECEIVE: plain message decoded as PeerHandshake" <+> pretty whom
                          --       fail ""
                          --   Left _ -> do
                          --       trace $ "ENCRYPTION RECEIVE: failed" <+> pretty whom
                          --       mzero

                      )
