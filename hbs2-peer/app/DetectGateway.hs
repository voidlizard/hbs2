{-# Language AllowAmbiguousTypes #-}
module DetectGateway (detectGatewayLoop) where

import HBS2.Prelude.Plated
import HBS2.Clock
import HBS2.Net.Messaging
import HBS2.Net.Proto.Types
import HBS2.OrDie
import HBS2.Net.Messaging.UDP

import HBS2.System.Logger.Simple

import Data.Attoparsec.Text as Atto
import Data.Functor
import Control.Concurrent.Async
-- import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.ByteString.Lazy.Char8 (ByteString)
import Control.Monad
import Data.Text.Encoding qualified as T
import Data.Text as T
import Data.List qualified as List
import Data.Maybe

detectGatewayLoop :: forall m . (MonadIO m) => m ()
detectGatewayLoop = do

  mlisten <- newMessagingUDPMulticast upnpMulticast
               `orDie` "Can't start UPnP protocol"

  trace $ "UPnP listen:" <+> pretty (listenAddr mlisten)

  upaddr <- parsePeerAddr upnpMulticast
            `orDie` "Can't parse upnp address"

  udpMessListen <- liftIO $ async $ runMessagingUDP mlisten


  reqLoop <- liftIO $ async $ forever do
    sendTo mlisten (To upaddr) (From upaddr) gwDetectMsg
    pause @'Seconds 10

  forever $ do
    debug "detectGatewayLoop"

    answ <- receive @_ @UDP @ByteString mlisten (To upaddr)

    gwAddrs <- forM answ $ \(From sa,msg) -> do
                let txt = foldMap T.words $ T.lines $ T.decodeUtf8 $ LBS.toStrict msg
                debug $ pretty sa <+> pretty (show msg)
                pure Nothing
                -- maybe1 (List.find (== "urn:schemas-upnp-org:device:InternetGatewayDevice:1") txt)
                --        (pure Nothing)
                --        (const $ pure $ Just sa)

    pure ()
    -- let gwAddr = headMay $ catMaybes gwAddrs

    -- TODO: what-if-more-than-one-gateway
    -- maybe1 gwAddr none $ \gwa -> do
    --   debug $ "FOUND FIRST GATEWAY:" <+> pretty gwa

    -- FIXME: remove-debug-hardcode
    -- pause @'Seconds 30

  -- void $ liftIO $ waitAnyCatchCancel [udpMessSend] -- , udpMessListen]

  where
    upnpMulticast = "239.255.255.250:1900"
    gwDetectMsg = LBS.intercalate "\r\n"
      [ "M-SEARCH * HTTP/1.1"
      , "HOST: 239.255.255.250:1900"
      , "MAN: \"ssdp:discover\""
      , "MX: 2"
      , "ST: urn:schemas-upnp-org:device:InternetGatewayDevice:1"
      ]

