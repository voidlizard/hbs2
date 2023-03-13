{-# Language AllowAmbiguousTypes #-}
module DetectGateway (detectGatewayLoop) where

import HBS2.Prelude.Plated
import HBS2.Clock
import HBS2.Net.Messaging
import HBS2.OrDie
import HBS2.Net.Messaging.UDP
import HBS2.Events
import HBS2.Net.IP.Addr

import HBS2.System.Logger.Simple
import PeerTypes

import Data.Char (isSpace)
import Data.Attoparsec.Text as Atto
import Data.Functor
import Control.Concurrent.Async
-- import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.ByteString.Lazy.Char8 (ByteString)
import Control.Monad
import Data.Text.Encoding qualified as T
import Data.Text as T
import Data.Maybe
import Data.Either
import Data.Function

checkHTTPHead :: Text -> Bool
checkHTTPHead s = parseOnly pHead s & fromRight False

checkIGDString :: Text -> Bool
checkIGDString s = parseOnly pIGD s & fromRight False

pHead :: Parser Bool
pHead = do
  void $ string "HTTP/"
  skipWhile (not . isSpace) --  >> skipWhile isSpace
  void skipSpace
  void $ string "200"
  pure True

pIGD :: Parser Bool
pIGD = do
  -- ST: urn:schemas-upnp-org:device:InternetGatewayDevice:1
  void $ string "ST:"
  void skipSpace
  void $ string "urn:schemas-upnp-org:device:InternetGatewayDevice:1"
  pure True

detectGatewayLoop :: forall e m . (MonadIO m, EventEmitter e (UPnPGatewayDetect e) m) => m ()
detectGatewayLoop = do

  mlisten <- newMessagingUDPMulticast upnpMulticast
               `orDie` "Can't start UPnP protocol"

  trace $ "UPnP listen:" <+> pretty (listenAddr mlisten)

  upaddr <- parsePeerAddr upnpMulticast
            `orDie` "Can't parse upnp address"

  udpMessListen <- liftIO $ async $ runMessagingUDP mlisten


  let waitIGDAnswer = liftIO $ race (pause @'Seconds 5 >> pure Nothing) $
        fix \next -> do
          answ <- receive @_ @UDP @ByteString mlisten (To upaddr)
          gwAddrs <- forM answ $ \(From sa,msg) -> do
                      let txt = T.lines $ T.decodeUtf8 $ LBS.toStrict msg
                      trace $ "ANSW:" <+> pretty txt
                      let isAnsw = headMay txt <&> checkHTTPHead
                      let isIGD  = or $ fmap checkIGDString txt
                      if isAnsw == Just True && isIGD then
                        pure (Just sa)
                      else
                        pure Nothing

          let gwAddr = headMay $ catMaybes gwAddrs

          -- TODO: what-if-more-than-one-gateway
          maybe1 gwAddr next $ \gwa -> do
            pure (Just gwa)

  forever do
    debug "detectGatewayLoop"
    sendTo mlisten (To upaddr) (From upaddr) gwDetectMsg
    mbGwa <- waitIGDAnswer <&> fromRight Nothing

    maybe1 mbGwa (pause @'Seconds 60) $ \gwa -> do
            let wa = pause @'Seconds 1200
            let a = getHostPort (T.pack $ show $ pretty gwa)
            maybe1 a wa $ \(s,_) -> do
              debug $ "found some gateway:" <+> pretty s
              emit @e UPnPGatewayDetectKey (UPnPGatewayDetect s)
            wa

  void $ liftIO $ waitAnyCatchCancel [udpMessListen]

  where
    upnpMulticast = "239.255.255.250:1900"
    gwDetectMsg = LBS.intercalate "\r\n"
      [ "M-SEARCH * HTTP/1.1"
      , "HOST: 239.255.255.250:1900"
      , "MAN: \"ssdp:discover\""
      , "MX: 2"
      , "ST: urn:schemas-upnp-org:device:InternetGatewayDevice:1"
      ]

