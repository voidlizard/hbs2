module HttpWorker where

import HBS2.Prelude
import HBS2.Actors.Peer
import HBS2.Net.Proto.PeerMeta
import HBS2.Storage
import HBS2.Data.Types.Refs
import HBS2.Net.Proto.Types

import HBS2.System.Logger.Simple

import PeerTypes
import PeerConfig

import Data.Functor
import Data.Maybe
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import Data.ByteString.Lazy qualified as LBS
import Network.HTTP.Types.Status
import Network.Wai.Middleware.RequestLogger
import Text.InterpolatedString.Perl6 (qc)
import Web.Scotty



-- TODO: introduce-http-of-off-feature

httpWorker :: forall e s m . ( MyPeer e
                             , MonadIO m
                             , HasStorage m
                             , IsRefPubKey s
                             , s ~ Encryption e
                             ) => PeerConfig -> DownloadEnv e -> m ()

httpWorker conf e = do

  sto <- getStorage
  let port' = cfgValue @PeerHttpPortKey conf  <&>  fromIntegral

  maybe1 port' none $ \port -> liftIO do

    scotty port $ do
      middleware logStdout

      get "/size/:hash" do
        what <- param @String "hash" <&> fromString
        size <- liftIO $ hasBlock sto what
        case size of
          Nothing -> status status404
          Just n -> do
            json n

      get "/cat/:hash" do
        what <- param @String "hash" <&> fromString
        blob <- liftIO $ getBlock sto what
        case blob of
          Nothing -> status status404
          Just lbs -> do
            addHeader "content-type" "application/octet-stream"
            addHeader "content-length" [qc|{LBS.length lbs}|]
            raw lbs

      get "/reflog/:ref" do
        re <- param @String "ref" <&> fromStringMay
        case re of
          Nothing -> status status404
          Just ref -> do
            va <- liftIO $ getRef sto (RefLogKey @s ref)
            maybe1 va (status status404) $ \val -> do
              text [qc|{pretty val}|]

      get "/metadata" do
          let mport = cfgValue @PeerHttpPortKey conf  <&>  fromIntegral
          raw $ serialise . annMetaFromPeerMeta . PeerMeta . catMaybes $
            [ mport <&> \port -> ("http-port", TE.encodeUtf8 . Text.pack . show $ port)
            ]

      put "/" do
        -- FIXME: optional-header-based-authorization
        --   signed nonce + peer key?

        -- TODO: ddos-protection
        -- FIXME: fix-max-size-hardcode
        bs <- LBS.take 4194304 <$> body
        -- let ha = hashObject @HbSync bs
        -- here <- liftIO $ hasBlock sto ha <&> isJust

        mbHash <- liftIO $ putBlock sto bs

        case mbHash of
          Nothing -> status status500
          Just h  -> text [qc|{pretty h}|]

  pure ()

