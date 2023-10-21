{-# Language TypeOperators #-}
module HttpWorker where

import HBS2.Prelude
import HBS2.Clock
import HBS2.Actors.Peer
import HBS2.Storage
import HBS2.Data.Types.Refs
import HBS2.Merkle (AnnMetaData)
import HBS2.Net.Proto.Types
import HBS2.Net.Proto.RefLog
import HBS2.Events

import HBS2.System.Logger.Simple

import PeerTypes
import PeerConfig
import RefLog ( doRefLogBroadCast )

import Data.ByteString.Lazy qualified as LBS
import Network.HTTP.Types.Status
import Network.Wai.Middleware.RequestLogger
import Text.InterpolatedString.Perl6 (qc)
import Web.Scotty
import Codec.Serialise (deserialiseOrFail)
import Data.Aeson (object, (.=))
import Control.Monad.Reader
import Lens.Micro.Platform (view)

-- TODO: introduce-http-of-off-feature

httpWorker :: forall e s m . ( MyPeer e
                             , MonadIO m
                             , HasStorage m
                             , IsRefPubKey s
                             , s ~ Encryption e
                             , m ~ PeerM e IO
                             , e ~ L4Proto
                             ) => PeerConfig -> AnnMetaData -> DownloadEnv e -> m ()

httpWorker (PeerConfig syn) pmeta e = do

  sto <- getStorage
  let port' = runReader (cfgValue @PeerHttpPortKey) syn  <&>  fromIntegral
  penv <- ask

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

      -- FIXME: to-replace-to-rpc
      post "/reflog" do
        bs <- LBS.take 4194304 <$> body
        let msg' =
              deserialiseOrFail @(RefLogUpdate L4Proto) bs
                & either (const Nothing) Just
        case msg' of
          Nothing -> do
            status status400
            json $ object ["error" .= "unable to parse RefLogUpdate message"]
          Just msg -> do
            let pubk = view refLogId msg
            liftIO $ withPeerM penv $ do
              emit @e RefLogUpdateEvKey (RefLogUpdateEvData (pubk, msg, Nothing))
              doRefLogBroadCast msg
            status status200

      get "/metadata" do
          raw $ serialise $ pmeta

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

  warn "http port not set"
  forever $ pause @'Seconds 600

