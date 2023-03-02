module HttpWorker where

import HBS2.Prelude
import HBS2.Actors.Peer
import HBS2.Storage

import HBS2.System.Logger.Simple

import PeerTypes
import PeerConfig

import Data.Maybe
import Data.Function
import Data.Functor
import Data.Text.Lazy qualified as Text
import Data.ByteString.Lazy qualified as LBS
import Network.HTTP.Types.Status
import Network.Wai.Middleware.RequestLogger
import Text.InterpolatedString.Perl6 (qc)
import Web.Scotty



-- TODO: introduce-http-of-off-feature

httpWorker :: forall e m . ( MyPeer e
                           , MonadIO m
                           , HasStorage m
                           ) => PeerConfig -> DownloadEnv e -> m ()

httpWorker conf e = do

  sto <- getStorage
  let port' = cfgValue @PeerHttpPortKey conf  <&>  fromIntegral

  maybe1 port' none $ \port -> liftIO do

    scotty port $ do
      middleware logStdoutDev

      get "/cat/:hash" do
        what <- param @String "hash" <&> fromString
        blob <- liftIO $ getBlock sto what
        case blob of
          Nothing -> status status404
          Just lbs -> do
            addHeader "content-type" "application/octet-stream"
            addHeader "content-length" [qc|{LBS.length lbs}|]
            raw lbs

    -- get (regex "^/(.+)/refs/(.+)$") $ do
    --   to <-  fromString <$> param "1"
    --   ref' <- fromString <$> param "2"
    --   liftIO $ putStrLn [qc|{to} {ref'}|]
    --   let ref = "refs" <> "/" <> ref'
    --   refval <- indexGetRef to ref
    --   case refval of
    --     Nothing  -> status status404
    --     Just val -> do
    --       let s = (show $ pretty val) <> "\n"
    --       let lbs = fromString s
    --       addHeader "content-type" "application/octet-stream"
    --       addHeader "content-length" [qc|{LBS.length lbs}|]
    --       raw lbs


  pure ()

