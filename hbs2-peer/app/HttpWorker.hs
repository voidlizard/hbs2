{-# Language TypeOperators #-}
module HttpWorker where

import HBS2.Prelude
import HBS2.Hash
import HBS2.Actors.Peer
import HBS2.Storage
import HBS2.Data.Detect
import HBS2.Data.Types.Refs
import HBS2.Merkle
import HBS2.Peer.Proto
import HBS2.Peer.Proto.LWWRef
import HBS2.Net.Auth.Schema
import HBS2.Data.Types.SignedBox
import HBS2.Events
import HBS2.Storage.Operations.ByteString

import PeerTypes
import PeerConfig
import RefLog ( doRefLogBroadCast )

import Data.Config.Suckless

import Data.ByteString.Lazy qualified as LBS
import Network.HTTP.Types.Status
import Network.Wai.Middleware.RequestLogger
import Text.InterpolatedString.Perl6 (qc)
import Web.Scotty

import Data.ByteString.Builder (byteString, Builder)

import Data.Either
import Codec.Serialise (deserialiseOrFail)
import Data.Aeson (object, (.=))
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Control.Monad.Reader
import Lens.Micro.Platform (view)
import System.FilePath
import Control.Monad.Except
import Control.Monad.Trans.Cont

{- HLINT ignore "Functor law" -}

-- TODO: introduce-http-of-off-feature

extractMetadataHash :: Hash HbSync -> LBS.ByteString -> Maybe (Hash HbSync)
extractMetadataHash what blob =
  case tryDetect what blob of
     MerkleAnn (MTreeAnn {_mtaMeta = AnnHashRef h, _mtaCrypt = NullEncryption}) -> Just h
     _ -> Nothing

orElse :: m r -> Maybe a -> ContT r m a
orElse a mb = ContT $ maybe1 mb a

httpWorker :: forall e s m . ( MyPeer e
                             , MonadIO m
                             , HasStorage m
                             , IsRefPubKey s
                             , s ~ Encryption e
                             , m ~ PeerM e IO
                             , e ~ L4Proto
                             -- , ForLWWRefProto e
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

      get "/ref/:key" do

        void $ flip runContT pure do
          what <- lift (param @String "key" <&> fromStringMay @(LWWRefKey HBS2Basic))
                    >>= orElse (status status404)

          rv <- getRef sto what
                  >>= orElse (status status404)
                  >>= getBlock sto
                  >>= orElse (status status404)
                  <&> either (const Nothing) Just . deserialiseOrFail @(SignedBox (LWWRef e) e)
                  >>= orElse (status status404)
                  <&> unboxSignedBox0 @(LWWRef e)
                  >>= orElse (status status404)
                  <&> lwwValue . snd

          lift $ redirect [qc|/tree/{pretty rv}|]

      get "/tree/:hash" do
        what <- param @String "hash" <&> fromString

        void $ flip runContT pure do

          callCC $ \exit -> do

            blob <- liftIO (getBlock sto what)
                      >>= orElse (status status404)

            mh <- orElse (status status404) (extractMetadataHash what blob)

            meta <- lift (getBlock sto mh) >>= orElse (status status404)
                       <&> LBS8.unpack
                       <&> fromRight mempty . parseTop

            let tp = headDef "application/octet-stream"
                     [ show (pretty w)
                     | ListVal [SymbolVal "mime-type:", LitStrVal w] <- meta
                     ]

            let fn = headMay
                     [ show (pretty w)
                     | ListVal [SymbolVal "file-name:", LitStrVal w] <- meta
                     ]

            -- liftIO $ print $ pretty meta

            case fn of
              Just x | takeExtension x == ".html" -> pure  ()
                     | otherwise -> lift $ do
                         addHeader "content-disposition" [qc|attachment; filename="{x}"|]

              _ -> pure ()

            lift $ addHeader "content-type" (fromString tp)

            elbs <- lift $ runExceptT $ readFromMerkle sto (SimpleKey what)

            case elbs of
              Left{} -> lift $ status status404
              Right lbs -> lift do
                stream $ \write flush -> do
                  for_ (LBS.toChunks lbs) $ \chunk -> do
                    write $ byteString chunk
                  flush

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

