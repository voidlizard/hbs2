{-# Language AllowAmbiguousTypes #-}
{-# Language TypeOperators #-}
{-# Language ViewPatterns #-}
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
import HBS2.Net.Auth.Credentials
import HBS2.Data.Types.SignedBox
import HBS2.Events
import HBS2.Storage.Operations.ByteString
import HBS2.Misc.PrettyStuff

import PeerTypes
import PeerConfig
import RefLog ( doRefLogBroadCast )

import Data.Config.Suckless

import Data.Maybe
import Data.ByteString.Lazy qualified as LBS
import Network.HTTP.Types.Status
import Network.Wai.Middleware.RequestLogger
import Text.InterpolatedString.Perl6 (qc)

import Web.Scotty
import Web.Scotty.Trans (ActionT)

import Data.ByteString.Builder (byteString, Builder)

import Control.Concurrent
import Data.Either
import Codec.Serialise (deserialiseOrFail)
import Data.Aeson (object, (.=))
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LT
import Data.HashMap.Strict qualified as HM
import Control.Monad.Reader
import Lens.Micro.Platform (view)
import System.FilePath
import Control.Monad.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Cont
import Data.Coerce

import UnliftIO (async)

{- HLINT ignore "Functor law" -}

-- TODO: introduce-http-of-off-feature

extractMetadataHash :: MonadIO  m
                    => AnyStorage
                    -> HashRef
                    -> m (Maybe [Syntax C])

extractMetadataHash sto what = runMaybeT do

  blob <- getBlock sto (coerce what)
            >>= toMPlus

  case tryDetect (coerce what) blob of
     MerkleAnn (MTreeAnn {_mtaMeta = AnnHashRef h, _mtaCrypt = NullEncryption}) -> do

        getBlock sto h
          >>= toMPlus
          <&> LBS8.unpack
          <&> fromRight mempty . parseTop


     MerkleAnn (MTreeAnn {_mtaMeta = ShortMetadata txt, _mtaCrypt = NullEncryption}) -> do
       parseTop (Text.unpack txt) & toMPlus

     _ -> mzero

orElse :: m r -> Maybe a -> ContT r m a
orElse a mb = ContT $ maybe1 mb a

lookupLWWRef :: forall e s m . (s ~ Encryption e, IsRefPubKey s, ForSignedBox s, s ~ HBS2Basic)
           => AnyStorage
           -> LWWRefKey s
           -> ContT () ActionM HashRef

lookupLWWRef sto what =
  getRef sto what
      >>= orElse (status status404)
      >>= getBlock sto
      >>= orElse (status status404)
      <&> either (const Nothing) Just . deserialiseOrFail @(SignedBox (LWWRef s) s)
      >>= orElse (status status404)
      <&> unboxSignedBox0 @(LWWRef s)
      >>= orElse (status status404)
      <&> lwwValue . snd


httpWorker :: forall e s m . ( MyPeer e
                             , MonadIO m
                             , HasStorage m
                             , IsRefPubKey s
                             , s ~ Encryption e
                             , s ~ HBS2Basic
                             , m ~ PeerM e IO
                             , e ~ L4Proto
                             -- , ForLWWRefProto e
                             ) => PeerConfig -> AnnMetaData -> m ()

httpWorker (PeerConfig syn) pmeta = do

  sto <- getStorage
  let port' = runReader (cfgValue @PeerHttpPortKey) syn  <&>  fromIntegral
  penv <- ask

  maybe1 port' none $ \port -> liftIO do

    scotty port $ do
      middleware logStdout

      -- defaultHandler do
      --   status status500
      --
      --
      let handleRef ( p :: Maybe Text ) = \case

             Right ref  -> do

              what <- fromStringMay ref
                        & orElse (status status404)

              lift do
                getTreeHash @e sto p what

             Left ( ref  :: String ) -> do

                what  <- fromStringMay @(LWWRefKey s) ref
                           & orElse (status status404)

                rv <- getRef sto what
                        >>= orElse (status status404)
                        >>= getBlock sto
                        >>= orElse (status status404)
                        <&> either (const Nothing) Just . deserialiseOrFail @(SignedBox (LWWRef s) s)
                        >>= orElse (status status404)
                        <&> unboxSignedBox0 @(LWWRef s)
                        >>= orElse (status status404)
                        <&> lwwValue . snd

                lift do
                  getTreeHash @e sto p rv

      get "/size/:hash" do

        void $ flip runContT pure do
          what <- lift (pathParam @String "hash")
                    <&> fromStringMay
                    >>= orElse (status status404)

          size <- liftIO $ hasBlock sto what
          case size of
            Nothing -> lift $ status status404
            Just n -> do
              lift $ json n

      -- TODO: key-to-disable-tree-streaming

      get "/ref/:key" do
        void $ flip runContT pure do
          ref <- lift (pathParam @String "key")
          lift $ addHeader "Cache-Control" "public, must-revalidate, max-age=0"
          handleRef Nothing (Left ref)

      get "/ref/:key/:part" do
        void $ flip runContT pure do
          ref  <- lift (pathParam @String "key")
          part <- lift (pathParam @Text "part")
          lift $ addHeader "Cache-Control" "public, must-revalidate, max-age=0"
          handleRef (Just part) (Left ref)

      -- TODO: define-parsable-instance-for-our-types
      get "/tree/:hash" do
        void $ flip runContT pure do
          ref <- lift (pathParam @String "hash")
          lift $ addHeader "Cache-Control" "public, max-age=31536000, immutable"
          handleRef Nothing (Right ref)

      get "/tree/:hash/:part" do
        void $ flip runContT pure do
          ref <- lift (pathParam @String "hash")
          part <- lift (pathParam @Text "part")
          lift $ addHeader "Cache-Control" "public, must-revalidate, max-age=0"
          handleRef (Just part) (Right ref)

      get "/cat/:hash" do
        void $ flip runContT pure do
          what <- lift (pathParam @String "hash")
                    <&> fromStringMay
                    >>= orElse (status status404)
          lift do
            blob <- liftIO $ getBlock sto what
            case blob of
              Nothing -> status status404
              Just lbs -> do
                setHeader "Cache-Control" "public, max-age=31536000, immutable"
                setHeader "ETag" (LT.pack $ show $ pretty what)
                raw lbs

      get "/reflog/:ref" do
        re <- pathParam @String "ref" <&> fromStringMay
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

-- pattern WebRef :: forall {s} . sEither (L

data WebRefAction s =
         RefTree HashRef
       | RefRef (LWWRefKey s)
       | RefRedirect Text


webRef :: forall c s . (IsContext c, s ~ HBS2Basic) => Syntax c -> Maybe (Text, WebRefAction s)
webRef = \case
  ListVal [TextLike "web:bind", TextLike name, TextLike "tree", HashLike h] -> Just (name,  RefTree h)
  ListVal [TextLike "web:bind", TextLike name, TextLike "ref",  SignPubKeyLike k] -> Just (name, RefRef (LWWRefKey k))
  ListVal [TextLike "web:bind", TextLike name, TextLike "redirect",  TextLike re] -> Just (name, RefRedirect re)
  _ -> Nothing

noWebRoot :: [Syntax c] -> [Syntax c]
noWebRoot syn = flip filter syn \case
  ListVal (TextLike "web:root" : _) -> False
  ListVal (TextLike "webroot" : _) -> False
  _ -> True

getTreeHash :: forall e s . (s ~ Encryption e, ForSignedBox s, IsRefPubKey s, s ~ HBS2Basic)
            => AnyStorage -> Maybe Text -> HashRef -> ActionM ()

getTreeHash sto part what'' = void $ flip runContT pure do

  callCC \exit -> do

    flip fix (mempty, what'', 0) $ \again (p, what',i) -> do

      let what = fromHashRef what'

      meta' <- extractMetadataHash sto what'
                >>= orElse (status status404)

      let meta = p <> meta'

      debug $ red "META/0" <+> pretty meta <+> line

      let tp = headDef "application/octet-stream"
               [ show (pretty w)
               | ListVal [SymbolVal "mime-type:", LitStrVal w] <- meta
               ]

      let fn = headMay
               [ show (pretty w)
               | ListVal [SymbolVal "file-name:", LitStrVal w] <- meta
               ]

      let ce = headMay
               [ show (pretty w)
               | ListVal [SymbolVal "content-encoding:", StringLike w] <- meta
               ]


      let re = headMay
               [ show (pretty w)
               | ListVal [SymbolVal "web:redirect", StringLike w] <- meta
               ]

      for_ re $ \l -> do
        lift $ redirect (fromString l)
        exit ()

      let parts = (Nothing, RefTree what') : [ (Just name, w)
                  | ( webRef @C -> Just (name, w) ) <- meta
                  ] & HM.fromList

      for_ ce $ \c ->
        lift $ addHeader "Content-Encoding" (fromString c)

      let webroot = headMay [ w
                            | i < 2
                            , ListVal [SymbolVal r, HashLike w] <- meta
                            , r == "webroot" || r == "web:root"
                            ]

      case webroot of
        Just x | i < 2 -> again (noWebRoot meta, x, succ i)

        _ -> do

          for_ webroot $ \w -> do
            warn $ green "HTTP:WEBROOT" <+> pretty w

          -- liftIO $ print $ pretty meta

          case fn of
            Just x | takeExtension x == ".html" -> pure  ()
                   | otherwise -> lift $ do
                       addHeader "content-disposition" [qc|attachment; filename="{x}"|]

            _ -> pure ()

          lift $ addHeader "content-type" (fromString tp)

          debug $ red "META" <+> pretty meta <+> line <+> pretty (HM.keys parts)

          key <- case HM.lookup part parts of
                   Just (RefTree key) -> do
                    pure key

                   Just (RefRef lww)  -> lookupLWWRef @e sto lww

                   Just (RefRedirect s)  -> do
                      lift $ redirect (LT.fromStrict s)
                      exit ()

                   _ -> pure (HashRef what)

          lift $ addHeader "ETag" (LT.pack $ show $ pretty key)
          elbs <- lift $ runExceptT $ readFromMerkle sto (SimpleKey (coerce key))

          case elbs of
            Left{} -> lift $ status status404
            Right lbs -> lift do
              stream $ \write flush -> do
                for_ (LBS.toChunks lbs) $ \chunk -> do
                  write $ byteString chunk
                flush


