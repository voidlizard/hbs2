{-# Language AllowAmbiguousTypes #-}
module HBS2.CLI.NCQ3.Migrate where

import HBS2.CLI.Prelude
import HBS2.CLI.Run.Internal

import HBS2.Peer.NCQ3.Migrate.NCQ

import HBS2.Net.Auth.Schema()
import HBS2.Peer.RPC.API.Peer
import HBS2.Peer.RPC.Client
import HBS2.Peer.RPC.Client.Unix
import HBS2.Storage
import HBS2.Storage.NCQ3.Internal.Prelude
import HBS2.Peer.Proto.RefLog
import HBS2.Peer.Proto.RefChan
import HBS2.Peer.Proto.LWWRef

import Streaming.Prelude qualified as S

migrateEntries :: forall c m . ( MonadUnliftIO m
                               , IsContext c
                               , Exception (BadFormException c)
                               , HasClientAPI PeerAPI UNIX m
                               , HasStorage m
                               ) => MakeDictM c m ()
migrateEntries = do
  brief "migrate NCQv1 => NCQ3"
   $ args [ arg "path" "src"
          , arg "path" "dst"
          ]

   $ entry $ bindMatch "ncq3:migrate:ncq" $ nil_ $ \case
       [ StringLike src, StringLike dst] -> do

        api <- getClientAPI @PeerAPI  @UNIX

        refs <- callRpcWaitMay @RpcPollList2 (1.0 :: Timeout 'Seconds) api (Nothing, Nothing)
                  <&> fromMaybe mempty

        rrefs <- S.toList_ <$> for refs $ \(pk, s, _) -> case s of
                   "reflog"  -> S.yield (WrapRef $ RefLogKey @'HBS2Basic pk)

                   "refchan" -> do
                      S.yield (WrapRef $ RefChanLogKey @'HBS2Basic pk)
                      S.yield (WrapRef $ RefChanHeadKey @'HBS2Basic pk)

                   "lwwref"  -> S.yield (WrapRef $ LWWRefKey @'HBS2Basic pk)
                   _         -> none

        lift $ migrateNCQ1 nicelog rrefs src dst

       e -> throwIO $ BadFormException (mkList e)

nicelog :: forall m . MonadIO  m => Doc AnsiStyle -> m ()
nicelog doc = liftIO $ hPutDoc stdout (doc <> line)

