module HBS2.KeyMan.Keys.Direct where

import HBS2.Prelude.Plated
import HBS2.Net.Auth.Credentials
import HBS2.Net.Proto.Types

import HBS2.KeyMan.App.Types
import HBS2.KeyMan.State

import Control.Monad.Cont
import Control.Monad.Reader
import Control.Monad.Except
import UnliftIO
import DBPipe.SQLite
import Text.InterpolatedString.Perl6 (qc)
import Data.Maybe
import Control.Monad.Trans.Maybe
import Data.ByteString qualified as BS

data KeyManClientError = KeyManClientSomeError

newtype KeyManClient m a = KeyManClient { fromKeyManClient :: DBPipeM m a }
                           deriving newtype ( Applicative
                                            , Functor
                                            , Monad
                                            , MonadIO
                                            , MonadUnliftIO
                                            )

runKeymanClient :: MonadUnliftIO m => KeyManClient m a -> m a
runKeymanClient action = do
  env <- liftIO newAppEnv
  let db = appDb env
  flip runContT pure $ do
    void $ ContT $ bracket (async (runPipe db)) cancel
    lift $ withDB db (fromKeyManClient action)


loadCredentials :: forall a m .
                     ( MonadIO m
                     , SomePubKeyPerks a
                     )
                  => a
                  -> KeyManClient m (Maybe (PeerCredentials HBS2Basic))
loadCredentials k = KeyManClient do

  fnames <- select @(Only FilePath) [qc|
    select f.file
    from keytype t join keyfile f on t.key = f.key
    where t.key = ? and t.type = 'sign'
    limit 1 |]  (Only (SomePubKey k))

  runMaybeT do
    fn <- toMPlus $ fmap fromOnly fnames & listToMaybe
    -- FIXME: throwError?
    bs <- liftIO (try @_ @IOException $ BS.readFile fn) >>= toMPlus
    toMPlus $ parseCredentials (AsCredFile bs)

loadKeyRingEntry :: forall m .
                    ( MonadIO m
                    )
                 => PubKey 'Encrypt HBS2Basic
                 -> KeyManClient m (Maybe (KeyringEntry HBS2Basic))
loadKeyRingEntry pk = KeyManClient do
  runMaybeT do
    fn <- toMPlus =<< lift (selectKeyFile pk)
    bs <- liftIO (try @_ @IOException $ BS.readFile fn) >>= toMPlus
    creds <- toMPlus $ parseCredentials (AsCredFile bs)
    toMPlus $ headMay [ e
                      | e@(KeyringEntry p _ _) <- view peerKeyring creds
                      , p == pk
                      ]

