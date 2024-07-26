module HBS2.KeyMan.Keys.Direct where

import HBS2.KeyMan.App.Types
import HBS2.KeyMan.Prelude
import HBS2.KeyMan.State
import HBS2.KeyMan.Config

import HBS2.Prelude.Plated
import HBS2.Net.Auth.Credentials
import HBS2.Net.Auth.GroupKeySymm as Symm
import HBS2.Net.Proto.Types


import HBS2.System.Dir

import Control.Monad.Cont
import UnliftIO
import DBPipe.SQLite
import Text.InterpolatedString.Perl6 (qc)
import Data.Maybe
import Data.HashMap.Strict qualified as HM
import Control.Monad.Trans.Maybe
import Data.List qualified as List
import Data.ByteString qualified as BS
import Data.Ord

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
  dbPath <- getStatePath
  env <- liftIO newAppEnv
  let db = appDb env
  flip runContT pure $ do
    void $ ContT $ bracket (async (runPipe db)) cancel

    here <- doesPathExist dbPath

    unless here do
      withDB db $ populateState

    lift $ withDB db (fromKeyManClient action)


loadCredentials :: forall a m .
                     ( MonadIO m
                     , SomePubKeyPerks a
                     , SerialisedCredentials 'HBS2Basic
                     )
                  => a
                  -> KeyManClient m (Maybe (PeerCredentials 'HBS2Basic))
loadCredentials k = KeyManClient do

  fnames <- select @(Only FilePath) [qc|
    select f.file
    from keytype t
            join keyfile f on t.key = f.key
       left join keyweight w on w.key = f.key
    where t.key = ? and t.type = 'sign'
    order by w.weight desc nulls last
    limit 1 |]  (Only (SomePubKey k))

  runMaybeT do
    fn <- toMPlus $ fmap fromOnly fnames & listToMaybe
    -- FIXME: throwError?
    bs <- liftIO (try @_ @IOException $ BS.readFile fn) >>= toMPlus
    toMPlus $ parseCredentials (AsCredFile bs)

loadKeyRingEntry :: forall m .
                    ( MonadIO m
                    , SerialisedCredentials 'HBS2Basic
                    )
                 => PubKey 'Encrypt 'HBS2Basic
                 -> KeyManClient m (Maybe (KeyringEntry 'HBS2Basic))
loadKeyRingEntry pk = KeyManClient do
  runMaybeT do
    fn <- toMPlus =<< lift (selectKeyFile pk)
    bs <- liftIO (try @_ @IOException $ BS.readFile fn) >>= toMPlus
    creds <- toMPlus $ parseCredentials (AsCredFile bs)
    toMPlus $ headMay [ e
                      | e@(KeyringEntry p _ _) <- view peerKeyring creds
                      , p == pk
                      ]

loadKeyRingEntries :: forall m .
                    ( MonadIO m
                    , SerialisedCredentials 'HBS2Basic
                    )
                 => [PubKey 'Encrypt 'HBS2Basic]
                 -> KeyManClient m [(Word, KeyringEntry 'HBS2Basic)]
loadKeyRingEntries pks = KeyManClient do
    r <- for pks $ \pk -> runMaybeT do
              fn <- lift (selectKeyFile pk) >>= toMPlus
              w  <- lift (selectKeyWeight pk)
              bs <- liftIO (try @_ @IOException $ BS.readFile fn) >>= toMPlus
              creds <- toMPlus $ parseCredentials (AsCredFile bs)
              toMPlus $ headMay [ (w,e)
                                | e@(KeyringEntry p _ _) <- view peerKeyring creds
                                , p == pk
                                ]
    pure $ catMaybes r & List.sortOn (Down . fst)


extractGroupKeySecret :: MonadIO m
                      => GroupKey 'Symm 'HBS2Basic
                      -> KeyManClient m (Maybe GroupSecret)
extractGroupKeySecret gk = do
  runMaybeT do
    s <- forM (HM.toList $ recipients gk) $ \(pk,box) -> do
      KeyringEntry pk sk _ <- MaybeT $ loadKeyRingEntry pk
      MaybeT $ pure (Symm.lookupGroupKey sk pk gk)
    MaybeT $ pure $ headMay s

