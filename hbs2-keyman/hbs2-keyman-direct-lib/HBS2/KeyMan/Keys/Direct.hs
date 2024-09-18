module HBS2.KeyMan.Keys.Direct where

import HBS2.KeyMan.App.Types
import HBS2.KeyMan.Prelude
import HBS2.KeyMan.State
import HBS2.KeyMan.Config

import HBS2.Storage
import HBS2.Data.Types.Refs
import HBS2.Net.Auth.Credentials
import HBS2.Net.Auth.GroupKeySymm as Symm


import HBS2.System.Dir

import Control.Monad.Cont
import DBPipe.SQLite
import Text.InterpolatedString.Perl6 (qc)
import Data.Maybe
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Control.Monad.Trans.Maybe
import Data.List qualified as List
import Data.ByteString qualified as BS
import Data.Ord
import Data.Coerce
import Streaming.Prelude qualified as S

data KeyManClientError = KeyManClientSomeError

newtype KeyManClientEnv = KeyManClientEnv AppEnv

newtype KeyManClient m a = KeyManClient { fromKeyManClient :: DBPipeM m a }
                           deriving newtype ( Applicative
                                            , Functor
                                            , Monad
                                            , MonadIO
                                            , MonadUnliftIO
                                            )

newKeymanClientEnv :: MonadUnliftIO m => m KeyManClientEnv
newKeymanClientEnv = KeyManClientEnv <$> liftIO newAppEnv

withKeymanClientRO :: MonadUnliftIO m => KeyManClientEnv -> KeyManClient m a -> m a
withKeymanClientRO env action = do
  let db = appDb (coerce env)
  withDB db (fromKeyManClient action)

runKeymanClientRO :: MonadUnliftIO m => KeyManClient m a -> m a
runKeymanClientRO action = do
  env <- newKeymanClientEnv
  withKeymanClientRO env action

runKeymanClient :: MonadUnliftIO m => KeyManClient m a -> m a
runKeymanClient action = do
  KeyManClientEnv env <- newKeymanClientEnv
  -- FIXME: dbpath-to-appstatenv
  --  сейчас dbPath берётся из конфига, а db из стейта
  --  и хотя они должны быть одинаковы, это не гарантируется
  dbPath <- getStatePath

  let db = appDb env

  here <- doesPathExist dbPath

  unless here do
    withDB db $ populateState

  flip runContT pure $ do
    void $ ContT $ bracket (async (runPipe db)) cancel
    lift $ withDB db (fromKeyManClient action)

listCredentials :: forall  m .
                     ( MonadIO m
                     , SerialisedCredentials 'HBS2Basic
                     )
                  => KeyManClient m [PubKey 'Sign 'HBS2Basic]
listCredentials = KeyManClient do
  select_ [qc|
    select f.key
    from keytype t
            join keyfile f on t.key = f.key
       left join keyweight w on w.key = f.key
    where t.type = 'sign'
    order by w.weight desc nulls last
    limit 100 |]
      <&> mapMaybe ( fromStringMay . fromOnly )

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
    r <- S.toList_ do
          forM_ (HM.toList $ recipients gk) $ \(pk,box) -> runMaybeT do
            (KeyringEntry ppk ssk _) <- MaybeT $ lift $ loadKeyRingEntry pk
            let s = Symm.lookupGroupKey @'HBS2Basic ssk ppk gk
            for_ s $ lift . S.yield

    pure $ headMay r

trackGK :: forall s m . (MonadIO m, s ~ HBS2Basic)
         => AnyStorage
        -> HashRef
        -> m ()
trackGK sto href = do
  -- gk <- loadGroupKeyMaybe @s sto href
  pure ()

type TrackGroupKeyView = ( SomeHash GroupKeyId
                         , SomeHash HashRef
                         , String
                         , FilePath
                         , Int)

findMatchedGroupKeySecret :: forall s m . ( MonadIO m
                                          , SerialisedCredentials 'HBS2Basic
                                          , s ~ 'HBS2Basic
                                          )
                        => AnyStorage
                        -> GroupKey 'Symm s
                        -> KeyManClient m (Maybe GroupSecret)

findMatchedGroupKeySecret sto gk = do

  let sql = [qc|
      select t.secret
           , t.gkhash
           , f.key
           , f.file
           , coalesce(kw.weight, 0) as weight
      from  gkaccess gka
            join gktrack t on gka.gkhash = t.gkhash
            join keyfile f on f.key = gka.key
            left join keyweight kw on kw.key = f.key
      where t.secret = ?
      order by kw.weight desc nulls last
          |]

  let pks = recipients gk & HM.keysSet

  flip runContT pure $ callCC $ \exit -> do

    kre0 <- lift $ loadKeyRingEntries (HS.toList pks) <&> fmap snd

    sec0 <- findSecretDefault kre0 gk

    -- возвращаем первый, который нашли
    maybe1 sec0 none (exit . Just)

    -- если старый формат ключа -- то ничего не найдём
    secId <- ContT $ maybe1 (getGroupKeyId gk) (pure Nothing)

    rows <- lift $ KeyManClient $ select @TrackGroupKeyView sql (Only (SomeHash secId))

    let gkss = HS.fromList (fmap (coerce @_ @HashRef . view _2) rows) & HS.toList

    -- TODO: memoize

    -- ищем такой же
    --   если нашли -- хорошо бы проверить пруф, но как?
    --   для исходного ключа -- мы оказались здесь потому,
    --   что не смогли достать секрет из него и ищем такой же,
    --   но доступный нам. соответственно, мы не можем убедиться,
    --   что исходный ключ с правильным Id / правильным секретом.
    --   можем только обломаться при расшифровке и записать этот факт
    for_ gkss $ \gkh -> void $ runMaybeT do
      gkx <- loadGroupKeyMaybe @s sto gkh >>= toMPlus
      sec' <- lift $ lift $ extractGroupKeySecret gkx
      maybe1 sec' none $ (lift . exit . Just)

    pure Nothing



