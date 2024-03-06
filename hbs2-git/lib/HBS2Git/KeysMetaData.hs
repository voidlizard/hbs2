module HBS2Git.KeysMetaData where


import HBS2Git.Prelude
import HBS2Git.Types
import HBS2Git.Alerts
import HBS2Git.Annotations
import HBS2Git.Encryption
import HBS2Git.State
import HBS2Git.PrettyStuff
import HBS2Git.Config


import HBS2.Data.Detect
import HBS2.Merkle
import HBS2.Peer.Proto
import HBS2.OrDie
import HBS2.Storage
import HBS2.Storage.Operations.ByteString
import HBS2.System.Logger.Simple

import Control.Monad
import Control.Monad.Catch (MonadMask)
import Control.Monad.Except (runExceptT)
import Control.Monad.Trans.Maybe
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Either
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.List qualified as List
import Data.Maybe
import Lens.Micro.Platform
import Streaming.Prelude qualified as S
import System.IO
import Text.InterpolatedString.Perl6 (qc)


updateGK0 :: forall m . ( MonadIO m
                        -- , HasRPC m
                        , MonadMask m
                        , HasStorage m
                        , HasConf m
                        , HasEncryptionKeys m
                        )
                 => RepoRef
                 -> m ()
updateGK0 repo = void $ runMaybeT do

  guard =<< lift (isRefEncrypted (fromRefLogKey repo))

  db <- makeDbPath repo >>= dbEnv
  -- FIXME: check-if-for-die-good-here
  ki <- lift $ getKeyInfo (fromRefLogKey repo)
          `orDie` noKeyInfoMsg repo

  -- 2. Если нет GK0 или он expired
  mbGk0Hash <- withDB db $ stateGetLocalKey ki

  -- 2.1 Генерируем новый GK0
  gk0Hash <- lift $ maybe1 mbGk0Hash (makeNewGK0 ki) pure

  when (isNothing mbGk0Hash) do
    liftIO $ hPutDoc stderr $ "New GK0" <+> pretty gk0Hash <> line

  withDB db $ statePutLocalKey ki gk0Hash repo

  debug  $ "GK0" <+> pretty gk0Hash

  where
    makeNewGK0 ki = do
      sto <- getStorage
      gk <- genGK0 ki <&> serialise
      liftIO $ writeAsMerkle sto (gk :: ByteString) <&> HashRef

genKeysAnnotations :: forall m . ( MonadIO m
                                 , MonadMask m
                                 , HasStorage m
                                 , HasConf m
                                 , HasEncryptionKeys m
                                 )
                 => RepoRef
                 -> m (Maybe HashRef)

genKeysAnnotations repo = do
  sto <- getStorage

  runMaybeT do

    guard =<< lift (isRefEncrypted (fromRefLogKey repo))

    db <- makeDbPath repo >>= dbEnv
    -- TODO: generate-and-update-keys-metadata
    -- 1. get GK0

    ki <- lift $ getKeyInfo (fromRefLogKey repo)
            `orDie` noKeyInfoMsg repo

    gk0Hash <- withDB db $ stateGetLocalKey ki
                `orDie` noKeyInfoMsg repo

    let processedKey = serialise ("GENKEYMETADATA", gk0Hash)

    isNewKey  <- withDB db $ not <$> stateGetProcessed processedKey

    sp0 <- withDB db savepointNew
    withDB db $ savepointBegin sp0

    -- FIXME: excess-data-roundtrip
    gk0newBs <- (runExceptT (readFromMerkle sto (SimpleKey (fromHashRef gk0Hash))))
                  `orDie` [qc|*** Can't load GK0 {pretty gk0Hash}, maybe storage failure|]

    -- теперь нам надо как-то узнать, что ключ новый и нам надо обработать
    -- новых читателей.
    -- Вариант #1: писать авторов в стейт. если они не обработаны еще,
    -- то обрабатывать.

    -- 2.2 Генерируем новый GK1 ∀ members
    -- FIXME: might-be-slow

    guard isNewKey

    -- notice $ "NEW KEY APPEARED" <+> pretty gk0Hash

    h <- toMPlus =<< getRef sto (refAlias repo)

    gk0hs <- HashSet.fromList <$> S.toList_ (findAllGK0 sto h)

    let keySource = do
          forM_ gk0hs $ \gkh -> void $ runMaybeT do
            gbs <- toMPlus =<< runExceptT (readFromMerkle sto (SimpleKey gkh))
            gk0 <- toMPlus $ deserialiseOrFail @(GroupKey 'Symm HBS2Basic) gbs
            -- TODO: decrypt-secret-right-here
            lift $ S.yield (gkh, gk0)

    allKeys <- S.toList_ keySource <&> HashMap.fromList

    -- ∀ gk0:
    --    - вытащить секрет (найти, кем расшифровать) recipients
    --    - взять вообще всех recipients и сформировать новый GK1
    --      для каждого из recipients из allKeys

    -- взять все доступные пары ключей?
    keys <- lift enumEncryptionKeys <&> fmap (\x -> (view krPk x, view krSk x))

    new' <- forM (HashMap.toList allKeys) $ \(hx, gk0) -> do
      let gksec' = [ lookupGroupKey sk pk gk0 | (pk,sk) <- keys ] & catMaybes & headMay
      case gksec' of
        Nothing  -> pure (Left hx)
        Just sec -> pure $ Right (hx, gk0, sec)

    let missed = lefts new'

    forM_ missed $ \miss -> do
      warn $ "new group key: unavailable keys for gk" <+> pretty miss

    let new = rights new'

    gk0new <- pure (deserialiseOrFail @(GroupKey 'Symm HBS2Basic) gk0newBs)
                `orDie` [qc|*** Malformed/corrupted group key {pretty gk0Hash}|]

    let rcpt0 = recipients gk0new

    gnew <- forM new $ \(hx, gk0, sec) -> do

      -- TODO: test-if-key-removing-works
      let newRcpt = (recipients gk0new & HashMap.keysSet)
                     `HashSet.difference`
                    (recipients gk0 & HashMap.keysSet)

      let r1 = HashMap.keys $ recipients gk0 <> recipients gk0new

      let r11 = [ x | x <- r1, HashMap.member x rcpt0 ]

      gk1 <- generateGroupKey @HBS2Basic (Just sec) r11

      pure (hx, newRcpt, gk1)

    let nr = HashSet.unions $ fmap (view _2) gnew

    ann <- if HashSet.null nr then do
             pure mempty
           else do
            forM gnew $ \(gk0h, _, gk1) -> do
              pure (GK1 (HashRef gk0h) gk1)


    annHash <- if List.null ann then do
                 pure Nothing
               else do
                 Just . HashRef <$> writeAsMerkle sto (serialise (SmallAnnotations ann))

    debug $ "ANNOTATIONS" <+> pretty annHash

    withDB db do
      statePutProcessed processedKey
      savepointRelease sp0

    toMPlus annHash

  where

    -- FIXME: deepScan-ScanShallow-broken
    -- TODO: deal-with-missed-blocks
    findAllGK0 sto h = do
        -- TODO: performance-memoize-possible
        --   можно мемоизировать для h
        deepScan ScanDeep (const none) h (getBlock sto) $ \hx -> do
          void $ runMaybeT do
            blk  <- toMPlus =<< getBlock sto hx
            refupd <- toMPlus $ deserialiseOrFail @(RefLogUpdate HBS2L4Proto) blk
            payload <- toMPlus $ deserialiseOrFail (LBS.fromStrict $ view refLogUpdData refupd)

            let (SequentialRef _ (AnnotatedHashRef _ ht)) = payload

            treeBs <- toMPlus =<< getBlock sto (fromHashRef ht)

            enc <- toMPlus (deserialiseOrFail @(MTreeAnn [HashRef]) treeBs)
                       <&> _mtaCrypt

            case enc of
              EncryptGroupNaClSymm g _  -> do
                -- liftIO $ hPutDoc stderr $ "GK0 FOR" <+> pretty
                lift $ S.yield g

              _ -> pure ()


importKeysAnnotations :: forall m . ( MonadIO m
                                    , MonadMask m
                                    , HasStorage m
                                    )
                 => RepoRef
                 -> HashRef
                 -> HashRef
                 -> DB m ()

importKeysAnnotations repo e href = do
  sto <- lift getStorage
  void $ runMaybeT do
    ebs <- runExceptT $ readFromMerkle sto (SimpleKey (fromHashRef href))

    bs <- toMPlus ebs

    anns <- toMPlus $ deserialiseOrFail @Annotations bs

    let entries = case anns of
                    SmallAnnotations e -> [ gk1 | gk1@(GK1{}) <- e ]
                    _                  -> mempty


    forM_ entries $ \(GK1 gk0h gk1) -> do

      forM_ (HashMap.toList (recipients gk1)) $ \(pk,box) -> do
        let gk1small = GroupKeySymm @HBS2Basic (HashMap.singleton pk box)
        lift $ statePutGK1 gk0h pk gk1small


